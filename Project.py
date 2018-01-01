import string
import json
import copy
import os
import subprocess
from random import randint
from datetime import datetime
import requests as req
from Configuration import conf
from xml.etree import ElementTree

class Project:
    """describes a repository"""

    # members
    base_headers = {"Authorization": "token %s" % conf['github_api']['token']}

    def __init__(self, openhub_name, owner, name):
        self.project_github_api_url = "%s/%s/%s" % (conf['github_api']['base_url'],owner, name)
        self.repo_url = "https://github.com/%s/%s.git" % (owner, name)
        self.owner = owner
        self.name = name
        self.issues = {}
        self.contributors = {}
        self.languages = {}
        self.openhub = {'query_name': openhub_name }
        self.created_at = self.set_create_date()
    
    def set_create_date(self):
        res = "N/A"
        req_r = req.get("%s" % (self.project_github_api_url), headers = self.base_headers)
        r_j = req_r.json()
        if "created_at" in r_j:
           res = r_j["created_at"]
        else:
            print("COULD NOT GET CREATED DATE")
        return res

    def set_language_data(self):
        res = req.get("%s/languages" % (self.project_github_api_url), headers = self.base_headers)
        r_j = res.json()
        self.languages = r_j


    def set_contrib_data(self):
        res = req.get("%s/stats/contributors" % (self.project_github_api_url), headers = self.base_headers)
        r_j = res.json()
        contrib = []
        for cont in r_j:
            contrib.append( cont["total"] )
        self.contributors["total_commits"] = sum(contrib)
        self.contributors["count"] = len(contrib)
        self.contributors["list"] = contrib
        contrib_p = []
        for  i in range(0,len(contrib)):
            contrib_p.append(int(contrib[i]/sum(contrib)*100))
        contrib_p.sort(reverse=True)
        BF = 0
        temp = 0
        index = 0
        while (temp < 75 and index < len(contrib_p)):
           BF += 1
           temp += contrib_p[index]
           index += 1
        self.contributors["bus_factor"] = BF
        self.contributors["list_p"] = contrib_p

    def set_issue_data(self):
        res = req.get("%s/issues?state=closed" % (self.project_github_api_url), headers = self.base_headers)
        if "last" in res.links:
            self.issues["pages"] = int(res.links["last"]["url"].split("=")[-1])
        else:
            self.issues["pages"] = 1
        self.issues["total_count"] = 0
        self.issues["avg_closed_time"] = 0
        isum = 0
        for i in range(1, self.issues["pages"]+1):
            r = req.get("%s/issues?state=closed&page=%i" % (self.project_github_api_url, i), headers = self.base_headers)
            r_j = r.json()
            if  len(r_j) > 0: 
                for issue in r_j:
                    self.issues["total_count"] += 1
                    opened = datetime.strptime(issue["created_at"], "%Y-%m-%dT%H:%M:%SZ")
                    closed = datetime.strptime(issue["closed_at"], "%Y-%m-%dT%H:%M:%SZ")
                    delta = closed - opened
                    isum += delta.days
                self.issues["avg_closed_time"] = int(isum / self.issues["total_count"])
    
    def set_openhub_data(self):
        self.openhub['base_params'] = {'api_key': conf['openhub_api']['api_key'] }
        self.openhub['stats'] = {}
        openhub_id = self.get_openhub_id()
        if openhub_id != -1:
            self.openhub['id'] = openhub_id
            self.openhub['api_url'] = "%s/%s" % (conf['openhub_api']['base_url'], self.openhub['id'])
            self.openhub['stats'] = self.set_openhub_stats()
        else:
            raise Exception("     ERROR - Can't get openhub id for this project")

    def get_openhub_id(self):
        found = False
        options = [self.openhub['query_name'],"%s/%s" % (self.owner, self.name), self.name]
        while not found and len(options) > 0:            
            params = copy.copy(self.openhub['base_params'])
            q = options[0]
            options.pop(0)
            params['query'] = q
            res = req.get("%s.xml" % conf['openhub_api']['base_url'], params = params)
            tree = ElementTree.fromstring(res.content)
            if int(tree.find('.//items_returned').text) > 0:
                found = True
                projects = tree.findall(".//project")
                if len(projects) > 0:
                    for project in projects:
                        id_element = project.find('.//id')
                        name_element = project.find('.//name')
                        if id_element is not None and name_element is not None:
                            return id_element.text

    def set_openhub_stats(self):
    # get analyses
        stats = { 'total_code_lines': -1, 'PAI': 'N/A', 'licenses': [] }
        res = req.get("%s.xml" % self.openhub['api_url'], params = self.openhub['base_params'])
        tree = ElementTree.fromstring(res.content)
        status = tree.find('.//status')
        if status is not None and status.text == 'success':
            analysis = tree.find('.//analysis')
            if analysis is not None:
    # collect statistics
                tcl = analysis.find('total_code_lines')
                if tcl is not None:
                    stats['total_code_lines'] = int(tcl.text)
    # collect PAI
            project_activity_index = tree.find('.//project_activity_index/description').text
            if project_activity_index is not None:
                stats['PAI'] = project_activity_index
    # collect License
            licenses = tree.findall('.//licenses/license/name')
            if licenses is not None:
                for license in licenses:
                    stats['licenses'].append(license.text)
        return stats

    def generate_csv_line(self, path):
        line = []
        line.append(self.owner)
        line.append(self.name)
        line.append(str(self.issues["total_count"]))
        line.append(str(self.issues["avg_closed_time"]))
        line.append(str(self.openhub["stats"]["total_code_lines"]))
        line.append("\"" + str('/'.join(self.languages.keys())) + "\"")
        line.append(str(self.openhub["stats"]["PAI"]))
        line.append(str(self.contributors["total_commits"]))
        line.append(str(self.contributors["bus_factor"]))
        with open(path, 'a') as f:
            f.write(",".join(line) + "\n")

    def to_json(self):
        return { 
            'owner': self.owner,
            'name': self.name,
            'issues': self.issues,
            'oselfenhub': self.openhub,
            'languages': self.languages,
            'contributors': self.contributors,
            'created_at': self.created_at
        }        

