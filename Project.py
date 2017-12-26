import string
import json
import copy
from random import randint
from datetime import datetime
import requests as req
from Configuration import conf
from xml.etree import ElementTree

class Project:
    """describes a repository"""

    # members
    base_headers = {"Authorization": "token %s" % conf['github_api']['token']}

    def __init__(self, owner, name):
        self.project_github_api_url = "%s/%s/%s" % (conf['github_api']['base_url'],owner, name)
        self.owner = owner
        self.name = name
        self.issues = {}
        self.openhub = {}

    def set_openhub_data(self):
        self.openhub['base_params'] = {'api_key': conf['openhub_api']['api_key'] }
        self.openhub['stats'] = {}
        openhub_id = self.get_openhub_id()
        if openhub_id != -1:
            self.openhub['id'] = openhub_id
            self.openhub['api_url'] = "%s/%s" % (conf['openhub_api']['base_url'], self.openhub['id'])
            self.openhub['stats'] = self.set_openhub_stats()
        else:
            print("     ERROR - Can't get openhub id for this project")
            

    def set_issue_data(self):
        res = req.get("%s/issues?state=closed" % (self.project_github_api_url), headers = self.base_headers)
        if "last" in res.links:
            self.issues["pages"] = int(res.links["last"]["url"].split("=")[-1])
        else:
            self.issues["pages"] = 1

        self.issues["total_count"] = 0

        isum = 0
        for i in range(1, self.issues["pages"]+1):
            r = req.get("%s/issues?state=closed&page=%i" % (self.project_github_api_url, i), headers = self.base_headers)
            r_j = r.json()
            for issue in r_j:
                self.issues["total_count"] += 1
                opened = datetime.strptime(issue["created_at"], "%Y-%m-%dT%H:%M:%SZ")
                closed = datetime.strptime(issue["closed_at"], "%Y-%m-%dT%H:%M:%SZ")
                delta = closed - opened
                isum += delta.days
        self.issues["avg_closed_time"] = int(isum / self.issues["total_count"])
    
    def get_openhub_id(self):
        found = False
        options = ["%s/%s" % (self.owner, self.name), self.name]
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
        stats = { 'total_code_lines': -1, 'languages': [], 'PAI': 'N/A' }
        res = req.get("%s.xml" % self.openhub['api_url'], params = self.openhub['base_params'])
        tree = ElementTree.fromstring(res.content)
        if tree.find('.//status').text == 'success':
            analysis = tree.find('.//analysis')
            if analysis is not None:
    # collect statistics
                tcl = analysis.find('total_code_lines')
                if tcl is not None:
                    stats['total_code_lines'] = int(tcl.text)
    # collect languages
                languages = analysis.findall('.//languages/language')
                for lang in languages:
                    stats['languages'].append(lang.text)
    # collect PAI
        project_activity_index = tree.find('.//project_activity_index/description').text        
        stats['PAI'] = project_activity_index
        return stats

