import string
import json
from random import randint
from datetime import datetime
import requests as req

class Project:
    """describes a repository"""

    # members
    base_headers = {"Authorization": "token a338ce75cacd955386c953c5f2470c246ba5db89"}
    base_github_api_url  = ""

    def __init__(self, owner, name):
        self.base_github_api_url = "https://api.github.com/repos/%s/%s" % (owner, name)
        self.owner = owner
        self.name = name
        self.issues = {}

    def get_issue_data(self):
        res = req.get("%s/issues?state=closed" % (self.base_github_api_url), headers = self.base_headers)
        if "last" in res.links:
            self.issues["pages"] = int(res.links["last"]["url"].split("=")[-1])
        else:
            self.issues["pages"] = 1

        self.issues["total_count"] = 0

        isum = 0
        for i in range(1, self.issues["pages"]+1):
            r = req.get("%s/issues?state=closed&page=%i" % (self.base_github_api_url, i), headers = self.base_headers)
            r_j = r.json()
            for issue in r_j:
                self.issues["total_count"] += 1
                opened = datetime.strptime(issue["created_at"], "%Y-%m-%dT%H:%M:%SZ")
                closed = datetime.strptime(issue["closed_at"], "%Y-%m-%dT%H:%M:%SZ")
                delta = closed - opened
                isum += delta.days
        self.issues["avg_closed_time"] = int(isum / self.issues["total_count"])
