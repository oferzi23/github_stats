import string
import json
from datetime import datetime
import requests as req

class Project:
    """describes a repository"""

    # members
    base_headers = {"Authorization": "token e4f0f58f35560e168113178687d66a942627ac55"}
    base_url  = ""
    owner = ""
    name  = ""
    issues = {}

    def __init__(self, owner, name):
        self.base_url = "https://api.github.com/repos/%s/%s" % (owner, name)
        self.owner = owner
        self.name = name

    def get_issue_data(self):
        res = req.get("%s/issues?state=closed" % (self.base_url), headers = self.base_headers)
        self.issues["pages"] = int(res.links["last"]["url"].split("=")[-1])
        self.issues["total_count"] = 0
        
        isum = 0
        for i in range(1, self.issues["pages"]+1):
            r = req.get("%s/issues?state=closed&page=%i" % (self.base_url, i), headers = self.base_headers)
            r_j = r.json()
            for issue in r_j:
                self.issues["total_count"] += 1
                opened = datetime.strptime(issue["created_at"], "%Y-%m-%dT%H:%M:%SZ")
                closed = datetime.strptime(issue["closed_at"], "%Y-%m-%dT%H:%M:%SZ")
                delta = closed - opened
                isum += delta.days
            print("after page %i: sum=%i, count=%i" % (i, isum, self.issues["total_count"]))
        self.issues["avg_closed_time"] = isum / self.issues["total_count"]
        

                

    
