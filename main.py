from Project import Project

BASE_URL = "https://api.github.com"

PROJECTS = [("elastic","kibana")]

def iter_projects():
    res = []
    for project in PROJECTS:
        p = Project(project[0],project[1])
        p.get_issue_data()
        for key, val in p.issues.iteritems():
            print(key, val)

if __name__ == "__main__":
    projects_metrics = iter_projects()