from Project import Project
from Configuration import conf
import requests as req
from xml.etree import ElementTree
import json

PROJECTS_FILENAME = "test/projects.txt"
OUTPUT_CSV_PATH = "test/github_stats.csv"
OUTPUT_JSON_PATH = "test/github_stats.json"

# PROJECTS_FILENAME = "projects2.txt"
# OUTPUT_CSV_PATH = "github_stats2.csv"
# OUTPUT_JSON_PATH  = "github_stats2.json"

def generate_project_list():
    base_params = {'api_key': conf['openhub_api']['api_key'], 'page': 0 }
    api_url = "%s.xml" % (conf['openhub_api']['base_url'])
    names = []
    for i in range(0,8):
        base_params['page'] = i
        res = req.get(api_url, params=base_params)
        tree = ElementTree.fromstring(res.content)
        name_elements = tree.findall('.//name')
        for n in name_elements:
            names.append(n.text)
    print ('\n'.join(names))
      
def read_project_list(filename):
    projects = []
    with open(filename, 'rU') as f:
        for line in f.readlines():
            line_stripped = line.rstrip()
            if line_stripped[0] != '#':
                parts = line_stripped.split(',')
                projects.append(tuple(parts))
    return projects

def iter_projects(projects):
    res = []
    for project in projects:
        try:
            print("working on %s" % project[0])
            p = Project(project[0],project[1],project[2])
            print("    - CREATED AT: " + str(p.created_at))
            p.set_language_data()
            print("    - LANGUAGES\n    " + str(p.languages))
            p.set_contrib_data()
            print("    - CONTRIBUTORS\n    " + str(p.contributors))
            p.set_openhub_data()
            print("    - OPENHUB\n    " + str(p.openhub))
            p.set_issue_data()
            print("    - ISSUES\n    " + str(p.issues))
            # p.generate_csv_line(OUTPUT_CSV_PATH)
            res.append(p)
        except Exception as e:
            print("      ERROR - error while analyzing project -= %s/%s =- \n" % (project[1],project[2]))
            print(e.__str__())
    print("FINISHED RUN")
    return res

def generate_json_datatset(data, path):
    dataset = []
    for p in data:
        dataset.append(p.to_json())
    with open(path, 'w') as f:
        f.write(json.dumps(dataset))

if __name__ == "__main__":
    # projects = generate_project_list()
    projects = read_project_list(PROJECTS_FILENAME)
    projects_metrics = iter_projects(projects)
    generate_json_datatset(projects_metrics, OUTPUT_JSON_PATH)
