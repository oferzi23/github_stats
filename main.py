from Project import Project
from Configuration import conf
import requests as req
from xml.etree import ElementTree

PROJECTS_FILENAME = "projects2.txt"

OUTPUT_CSV_PATH = "github_stats2.csv"

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
            print(" - analyzing issues")
            p.set_openhub_data()
            print("    - OPENHUB\n    " + str(p.openhub))
            p.set_issue_data()
            print("    - ISSUES\n    " + str(p.issues))
            # p.set_truck_factor()
            p.generate_csv_line(OUTPUT_CSV_PATH)
            res.append(p)
        except Exception as e:
            print("      ERROR - error while analyzing project -= %s/%s =- \n" % (p.owner,p.name) + str(e) )
    return res

def generate_csv_dataset(projects_metrics):
    with open(OUTPUT_CSV_PATH, 'w') as f:
        f.write("owner,name,issues.total_count,issues.avg_closed_time,total_code_lines,languages\n")
    for project in projects_metrics:
        line = []
        line.append(project.owner)
        line.append(project.name)
        line.append(str(project.issues["total_count"]))
        line.append(str(project.issues["avg_closed_time"]))
        line.append(str(project.openhub["stats"]["total_code_lines"]))
        line.append("\"" + str('/'.join(project.openhub["stats"]["languages"])) + "\"")
        with open(OUTPUT_CSV_PATH, 'a') as f:
            f.write(",".join(line) + "\n")

if __name__ == "__main__":
    # projects = generate_project_list()
    projects = read_project_list(PROJECTS_FILENAME)
    projects_metrics = iter_projects(projects)
    # generate_csv_dataset(projects_metrics)
