from Project import Project
from Configuration import conf


PROJECTS_FILENAME = "projects.txt"

OUTPUT_CSV_PATH = "github_stats.csv"

def generate_project_list(filename):
    projects = []
    with open(filename, 'rU') as f:
        for line in f.readlines():
            line_stripped = line.rstrip()
            parts = line_stripped.split(',')
            projects.append(tuple(parts))
    return projects

def iter_projects(projects):
    res = []
    for project in projects:
        print("working on %s/%s" % project)
        p = Project(project[0],project[1])
        print(" - analyzing issues")
        p.set_openhub_data()
        print("    OPENHUB\n    " + str(p.openhub))
        p.set_issue_data()
        print("    ISSUES\n    " + str(p.issues))
        res.append(p)
    return res

def generate_csv_dataset(projects_metrics):
    with open(OUTPUT_CSV_PATH, 'w') as f:
        f.write("owner,name,issues.total_count,issues.avg_closed_time,total_code_lines\n")
    for project in projects_metrics:
        line = []
        line.append(project.owner)
        line.append(project.name)
        line.append(str(project.issues["total_count"]))
        line.append(str(project.issues["avg_closed_time"]))
        line.append(str(project.openhub["stats"]["total_code_lines"]))

        with open(OUTPUT_CSV_PATH, 'a') as f:
            f.write(",".join(line) + "\n")

if __name__ == "__main__":
    projects = generate_project_list(PROJECTS_FILENAME)
    projects_metrics = iter_projects(projects)
    generate_csv_dataset(projects_metrics)
