import requests
import os
import argparse
import csv

############
# internal #
############
def path_exists(string):
  if os.path.exists(string):
    return string
  else:
    raise NotADirectoryError(string)

def get_proj_assignments(auth_headers):
  def go(next_page = None, projs = []):
    if next_page != None:
      next_projects = requests.get(next_page, headers = auth_headers).json()

      return go(next_projects['next_page'], projs + next_projects['project_assignments'])
    else:
      return projs

  return go("https://api.harvestapp.com/v2/users/me/project_assignments")

############
# commands #
############
def get_tasks(auth_headers):
  proj_assignments = get_proj_assignments(auth_headers)

  for proj_assign in proj_assignments:
    client_name = proj_assign['client']['name']
    proj = proj_assign['project']
    tasks = [task_assign['task'] for task_assign in proj_assign['task_assignments']]

    for task in tasks:
      print(f"\"{client_name}\",\"{proj['id']}\",\"{proj['name']}\",\"{task['id']}\",\"{task['name']}\"")

def delete_timesheets(auth_headers, ids = []):
  for _id in ids:
    res = requests.delete(
      f"https://api.harvestapp.com/v2/time_entries/{_id}",
      headers = auth_headers
    )

    if res.status_code != 200:
      raise Exception(f"Failed to delete timesheet {_id}\n{res.content}")


def push_tasks(auth_headers, path):
  with open(path, newline='\n') as csv_data:
    csvreader = csv.reader(csv_data, delimiter = ',')

    next(csvreader, None)

    for row in csvreader:
      unpushed_id = row[0]
      timesheet_id = row[1]

      data = {
        "project_id": row[2],
        "task_id": row[3],
        "spent_date": row[4],
        "hours": row[5]
      }

      # DELETEs in Harvest
      # on patch error, create
      # somehow return the new timesheet id and update that
      # DELETEs in org
      # -> we need to do a diff, get the current state file, do a diff, before update
      # -> find IDs that no longer exists, python should delete these

      if unpushed_id != "null":
        res = requests.post("https://api.harvestapp.com/v2/time_entries",
                            headers = auth_headers,
                            json = data)

        # if (res.status_code == 201):
        if (res.status_code == 201):
          json = res.json()
          print(f"{unpushed_id},{json['id']}")
        else:
          raise Exception(res.content)
      else:
        res = requests.patch(f"https://api.harvestapp.com/v2/time_entries/{timesheet_id}",
                             headers = auth_headers,
                             json = data)

        if (res.status_code != 200):
          raise Exception(res.content)

##############
# entrypoint #
##############
def main():
  ######################
  # parse harvest data #
  ######################
  harvest_pat = os.environ.get('HARVEST_PAT')
  harvest_account_id = os.environ.get('HARVEST_ACCOUNT_ID')

  if harvest_pat is None or harvest_pat == "":
    raise Exception('HARVEST_PAT environment variable is needed')

  if harvest_account_id is None or harvest_account_id == "":
    raise Exception('HARVEST_ACCOUNT_ID environment variable is needed')

  auth_headers = {
    'Authorization': f"Bearer {harvest_pat}",
    'Harvest-Account-Id': harvest_account_id
  }

  ##################
  # parse cmd args #
  ##################
  parser = argparse.ArgumentParser(description="org-harvest")
  subparsers = parser.add_subparsers(dest="command", required=True)

  parser_get = subparsers.add_parser("get_tasks", help="Retrieve tasks")
  parser_get.set_defaults(func=lambda _: get_tasks(auth_headers))

  parser_deletes = subparsers.add_parser("delete_timesheets",
                                         help="Delete timesheets from Harvest.")
  parser_deletes.add_argument("--ids",
                              dest="delete_ids",
                              required=True,
                              default=[],
                              type=lambda t: [s.strip() for s in t.split(',')])

  parser_deletes.set_defaults(func=lambda args: delete_timesheets(auth_headers, args.delete_ids))

  parser_push = subparsers.add_parser("push_tasks", help="Push tasks from a CSV file")
  parser_push.add_argument('--from',
                           type=path_exists,
                           dest='from_file',
                           required=True,
                           help='Path to the CSV file')

  parser_push.set_defaults(func=lambda args: push_tasks(auth_headers, args.from_file))

  args = parser.parse_args()
  args.func(args)

main()
