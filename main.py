import requests
import os

print("Starting org-clock agenda to Harvest")

harvest_pat = os.environ.get('HARVEST_PAT')
harvest_account_id = os.environ.get('HARVEST_ACCOUNT_ID')

if harvest_pat is None:
  raise Exception('HARVEST_PAT environment variable is needed')

if harvest_account_id is None:
  raise Exception('HARVEST_ACCOUNT_ID environment variable is needed')
