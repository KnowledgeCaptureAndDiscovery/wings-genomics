import json
import sys
import shutil
import re

input_json_file = sys.argv[1]
grab_filename = sys.argv[2]

reObj = re.compile(grab_filename)
req_files = ""
with open(input_json_file) as f:
	data = json.load(f)
	data_files = data["files"]
	for key in data_files.keys():
		if reObj.match(key):
			req_file = data_files[key]
			req_files = req_files + " " + req_file
			#print req_file
print req_files
