import json
import sys
import shutil

input_json_file = sys.argv[1]
grab_filename = sys.argv[2]
output_filename = sys.argv[3]

with open(input_json_file) as f:
	data = json.load(f)
	req_file = data["files"][grab_filename]
	# adding exception handling
	try:
		shutil.copy(req_file, output_filename)
		print "Copied", req_file, "to", output_filename
	except IOError as e:
		print("Unable to copy file. %s" % e)
	except:
		print("Unexpected error:", sys.exc_info())
