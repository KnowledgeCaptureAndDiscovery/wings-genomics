import sys
import json
import rdflib

wfns='http://purl.org/net/wf-invocation#'
exns='http://www.wings-workflows.org/ontology/execution.owl#'
runid=sys.argv[1]

# Load the run id
g=rdflib.Graph()
g.load(runid)

# Get the plan uri
run=rdflib.URIRef(runid)
hasPlan=rdflib.URIRef(exns+'hasPlan')
plan=g.value(run, hasPlan)

# Load the plan
if plan:
    g.load(plan)
    # Query for Variable bindings
    vbindings={}
    query='select ?v ?d where { ?v <'+wfns+'hasDataBinding> ?d }'
    for row in g.query(query):
        varid=str(row.v)
        varname=varid[varid.index("#")+1:]
        vbindings[varname] = str(row.d)

    rundetails={
        "runid": runid,
        "files": vbindings
    }

    print(json.dumps(rundetails))
