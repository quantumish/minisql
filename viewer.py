import json
import sys
import pkg_resources

installed = {pkg.key for pkg in pkg_resources.working_set}
if "rich" in installed:
    from rich import print

f = json.loads(open(sys.argv[1]).read())
for i in range(int(sys.argv[2])):
    print(f[i])
