# Sphinx's configuration file
#
# This is used by $(TOP)/configure to check if the sphinx is for
# python3 or later.
#

import sys

if (sys.version_info >= (3, 0)):
  pass
else:
  sys.exit(1)
