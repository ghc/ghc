#!/usr/bin/python

import re

x = open('.gitmodules').read()
x = re.sub(r"url *= *\.\.", "url = https://git.haskell.org", x)
open('.gitmodules', 'w').write(x)

