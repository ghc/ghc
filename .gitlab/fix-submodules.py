#!/usr/bin/python

import re

x = open('.gitmodules').read()
x = re.sub(r"url *= *\.\.", "url = https://gitlab.haskell.org/ghc", x)
open('.gitmodules', 'w').write(x)

