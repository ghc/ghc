#!/usr/bin/env python

import binascii, sys

funcs = {
    'decode': binascii.a2b_base64,
    'encode': binascii.b2a_base64,
    'read': lambda x:x,
    }
    
f = funcs[sys.argv[1]]

for n in sys.argv[2:]:
    sys.stdout.write(f(open(n).read()))
