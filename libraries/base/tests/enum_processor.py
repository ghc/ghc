#!/usr/bin/env python

import sys

def process(s):
    while True:
        start = s.find('printTest')
        if start == -1:
            return s
        j0 = j = s.index('(', start) + 1
        depth = 1
        while depth > 0:
            if s[j] == '(':
                depth += 1
            if s[j] == ')':
                depth -= 1
            j += 1
        argument = s[j0:j-1]
        expansion = '(do{ putStr ( "    " ++ "%s" ++ " = " ) ; print (%s) })' \
                    % (argument, argument)
        s = s[:start] + expansion + s[j:]

_, _, inputFile, outputFile = sys.argv
open(outputFile, 'w').write(process(open(inputFile, 'r').read()))
