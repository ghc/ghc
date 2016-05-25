#!/usr/bin/env python

# A linter to warn for ASSERT macros which are separated from their argument
# list by a space, which Clang's CPP barfs on

import sys
import logging
import os
import json

def setup_logging(logger):
    """
    ``arc lint`` makes it quite tricky to catch debug output from linters.
    Log to a file to work around this.
    """
    hdlr = logging.FileHandler('linter.log', 'w')
    logger.addHandler(hdlr)
    logger.setLevel(logging.DEBUG)
    return logger

logger = logging.getLogger()
#setup_logging(logger)
logger.debug(sys.argv)

path = sys.argv[1]
warnings = []
if os.path.isfile(path):
    with open(path) as f:
        for lineno, line in enumerate(f):
            if 'ASSERT (' in line:
                warning = {
                    'severity': 'warning',
                    'message': 'CPP macros should not have a space between the macro name and their argument list',
                    'line': lineno+1,
                }
                warnings.append(warning)

logger.debug(warnings)
print(json.dumps(warnings))
