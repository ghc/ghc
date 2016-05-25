#!/usr/bin/env python

# Warn for use of `--interactive` inside Makefiles (#11468).
#
# Encourage the use of `$(TEST_HC_OPTS_INTERACTIVE)` instead of
# `$(TEST_HC_OPTS) --interactive -ignore-dot-ghci -v0`. It's too easy to
# forget one of those flags when adding a new test.

import sys
import os
import json
import re

path = sys.argv[1]
warnings = []
if os.path.isfile(path):
    with open(path) as f:
        for lineno, line in enumerate(f):
            if '--interactive' in line:
                warning = {
                    'severity': 'warning',
                    'message': 'Use `$(TEST_HC_OPTS_INTERACTIVE)` instead of `--interactive -ignore-dot-ghci -v0`',
                    'line': lineno+1,
                }
                warnings.append(warning)

print(json.dumps(warnings))
