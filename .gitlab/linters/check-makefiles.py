#!/usr/bin/env python3

"""
Warn for use of `--interactive` inside Makefiles (#11468).

Encourage the use of `$(TEST_HC_OPTS_INTERACTIVE)` instead of
`$(TEST_HC_OPTS) --interactive -ignore-dot-ghci -v0`. It's too easy to
forget one of those flags when adding a new test.
"""

from linter import run_linters, RegexpLinter

linters = [
    RegexpLinter(r'--interactive',
                 message = "Warning: Use `$(TEST_HC_OPTS_INTERACTIVE)` instead of `--interactive -ignore-dot-ghci -v0`.")
]

if __name__ == '__main__':
    run_linters(linters) #$, subdir='testsuite')
