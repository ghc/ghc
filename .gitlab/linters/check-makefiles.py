#!/usr/bin/env python3

"""
Linters for testsuite makefiles
"""

from linter import run_linters, RegexpLinter

"""
Warn for use of `--interactive` inside Makefiles (#11468).

Encourage the use of `$(TEST_HC_OPTS_INTERACTIVE)` instead of
`$(TEST_HC_OPTS) --interactive -ignore-dot-ghci -v0`. It's too easy to
forget one of those flags when adding a new test.
"""
interactive_linter = \
    RegexpLinter(r'--interactive',
                 message = "Warning: Use `$(TEST_HC_OPTS_INTERACTIVE)` instead of `--interactive -ignore-dot-ghci -v0`."
                ).add_path_filter(lambda path: path.name == 'Makefile')

test_hc_quotes_linter = \
    RegexpLinter('\t\\$\\(TEST_HC\\)',
                 message = "Warning: $(TEST_HC) should be quoted in Makefiles.",
                ).add_path_filter(lambda path: path.name == 'Makefile')

linters = [
    interactive_linter,
    test_hc_quotes_linter,
]

if __name__ == '__main__':
    run_linters(linters,
                subdir='testsuite')
