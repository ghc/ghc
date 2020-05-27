"""
This module provides some type definitions and backwards compatibility shims
for use in the testsuite driver.

The testsuite driver can be typechecked using mypy [1].


[1] http://mypy-lang.org/
"""

try:
    from typing import *
    import typing
except:
    # The backwards compatibility stubs must live in another module lest
    # mypy complains.
    from typing_stubs import * # type: ignore


####################################################
# Backwards compatibility shims
#
# N.B. mypy appears to typecheck as though the "then" clause of if structures
# is taken. We exploit this below.

# TextIO is missing on some older Pythons.
if 'TextIO' not in globals():
    try:
        from typing import TextIO
    except ImportError:
        TextIO = None # type: ignore
else:
    TextIO = None # type: ignore


####################################################
# Testsuite-specific types

WayName = NewType("WayName", str)
TestName = NewType("TestName", str)
OutputNormalizer = Callable[[str], str]
IssueNumber = NewType("IssueNumber", int)

# Used by perf_notes
GitHash = NewType("GitHash", str) # a Git commit hash
GitRef = NewType("GitRef", str)
TestEnv = NewType("TestEnv", str)
MetricName = NewType("MetricName", str)
