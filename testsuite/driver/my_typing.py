"""
This module provides some type definitions and backwards compatibility shims
for use in the testsuite driver.

The testsuite driver can be typechecked using mypy [1].


[1] http://mypy-lang.org/
"""

from typing import *
import typing


####################################################
# Backwards compatibility shims
#
# N.B. mypy appears to typecheck as though the "then" clause of if structures
# is taken. We exploit this below.

from typing import TextIO


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
