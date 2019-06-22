"""
This module provides some type definitions and backwards compatibility shims
for use in the testsuite driver.

The testsuite driver can be typechecked using mypy [1].


[1] http://mypy-lang.org/
"""

try:
    from typing import *
except:
    # The backwards compatibility stubs must live in another module lest
    # mypy complains.
    from typing_stubs import * # type: ignore

WayName = NewType("WayName", str)
TestName = NewType("TestName", str)
OutputNormalizer = Callable[[str], str]
IssueNumber = NewType("IssueNumber", int)

# TextIO is missing on some older Pythons.
if 'TextIO' not in globals():
    TextIO = None
