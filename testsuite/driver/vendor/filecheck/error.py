import re
from dataclasses import dataclass
from typing import Any

from filecheck.ops import CheckOp


class CheckError(Exception):
    message: str
    op: CheckOp

    def __init__(self, msg: str, op: CheckOp, *args: Any):
        super().__init__(*args)
        self.message = msg
        self.op = op


@dataclass
class ErrorOnMatch(Exception):
    """
    Signal error on the provided match
    """

    message: str
    op: CheckOp
    match: re.Match[str]


@dataclass
class ParseError(Exception):
    """
    Signal an error during parsing
    """

    message: str
    line_no: int
    offset: int
    offending_line: str
