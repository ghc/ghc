from __future__ import annotations

from dataclasses import dataclass, field
from typing import Callable, TypeAlias

from filecheck.options import Options

OP_KINDS = ("DAG", "COUNT", "NOT", "EMPTY", "NEXT", "SAME", "LABEL", "CHECK")

VALUE_MAPPER_T: TypeAlias = Callable[[str], int] | Callable[[str], str]


@dataclass(slots=True)
class CheckOp:
    """
    Represents a concrete check instruction (e.g. CHECK-NEXT)
    """

    prefix: str
    name: str
    arg: str
    source_line: int
    uops: list[UOp]
    is_literal: bool = field(default=False, kw_only=True)

    def check_line_repr(self):
        return f"{self.check_name}: {self.arg}"

    def source_repr(self, opts: Options) -> str:
        return (
            f"Check rule at {opts.match_filename}:{self.source_line}\n"
            f"{self.check_line_repr()}"
        )

    def _suffix(self):
        suffix = "{LITERAL}" if self.is_literal else ""
        if self.name == "CHECK":
            return suffix
        return f"-{self.name}{suffix}"

    @property
    def check_name(self):
        return f"{self.prefix}{self._suffix()}"


@dataclass(slots=True)
class CountOp(CheckOp):
    """
    special case for COUNT-<number>
    """

    count: int = field(kw_only=True)

    def _suffix(self):
        suffix = "{LITERAL}" if self.is_literal else ""
        return f"-COUNT{self.count}{suffix}"


@dataclass(frozen=True, slots=True)
class UOp:
    """
    micro-ops, these make up the filecheck matching logic
    """

    pass


@dataclass(frozen=True, slots=True)
class Literal(UOp):
    """
    literal match
    """

    content: str


@dataclass(frozen=True, slots=True)
class RE(UOp):
    """
    Regular expression matching
    """

    content: str


@dataclass(frozen=True, slots=True)
class Capture(UOp):
    """
    Variable capture expression

    Stuff like this:
    ```
    ; CHECK: test5:
    ; CHECK:    notw     [[REGISTER:%[a-z]+]]
    ; CHECK:    andw     {{.*}}[[REGISTER]]
    ```
    """

    name: str
    pattern: str
    value_mapper: VALUE_MAPPER_T


@dataclass(frozen=True, slots=True)
class Subst(UOp):
    """
    Variable substitution, e.g. expect the contents of a variable here.

    Stuff like this:
    ```
    ; CHECK: test5:
    ; CHECK:    notw     [[REGISTER:%[a-z]+]]
    ; CHECK:    andw     {{.*}}[[REGISTER]]
    ````
    """

    variable: str


@dataclass(frozen=True, slots=True)
class NumSubst(UOp):
    """
    Numeric substitution (substitute variable with derived expression).

    Stuff like this:
    ```
    ; CHECK: load r[[#REG:]], [r0]
    ; CHECK: load r[[#REG+1]], [r1]
    ; CHECK: Loading from 0x[[#%x,ADDR:]]
    ; CHECK-SAME: to 0x[[#ADDR + 7]]
    ```
    """

    variable: str
    expr: str


@dataclass(frozen=True, slots=True)
class PseudoVar(UOp):
    """
    Pseudo Numeric Variables (substitute @line with actual line number).

    Stuff like this:
    ```
    ; CHECK: [[# @line]]: error: ...
    ; CHECK: [[# @line + 1]]: warning: ...
    ```
    """

    offset: int
