"""
parser for filecheck syntax
"""

import io
import re
from dataclasses import dataclass, field
from typing import Iterator, TextIO

from filecheck.error import ParseError
from filecheck.ops import (
    CheckOp,
    Literal,
    RE,
    Capture,
    PseudoVar,
    Subst,
    NumSubst,
    UOp,
    CountOp,
)
from filecheck.options import Options, parse_argv_options, Extension
from filecheck.regex import (
    posix_to_python_regex,
    pattern_from_num_subst_spec,
    mlir_regex_extensions,
)


def pattern_for_opts(opts: Options) -> tuple[re.Pattern[str], re.Pattern[str]]:
    prefixes = f"({'|'.join(map(re.escape, opts.check_prefixes))})"
    return re.compile(
        r"(^|[^a-zA-Z-_])"
        + prefixes
        + r"(-(DAG|COUNT-\d+|NOT|EMPTY|NEXT|SAME|LABEL))?(\{LITERAL})?:([^\n]*)\n?"
    ), re.compile(f"({'|'.join(map(re.escape, opts.comment_prefixes))}).*{prefixes}")


# see https://llvm.org/docs/CommandGuide/FileCheck.html#filecheck-string-substitution-blocks
VAR_CAPTURE_PATTERN = re.compile(r"\[\[(\$?[a-zA-Z_][a-zA-Z0-9_]*):([^\n]*)]]")

# see https://llvm.org/docs/CommandGuide/FileCheck.html#filecheck-string-substitution-blocks
VAR_SUBST_PATTERN = re.compile(r"\[\[(\$?[a-zA-Z_][a-zA-Z0-9_]*)]]")

# numeric substitution blocks, see:
# https://llvm.org/docs/CommandGuide/FileCheck.html#filecheck-numeric-substitution-blocks
NUMERIC_CAPTURE_PATTERN = re.compile(
    r"\[\[#(%(\.[0-9]+)?([udxX])?,)?((\$?[a-zA-Z_][a-zA-Z0-9_]+):(\d+)?)?]]"
)

# numeric substitution blocks, see:
# https://llvm.org/docs/CommandGuide/FileCheck.html#filecheck-numeric-substitution-blocks
NUMERIC_SUBST_PATTERN = re.compile(
    r"\[\[#(\$?[a-zA-Z_][a-zA-Z0-9_]*)([a-z0-9 +\-()]*)]]"
)

# pseudo numeric variable, see:
# https://llvm.org/docs/CommandGuide/FileCheck.html#filecheck-pseudo-numeric-variables
PSEUDO_NUMERIC_VARIABLE = re.compile(r"\[\[# @LINE ?(([+-]) (\d+))?]]")

LINE_SPLIT_RE = split = re.compile(r"(\{\{|\[\[\$?[#a-zA-Z_]|]|})")


@dataclass
class Parser(Iterator[CheckOp]):
    """
    parse a file containing filecheck directives and returns all found check directives
    """

    opts: Options

    input: TextIO

    check_line_regexp: re.Pattern[str]
    comment_line_regexp: re.Pattern[str]

    line_no: int = field(default=0)
    line: str = field(default="")

    @classmethod
    def from_opts(cls, opts: Options):
        with open(opts.match_filename, encoding="utf-8") as f:
            content = f.read()
        return Parser(opts, io.StringIO(content), *pattern_for_opts(opts))

    def __next__(self) -> CheckOp:
        """
        Scans forward in self.input until a line matching  self.check_line_regexp is
        found. Returns the kind (CHECK/NEXT/DAG/EMPTY/NOT) and the arg (whatever comes
        after the colon).

        Note that for all CHECK-<thing>: it returns <thing> as kind, but for CHECK: it
        returns CHECK.
        """
        while True:
            self.line_no += 1
            line = self.input.readline()
            if line == "":
                raise StopIteration()
            # skip any lines containing comment markers before checks
            if self.comment_line_regexp.search(line) is not None:
                continue
            match = self.check_line_regexp.search(line)
            # no check line = skip
            if match is None:
                continue
            prefix = match.group(2)
            kind = match.group(4)
            literal = match.group(5)
            arg = match.group(6)
            if kind is None:
                kind = "CHECK"
            if arg is None:
                arg = ""
            if not (self.opts.strict_whitespace and self.opts.match_full_lines):
                arg = arg.strip()

            # verify that non-empty checks have an actual thing to match
            if kind != "EMPTY":
                if not arg:
                    offset = (
                        match.start(4) if match.group(4) is not None else match.start(2)
                    )
                    raise ParseError(
                        f"found empty check string with prefix '{prefix}:'",
                        self.line_no,
                        offset,
                        line,
                    )

            # parse the uops, but only if we are not in LITERAL mode
            uops: list[UOp]
            if literal is None:
                uops = self.parse_args(arg, line)
            else:
                uops = [Literal(arg)]

            # special case for COUNT ops
            if kind.startswith("COUNT"):
                count = int(kind[6:])
                if count == 0:
                    raise ParseError(
                        f"invalid count in -COUNT specification on prefix '{prefix}' "
                        f"(count can't be 0)",
                        self.line_no,
                        match.end(2),
                        line,
                    )
                return CountOp(
                    prefix,
                    "COUNT",
                    arg,
                    self.line_no,
                    uops,
                    is_literal=literal is not None,
                    count=count,
                )
            return CheckOp(
                prefix,
                kind,
                arg,
                self.line_no,
                uops,
                is_literal=literal is not None,
            )

    def parse_args(self, arg: str, line: str) -> list[UOp]:
        """
        parse check args into uops that can later be compiled into a regex

        Returns a list of uops
        """
        uops: list[UOp] = []
        parts = LINE_SPLIT_RE.split(arg)
        offset = len(line) - len(arg)
        while parts:
            part = parts.pop(0)
            if part.startswith("[["):
                brackets = 2
                # grab parts greedily until we hit a ]]
                while brackets > 0:
                    if not parts:
                        raise ParseError(
                            "Invalid substitution block, no ]]",
                            self.line_no,
                            offset,
                            line,
                        )
                    addition = parts.pop(0)
                    brackets += addition.count("[") - addition.count("\\[")
                    brackets -= addition.count("]") + addition.count("\\]")
                    part += addition
                # check if we are a simple capture pattern [[<name>:<regex>]]
                if match := VAR_CAPTURE_PATTERN.fullmatch(part):
                    re_expr = posix_to_python_regex(match.group(2))
                    if Extension.MLIR_REGEX_CLS in Extension:
                        re_expr = mlir_regex_extensions(re_expr)
                    uops.append(
                        Capture(
                            match.group(1),
                            re_expr,
                            str,
                        )
                    )
                # check if we are a simple substitution pattern: [[<<name>>]]
                elif match := VAR_SUBST_PATTERN.fullmatch(part):
                    uops.append(Subst(match.group(1)))
                # check if we are a numeric substitution pattern [[#<name>(<expr>)?]]
                elif match := NUMERIC_SUBST_PATTERN.fullmatch(part):
                    # simplify to non-numeric substitution if expr is empty
                    if match.group(2) is None or match.group(2).strip() == "":
                        uops.append(Subst(match.group(1)))
                    else:
                        uops.append(NumSubst(match.group(1), match.group(2)))
                # numeric capture patterns are a tricky bunch
                # see https://llvm.org/docs/CommandGuide/FileCheck.html#filecheck-numeric-substitution-blocks
                elif match := NUMERIC_CAPTURE_PATTERN.fullmatch(part):
                    pattern, mapper = pattern_from_num_subst_spec(
                        match.group(2), match.group(3)
                    )
                    uops.append(Capture(match.group(5), pattern, mapper))
                # check if we are a pseudo numeric variable: [[# @line [+-] <offset>]]
                # see https://llvm.org/docs/CommandGuide/FileCheck.html#filecheck-pseudo-numeric-variables
                elif match := PSEUDO_NUMERIC_VARIABLE.fullmatch(part):
                    if match.group(1):
                        offset = int(match.group(3))
                        if match.group(2) == "-":
                            offset *= -1
                        uops.append(PseudoVar(offset))
                    else:
                        uops.append(PseudoVar(0))
                else:
                    raise ParseError(
                        f"Invalid substitution block, unknown format: {part}",
                        self.line_no,
                        offset,
                        line,
                    )
            elif part == "{{":
                while not part.endswith("}}"):
                    if not parts:
                        raise ParseError(
                            "Invalid regex block, no }}", self.line_no, offset, line
                        )
                    part += parts.pop(0)

                pattern = part[2:-2]

                re_expr = posix_to_python_regex(pattern)
                if Extension.MLIR_REGEX_CLS in Extension:
                    re_expr = mlir_regex_extensions(re_expr)

                uops.append(RE(re_expr))
            elif part != "":
                uops.append(Literal(part))
            offset += len(part)
        return uops


if __name__ == "__main__":
    import sys

    opts = parse_argv_options(sys.argv)
    for p in Parser.from_opts(opts):
        print(p)
