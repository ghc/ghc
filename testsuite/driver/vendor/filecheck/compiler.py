import re

from filecheck.error import CheckError
from filecheck.ops import (
    Literal,
    RE,
    Capture,
    NumSubst,
    PseudoVar,
    Subst,
    CheckOp,
    VALUE_MAPPER_T,
)
from filecheck.options import Options

UNESCAPED_BRACKETS = re.compile(r"^\(|[^\\]\(")

CHECK_EMPTY_EXPR = re.compile(r"[^\n]*\n\n")


def compile_uops(
    check: CheckOp, variables: dict[str, str | int], opts: Options
) -> tuple[re.Pattern[str], dict[str, tuple[int, VALUE_MAPPER_T]]]:
    """
    Compile a series of uops, given a set of variables, to a regex pattern and an
    extraction dictionary.

    The extraction dictionary tells you which Capture can be found in which regex
    group.
    """
    groups = 0
    expr: list[str] = []
    if check.name == "EMPTY":
        return CHECK_EMPTY_EXPR, dict()

    captures: dict[str, tuple[int, VALUE_MAPPER_T]] = dict()

    for uop in check.uops:
        if isinstance(uop, Literal):
            # literals are matched as is
            if opts.strict_whitespace:
                expr.append(re.escape(uop.content))
            else:
                # TODO: fix this mess
                # basically, I need to replace all whitespaces in the original literal by "\s+"
                # but I still want to regex escape them
                # and re.sub doesn't let me insert \s into the new string for some reason......
                expr.append(
                    re.sub(r"(\\ )+", " ", re.escape(uop.content)).replace(
                        " ", r"[ \t\v]+"
                    ),
                )

        elif isinstance(uop, RE):
            # For regexes, we must make sure that we count the number of capture groups
            # present, so that we know which ones contain the Captures, and which ones
            # contain "user supplied" groups.

            # count number of capture groups by counting unescaped brackets
            groups += len(UNESCAPED_BRACKETS.findall(uop.content))
            # add brackets sorrounding patterns that contain ORs (fixes #3)
            if "|" in uop.content:
                expr.append(f"({uop.content})")
                groups += 1
            else:
                expr.append(uop.content)
        elif isinstance(uop, Capture):
            # record the group we capture in the dictionary
            captures[uop.name] = (groups + 1, uop.value_mapper)
            # this is used to match same-line use of variables
            # like this:
            # // CHECK: alloc [[REG:[a-z]+]], [[REG]]
            # here we can't insert the value now, as it will only be determined when we
            # match the pattern. Luckily, python regex has backwards references.
            expr.append(f"({uop.pattern})")
            groups += len(UNESCAPED_BRACKETS.findall(uop.pattern)) + 1
        elif isinstance(uop, Subst):
            # if we have substitutions, check if the variable is defined in this line
            if uop.variable in captures:
                expr.append(f"\\{captures[uop.variable][0]}")
            else:
                # otherwise match immediate
                if uop.variable not in variables:
                    raise CheckError(
                        f"Variable {uop.variable} referenced before assignment",
                        check,
                    )
                expr.append(re.escape(str(variables[uop.variable])))
        elif isinstance(uop, PseudoVar):
            expr.append(f"{check.source_line + uop.offset}")
        elif isinstance(uop, NumSubst):
            # we don't do numerical substitutions yet
            raise NotImplementedError("Numerical substitutions not supported!")
    pattern = "".join(expr)
    if opts.match_full_lines:
        # anchor the pattern to the whole line; in non-strict mode, leading and
        # trailing whitespace in the input is ignored, like upstream
        ws = "" if opts.strict_whitespace else r"[ \t\v]*"
        pattern = rf"(?:(?<=\n)|(?=\A)){ws}{pattern}{ws}(?=\n|\Z)"
    if check.name == "NEXT":
        # the prefix is added after the full-line anchors, so that the anchors
        # apply to the check itself and not to the skipped leading characters
        pattern = r"\n?[^\n]*" + pattern
    try:
        # compile with MULTILINE flag, so that `^` and `$` can match start/end of line correctly
        return re.compile(pattern, flags=re.MULTILINE), captures
    except re.error:
        raise CheckError(f"Malformed regex expression: '{pattern}'", check)
