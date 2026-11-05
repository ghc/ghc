import re
from typing import Callable

POSIX_REGEXP_PATTERN = re.compile(
    r"\[:(alpha|upper|lower|digit|alnum|xdigit|space|blank|print|punct|graph|word|ascii|cntrl):]"
)
POSIX_REGEXP_REPLACEMENTS = {
    "alpha": "A-Za-z",
    "upper": "A-Z",
    "lower": "a-z",
    "digit": "0-9",
    "alnum": "A-Za-z0-9",
    "xdigit": "A-Fa-f0-9",
    "space": r"\s",
    "blank": r" \t",
}

NEGATED_SET_WITHOUT_NEWLINES = re.compile(r"([^\\]|^)\[\^((?!\\n))")


def posix_to_python_regex(expr: str) -> str:
    """
    We need to translate things like `[:alpha:]` to `[A-Za-z]`, etc.

    This also takes care of a little known fact about the llvm::Regex implementation:

    ```
    enum llvm::Regex::RegexFlags::Newline = 2U

    Compile for newline-sensitive matching. With this flag '[^' bracket
    expressions and '.' never match newline. A ^ anchor matches the
    null string after any newline in the string in addition to its normal
    function, and the $ anchor matches the null string before any
    newline in the string in addition to its normal function.
    ```

    This bad boy is enabled in all FileCheck cases, meaning we need to also add `\n` to all
    negative bracket expressions, otherwise we'll eat *so* many newlines.

    LLVM supports them, but pythons regex doesn't.
    """
    while (match := POSIX_REGEXP_PATTERN.search(expr)) is not None:
        if match.group(1) not in POSIX_REGEXP_REPLACEMENTS:
            raise ValueError(
                f"Can't translate posix regex, unknown character set: {match.group(1)}"
            )
        expr = expr.replace(match.group(0), POSIX_REGEXP_REPLACEMENTS[match.group(1)])

    expr = NEGATED_SET_WITHOUT_NEWLINES.sub(r"\1[^\\n\2", expr)

    return expr


def mlir_regex_extensions(expr: str) -> str:
    """
    This implements the special extensions for MLIR regex classes, enabled with the
    FILECHECK_FEATURE_ENABLE=MLIR_REGEX_CLS feature flag.
    """
    return expr.replace(r"\V", r"%([0-9]+|[A-Za-z_.$-][A-Za-z_.$0-9-]*)(#\d+)?")


ENCODINGS_MAP = {
    "u": r"\d",
    "d": r"[+-]?\d",
    "x": "[a-f0-9]",
    "X": "[A-F0-9]",
}


def pattern_from_num_subst_spec(
    digits: str | None, encoding: str | None
) -> tuple[str, Callable[[str], int]]:
    digits_expr = "+" if digits is None else f"{{{int(digits[1:])}}}"
    if encoding is None:
        encoding = "u"
    return f"{ENCODINGS_MAP[encoding]}{digits_expr}", (
        hex_int if encoding.lower() == "x" else int
    )


def hex_int(v: str):
    return int(v, base=16)
