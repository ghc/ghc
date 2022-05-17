#!/usr/bin/env python3

import sys
from pathlib import Path
import csv
import unicodedata
import argparse
from operator import methodcaller

def parse_codepoint(s: str, base=16):
    return chr(int(s, base))

def general_category(c: str):
    match(unicodedata.category(c)):
        case "Lu":
            return "UppercaseLetter"
        case "Ll":
            return "LowercaseLetter"
        case "Lt":
            return "TitlecaseLetter"
        case "Lm":
            return "ModifierLetter"
        case "Lo":
            return "OtherLetter"
        case "Mn":
            return "NonSpacingMark"
        case "Mc":
            return "SpacingCombiningMark"
        case "Me":
            return "EnclosingMark"
        case "Nd":
            return "DecimalNumber"
        case "Nl":
            return "LetterNumber"
        case "No":
            return "OtherNumber"
        case "Pc":
            return "ConnectorPunctuation"
        case "Pd":
            return "DashPunctuation"
        case "Ps":
            return "OpenPunctuation"
        case "Pe":
            return "ClosePunctuation"
        case "Pi":
            return "InitialQuote"
        case "Pf":
            return "FinalQuote"
        case "Po":
            return "OtherPunctuation"
        case "Sm":
            return "MathSymbol"
        case "Sc":
            return "CurrencySymbol"
        case "Sk":
            return "ModifierSymbol"
        case "So":
            return "OtherSymbol"
        case "Zs":
            return "Space"
        case "Zl":
            return "LineSeparator"
        case "Zp":
            return "ParagraphSeparator"
        case "Cc":
            return "Control"
        case "Cf":
            return "Format"
        case "Cs":
            return "Surrogate"
        case "Co":
            return "PrivateUse"
        case "Cn":
            return "NotAssigned"
        case cat:
            raise ValueError((c, cat))

def check(r: bool, info):
    if r:
        return True
    else:
        raise AssertionError(info)

def make_parser():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "csv", type=Path,
        metavar="FILE",
        help="CSV file to analyse."
    )
    parser.add_argument(
        "-v", "--verbosity",
        action="count", default=0,
        help="Verbosity level. Default: %(default)s"
    )
    return parser

def check_case_mapping(case_mapping: str, char: str, raw_expected: str, verbosity: int):
    got = methodcaller(case_mapping)(char)
    expected = parse_codepoint(raw_expected)
    if len(got) == 1:
        check(got == expected, (char, got, expected))
    elif verbosity > 0:
        print(
            f"[INFO] Skipped {case_mapping} check for: {codepoint}; "
            f"“{got}” ({len(got)} chars) cannot be compared to "
            f"“{expected}” (1 char)."
        )

if __name__ == "__main__":

    parser = make_parser()
    args = parser.parse_args()
    path = args.csv
    verbosity = args.verbosity

    with path.open("rt", encoding="utf-8") as fp:
        version = next(fp).strip()
        if version != unicodedata.unidata_version:
            sys.exit(
                f"Incompatible Python Unicode version: expecting “{version}”, "
                f"but got: “{unicodedata.unidata_version}”."
            )
        # Skip header
        next(fp)
        reader = csv.reader(fp)
        for row in reader:
            raw_code, gc, raw_lower, raw_upper, raw_title = row
            char = parse_codepoint(raw_code)
            codepoint = f"U+{raw_code.upper():0>4}"
            check(gc == general_category(char), (char, gc, general_category(char)))
            check_case_mapping("lower", char, raw_lower, verbosity)
            check_case_mapping("upper", char, raw_upper, verbosity)
            check_case_mapping("title", char, raw_title, verbosity)
