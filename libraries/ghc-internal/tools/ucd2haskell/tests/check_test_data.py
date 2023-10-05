#!/usr/bin/env python3

from collections import defaultdict
from pathlib import Path
import platform
import sys
import unicodedata
from functools import reduce #, partial
# from itertools import islice
from ctypes import c_int32

# The following must follow the choice of the unicode003 test.
c_int = c_int32
HASH_SALT = 16777619 # 32 bits
# HASH_SALT = 1099511628211 # 64 bits

MIN_PYTHYON_VERSION = (3, 10)

if sys.version_info < MIN_PYTHYON_VERSION:
    sys.exit(
        r"Incompatible minimal Python version: "
        f"Expected {'.'.join(map(str, MIN_PYTHYON_VERSION))}+, "
        f"but go: {platform.python_version()}"
    )

PACKAGE_DIR = Path(__file__).parent.parent
UNICODE_VERSION = None

# Load target Unicode version
with (PACKAGE_DIR / "unicode_version").open("rt", encoding="utf-8") as fp:
    for line in fp:
        line = line.strip()
        if line and not line.startswith("#"):
            var, value = line.split("=")
            if var == "VERSION":
                UNICODE_VERSION = value.replace("\"", "")
                break

# Check compatible Unicode version
if unicodedata.unidata_version != UNICODE_VERSION:
    sys.exit(
        f"Expected a Python version supporting Unicode {UNICODE_VERSION}, " +
        f"but got: {unicodedata.unidata_version}."
    )

def check_unicode002(tests_path: Path):
    path = tests_path / "unicode002.stdout"
    print("Checking test output:", path)
    with path.open("rt", encoding="utf-8") as fp:
        # Skip header
        next(fp)
        for line in fp:
            raw_code, isControl, isPrint, isSpace, isUpper, isLower, isAlpha, isDigit = line.split()
            code = int(raw_code)
            char = chr(code)
            check(check_bool(is_control(char), isControl), show_entry(char, code, "isControl", isControl))
            check(check_bool(is_print(char)  , isPrint  ), show_entry(char, code, "isPrint"  , isPrint))
            check(check_bool(is_space(char)  , isSpace  ), show_entry(char, code, "isSpace"  , isSpace))
            check(check_bool(is_upper(char)  , isUpper  ), show_entry(char, code, "isUpper"  , isUpper))
            check(check_bool(is_lower(char)  , isLower  ), show_entry(char, code, "isLower"  , isLower))
            check(check_bool(is_alpha(char)  , isAlpha  ), show_entry(char, code, "isAlpha"  , isAlpha))
            check(check_bool(is_digit(char)  , isDigit  ), show_entry(char, code, "isDigit"  , isDigit))

def check_unicode003(tests_path: Path):
    path = tests_path / "unicode003.stdout"
    print("Checking test output:", path)
    with path.open("rt", encoding="utf-8") as fp:
        # Skip header
        next(fp)
        general_category_hashes = defaultdict(list)
        for line in fp:
            raw_code, raw_gc_hash_ref, raw_cm_hash_ref = line.strip().split(",")
            code = int(raw_code, 16)
            gc_hash_ref = int(raw_gc_hash_ref)
            cm_hash_ref = int(raw_cm_hash_ref)
            max_code = 0x10ffff
            chunk_size = 100
            char = chr(code)
            # gc_hash = reduce(
            #     general_category_hash_with_salt,
            #     map(chr, range(code, min(code + chunk_size, max_code + 1))),
            #     general_category_hash(char)
            # )
            gc_hash, cats = reduce(
                general_category_hash_check,
                map(chr, range(code, min(code + chunk_size, max_code + 1))),
                (general_category_hash(char), [])
            )
            general_category_hashes[gc_hash.value].append((raw_code, cats))
            # [FIXME] Restore cm_hash check when issue in case_mapping_hash is solved
            # cm_hash = reduce(
            #     case_mapping_hash_with_salt,
            #     map(chr, range(code, min(code + chunk_size, max_code + 1))),
            #     case_mapping_hash(char)
            # )
            check(
                gc_hash.value == gc_hash_ref, # and cm_hash.value == cm_hash_ref,
                show_entry(char, code, gc_hash_ref, gc_hash.value, cm_hash_ref) # , cm_hash.value)
            )
        # Check collisions
        for hash, collisions in general_category_hashes.items():
            if len(collisions) > 1 and any(x[1] != collisions[0][1] for x in collisions[1:]):
                print(f"[WARNING] Collisions of general category hashes ({hash}): {collisions}")

def check(r: bool, info):
    if r:
        return True
    else:
        raise AssertionError(info)

def check_bool(b1: bool, b2: str):
    if b1:
        return b2 == "T"
    else:
        return b2 == "F"

def show_unicode(c: str):
    return f"U+{ord(c):0<4x}"

def show_entry(c: str, *args):
    return (show_unicode(c), unicodedata.category(c), general_category(c)) + args

ALL_HASKELL_CATEGORIES = (
    "UppercaseLetter",
    "LowercaseLetter",
    "TitlecaseLetter",
    "ModifierLetter",
    "OtherLetter",
    "NonSpacingMark",
    "SpacingCombiningMark",
    "EnclosingMark",
    "DecimalNumber",
    "LetterNumber",
    "OtherNumber",
    "ConnectorPunctuation",
    "DashPunctuation",
    "OpenPunctuation",
    "ClosePunctuation",
    "InitialQuote",
    "FinalQuote",
    "OtherPunctuation",
    "MathSymbol",
    "CurrencySymbol",
    "ModifierSymbol",
    "OtherSymbol",
    "Space",
    "LineSeparator",
    "ParagraphSeparator",
    "Control",
    "Format",
    "Surrogate",
    "PrivateUse",
    "NotAssigned"
)

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

def general_category_hash(c: str):
    return c_int(ALL_HASKELL_CATEGORIES.index(general_category(c)))

def general_category_hash_with_salt(s: c_int, c: str):
    return hash_int(s, general_category_hash(c))

def general_category_hash_check(acc, c: str):
    s, cats = acc
    cat = general_category(c)
    return hash_int(s, c_int(ALL_HASKELL_CATEGORIES.index(cat))), [*cats, cat]

def case_mapping_hash(c: str):
    return hash_int(
        # [FIXME] Does not work because Python does not use simple case mappings:
        #         some transformations result in multiple characters.
        hash_int(
            c_int(ord(c.lower())),
            c_int(ord(c.upper()))
        ),
        c_int(ord(c.title()))
    )

def case_mapping_hash_with_salt(s: c_int, c: str):
    return hash_int(s, case_mapping_hash(c))

# def take(n, iterable):
#     return list(islice(iterable, n))

# def chunked(iterable, n):
#     return iter(partial(take, n, iter(iterable)), [])

def hash_int(s: c_int, x: c_int):
    return c_int(s.value * HASH_SALT ^ x.value)

def is_control(c: str):
    match(general_category(c)):
        case "Control":
            return True
        case _:
            return False

def is_print(c: str):
    match(general_category(c)):
        case "LineSeparator":
            return False
        case "ParagraphSeparator":
            return False
        case "Control":
            return False
        case "Format":
            return False
        case "Surrogate":
            return False
        case "PrivateUse":
            return False
        case "NotAssigned":
            return False
        case _:
            return True

def is_space(c: str):
    match(c):
        case '\t':
            return True
        case '\n':
            return True
        case '\v':
            return True
        case '\f':
            return True
        case '\r':
            return True
        case _:
            match(general_category(c)):
                case "Space":
                    return True
                case _:
                    return False

def is_upper(c: str):
    match(general_category(c)):
        case "UppercaseLetter":
            return True
        case "TitlecaseLetter":
            return True
        case _:
            return False

def is_lower(c: str):
    match(general_category(c)):
        case "LowercaseLetter":
            return True
        case _:
            return False

def is_alpha(c: str):
    match(general_category(c)):
        case "UppercaseLetter":
            return True
        case "LowercaseLetter":
            return True
        case "TitlecaseLetter":
            return True
        case "ModifierLetter":
            return True
        case "OtherLetter":
            return True
        case _:
            return False

def is_digit(c: str):
    return 0<= ord(c) - ord('0') <= 9


if __name__ == "__main__":
    TESTS_DIR = PACKAGE_DIR.parent.parent / "tests"
    check_unicode002(TESTS_DIR)
    check_unicode003(TESTS_DIR)