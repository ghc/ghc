#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Linter to verify that all flags reported by GHC's --show-options mode
are documented in the user's guide.
"""

import sys
import subprocess
from typing import Set
from pathlib import Path

# A list of known-undocumented flags. This should be considered to be a to-do
# list of flags that need to be documented.
EXPECTED_UNDOCUMENTED_PATH = \
    Path(__file__).parent / 'expected-undocumented-flags.txt'

EXPECTED_UNDOCUMENTED = \
    {line for line in EXPECTED_UNDOCUMENTED_PATH.read_text().split()}

def expected_undocumented(flag: str) -> bool:
    if flag in EXPECTED_UNDOCUMENTED:
        return True
    if flag.startswith('-Werror'):
        return True
    if flag.startswith('-Wno-') \
            or flag.startswith('-dno') \
            or flag.startswith('-fno') \
            or flag.startswith('-XNo'):
        return True
    if flag.startswith('-Wwarn=') \
            or flag.startswith('-Wno-warn='):
        return True

    return False

def read_documented_flags(doc_flags) -> Set[str]:
    # Map characters that mark the end of a flag
    # to whitespace.
    trans = str.maketrans({
        '=': ' ',
        '[': ' ',
        '⟨': ' ',
    })
    return {line.translate(trans).split()[0]
            for line in doc_flags.read().split('\n')
            if line != ''}

def read_ghc_flags(ghc_path: str) -> Set[str]:
    ghc_output = subprocess.check_output([ghc_path, '--show-options'])
    return {flag
            for flag in ghc_output.decode('UTF-8').split('\n')
            if not expected_undocumented(flag)
            if flag != ''}

def error(s: str):
    print(s, file=sys.stderr)

def main() -> None:
    pass


if __name__ == '__main__':
    main()
