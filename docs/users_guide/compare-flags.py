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
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('--ghc', type=argparse.FileType('r'),
                        help='path of GHC executable')
    parser.add_argument('--doc-flags', type=argparse.FileType(mode='r', encoding='UTF-8'),
                        help='path of ghc-flags.txt output from Sphinx')
    args = parser.parse_args()

    doc_flags = read_documented_flags(args.doc_flags)
    ghc_flags = read_ghc_flags(args.ghc.name)

    failed = False

    undocumented = ghc_flags - doc_flags
    if len(undocumented) > 0:
        error('Found {len_undoc} flags not documented in the users guide:'.format(len_undoc=len(undocumented)), )
        error('\n'.join('  {}'.format(flag) for flag in sorted(undocumented)))
        error('')
        failed = True

    now_documented = EXPECTED_UNDOCUMENTED.intersection(doc_flags)
    if len(now_documented) > 0:
        error('Found flags that are documented yet listed in {}:'.format(EXPECTED_UNDOCUMENTED_PATH))
        error('\n'.join('  {}'.format(flag) for flag in sorted(now_documented)))
        error('')
        failed = True

    if failed:
        sys.exit(1)


if __name__ == '__main__':
    main()
