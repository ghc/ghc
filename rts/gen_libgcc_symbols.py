#!/usr/bin/env python3

import sys
import subprocess
import argparse
from typing import Set
from pathlib import Path

def list_symbols(nm: Path, lib: Path) -> Set[str]:
    out = subprocess.check_output([
        nm, '--format=posix', '--extern-only', '--defined-only', lib
    ], encoding='ASCII')
    syms = set()
    for l in out.split('\n'):
        parts = l.split(' ')
        if len(parts) == 4:
            syms.add(parts[0])

    return syms

def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument('libgcc', type=Path, help='path to libgcc')
    parser.add_argument('--nm', default='nm', type=Path, help='path to nm utility')
    parser.add_argument('-o', '--output', default=sys.stdout, type=argparse.FileType('w'), help='output file name')
    args = parser.parse_args()

    syms = list_symbols(args.nm, args.libgcc)
    lines = [ '#define RTS_LIBGCC_SYMBOLS' ]
    lines += [ f'    SymE_HasProto({sym})' for sym in sorted(syms) ]
    print(' \\\n'.join(lines), file=args.output)

if __name__ == '__main__':
    main()
