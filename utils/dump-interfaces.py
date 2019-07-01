#!/usr/bin/env python

"""
This is a handy utility for comparing the interfaces exposed by the core
library included in GHC's global package database. Given a compiler and a set
of packages it will produce a directory containing dumps of the :browse output
from each of the packages' exposed modules. These directories can be compared
with, e.g., meld with a reasonable number of false differences.
"""

from pathlib import Path
import subprocess
from typing import TextIO, Set
import re

CORE_PACKAGES = [
    "base",
    "ghc-prim",
    "template-haskell",
    "ghc-boot",
    "ghc-boot-th"
]

exposed_modules_re = re.compile('exposed-modules:\s*((?:(?:[A-Z][A-Za-z0-9_]*\.)*(?:[A-Z][A-Za-z0-9_]*)\s*)*)')

def dump_module(out: TextIO, ghc: Path, mod: str):
    print(f'  Dumping {mod}...')
    subprocess.run([ghc, '--interactive', '-dppr-cols=9999', '-v0'],
                   input=f':bro {mod}',
                   stdout=out,
                   encoding='UTF-8',
                   check=True)

def dump_package(out_dir: Path, ghc: Path, pkg: str):
    pkg_out = out_dir / pkg
    pkg_out.mkdir(exist_ok=True, parents=True)

    modules = get_modules(ghc, pkg)
    print(f'Dumping {len(modules)} exposed modules from {pkg}...')
    for mod in modules:
        mod_out = pkg_out / f"{mod}.txt"
        dump_module(mod_out.open('w'), ghc, mod)

def get_modules(ghc: Path, pkg: str) -> Set[str]:
    ghc_pkg = ghc.parent / "ghc-pkg"
    out = subprocess.check_output([ghc_pkg, 'describe', pkg], encoding='UTF-8')
    m = exposed_modules_re.search(out)
    return set(m.group(1).split())

def main() -> None:
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('-c', '--compiler', type=Path, required=True,
                        help='GHC executable')
    parser.add_argument('-o', '--output', type=Path, default=Path('interfaces'),
                        help='Output directory')
    parser.add_argument('package', nargs='*', help='Packages to dump')
    args = parser.parse_args()

    packages = args.package
    if packages == []:
        packages = CORE_PACKAGES

    for pkg in packages:
        dump_package(args.output, args.compiler, pkg)

if __name__ == "__main__":
    main()
