#!/usr/bin/env python
# -*- coding: utf-8 -*-

from pathlib import Path
import urllib.request
import subprocess
import argparse

TARBALL_VERSION = '0.1'
BASE_URL = "https://downloads.haskell.org/ghc/mingw/{}".format(TARBALL_VERSION)
BASE_URL = "http://home.smart-cactus.org/~ben/ghc/mingw/{}".format(TARBALL_VERSION)
DEST = Path('ghc-tarballs/mingw-w64')
ARCHS = ['i686', 'x86_64', 'sources']

def file_url(arch: str, fname: str) -> str:
    return "{base}/{arch}/{fname}".format(
        base=BASE_URL,
        arch=arch,
        fname=fname)

def fetch(url: str, dest: Path):
    print('Fetching', url, '=>', dest)
    urllib.request.urlretrieve(url, dest)

def fetch_arch(arch: str):
    req = urllib.request.urlopen(file_url(arch, 'MANIFEST'))
    files = req.read().decode('UTF-8').split('\n')
    d = DEST / arch
    if not d.is_dir():
        d.mkdir(parents=True)
    fetch(file_url(arch, 'SHA256SUMS'), d / 'SHA256SUMS')
    for fname in files:
        if not (d / fname).is_file():
            fetch(file_url(arch, fname), d / fname)

    subprocess.check_call(['sha256sum', '--check', '--ignore-missing', 'SHA256SUMS'],
                          cwd=d)

def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument('arch', help="Architecture to fetch (either i686, x86_64, sources, or all)")
    args = parser.parse_args()
    if args.arch == 'all':
        for arch in ARCHS:
            fetch_arch(arch)
    elif args.arch not in ARCHS:
        raise ArgumentError("Unknown architecture {}".format(args.arch))
    else:
        fetch_arch(args.arch)

if __name__ == '__main__':
    main()
