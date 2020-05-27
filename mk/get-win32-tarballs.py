#!/usr/bin/env python
# -*- coding: utf-8 -*-

from pathlib import Path
import urllib.request
import subprocess
import argparse
from sys import stderr

TARBALL_VERSION = '0.2'
BASE_URL = "https://downloads.haskell.org/ghc/mingw/{}".format(TARBALL_VERSION)
DEST = Path('ghc-tarballs/mingw-w64')
ARCHS = ['i686', 'x86_64', 'sources']

def file_url(arch: str, fname: str) -> str:
    return "{base}/{arch}/{fname}".format(
        base=BASE_URL,
        arch=arch,
        fname=fname)

def fetch(url: str, dest: Path):
    print('Fetching', url, '=>', dest, file=stderr)
    urllib.request.urlretrieve(url, dest)

def fetch_arch(arch: str):
    manifest_url = file_url(arch, 'MANIFEST')
    print('Fetching', manifest_url, file=stderr)
    req = urllib.request.urlopen(manifest_url)
    files = req.read().decode('UTF-8').split('\n')
    d = DEST / arch
    if not d.is_dir():
        d.mkdir(parents=True)
    fetch(file_url(arch, 'SHA256SUMS'), d / 'SHA256SUMS')
    for fname in files:
        if not (d / fname).is_file():
            fetch(file_url(arch, fname), d / fname)

    verify(arch)

def verify(arch: str):
    if not Path(DEST / arch / "SHA256SUMS").is_file():
        raise IOError("SHA256SUMS doesn't exist; have you fetched?")

    cmd = ['sha256sum', '--quiet', '--check', '--ignore-missing', 'SHA256SUMS']
    subprocess.check_call(cmd, cwd=DEST / arch)

def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument('mode', choices=['verify', 'download'])
    parser.add_argument(
        'arch',
        choices=ARCHS + ['all'],
        help="Architecture to fetch (either i686, x86_64, sources, or all)")
    args = parser.parse_args()

    action = fetch_arch if args.mode == 'download' else verify
    if args.arch == 'all':
        for arch in ARCHS:
            action(arch)
    else:
        action(args.arch)

if __name__ == '__main__':
    main()
