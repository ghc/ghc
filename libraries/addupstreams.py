#!/usr/bin/env python3

from pathlib import Path
import subprocess
import logging
import re

logging.basicConfig(level=logging.INFO)

IGNORE = 1 # ignore submodule
GITHUB_HASKELL = 2 # in the haskell github org
ORIGIN = 3 # upstream remote == origin remote

def github(owner: str, name: str) -> str:
    return f'https://github.com/{owner}/{name}'

def github_haskell(name: str) -> str:
    return github('haskell', name)

def gitlab_ssh(owner: str, name: str) -> str:
    return f'git@gitlab.haskell.org:{owner}/{name}'

upstreams = {
    '.arc-linters/arcanist-external-json-linter': IGNORE,
    'libffi-tarballs': IGNORE,
    'libraries/array': GITHUB_HASKELL,
    'libraries/binary': GITHUB_HASKELL,
    'libraries/bytestring': GITHUB_HASKELL,
    'libraries/Cabal': GITHUB_HASKELL,
    'libraries/containers': GITHUB_HASKELL,
    'libraries/deepseq': GITHUB_HASKELL,
    'libraries/directory': GITHUB_HASKELL,
    'libraries/file-io': GITHUB_HASKELL,
    'libraries/filepath': GITHUB_HASKELL,
    'libraries/ghc-bignum/gmp/gmp-tarballs': ORIGIN,
    'libraries/ghc-internal/gmp/gmp-tarballs': ORIGIN,
    'libraries/haskeline': GITHUB_HASKELL,
    'libraries/hpc': ORIGIN,
    'libraries/integer-gmp/gmp/gmp-tarballs': ORIGIN,
    'libraries/mtl': GITHUB_HASKELL,
    'libraries/os-string': GITHUB_HASKELL,
    'libraries/parallel': GITHUB_HASKELL,
    'libraries/parsec': GITHUB_HASKELL,
    'libraries/pretty': GITHUB_HASKELL,
    'libraries/primitive': GITHUB_HASKELL,
    'libraries/process': GITHUB_HASKELL,
    'libraries/semaphore-compat': ORIGIN,
    'libraries/stm': GITHUB_HASKELL,
    'libraries/terminfo': GITHUB_HASKELL,
    'libraries/text': GITHUB_HASKELL,
    'libraries/time': GITHUB_HASKELL,
    'libraries/transformers': IGNORE, # darcs mirror
    'libraries/unix': GITHUB_HASKELL,
    'libraries/Win32': GITHUB_HASKELL,
    'nofib': 'https://gitlab.haskell.org/ghc/nofib',
    'utils/hpc': GITHUB_HASKELL,
    'utils/haddock': GITHUB_HASKELL,
}

all_submods = [
    line.split()[1]
    for line in subprocess.check_output(['git', 'submodule'], encoding='UTF-8').split('\n')
    if len(line.split()) > 0
]

packages = {
    line.split()[0]: line.split()[3]
    for line in open('packages').read().split('\n')
    if not line.startswith('#')
    if len(line.split()) == 4
    if line.split()[3] != '-'
}

def get_remote_url(submod: str, remote: str):
    p = subprocess.run(['git', '-C', submod, 'remote', 'get-url', remote],
                       encoding='UTF-8',
                       stdout=subprocess.PIPE,
                       stderr=subprocess.DEVNULL)
    if p.returncode == 0:
        return p.stdout
    else:
        return None

def add_remote(submod: str, remote: str, url: str):
    old_url = get_remote_url(submod, remote)
    if old_url is None:
        logging.info(f'{submod}: adding remote {remote} = {url}')
        subprocess.call(['git', '-C', submod, 'remote', 'add', remote, url])
    elif old_url == url:
        return
    else:
        logging.info(f'{submod}: updating remote {remote} = {url}')
        subprocess.call(['git', '-C', submod, 'remote', 'set-url', remote, url])

    #update_remote(submod, remote)

def update_remote(submod: str, remote: str):
    subprocess.check_call(['git', '-C', submod, 'remote', 'update', remote])

def main():
    for submod in all_submods:
        print(submod)
        upstream = None
        if submod in upstreams:
            upstream = upstreams[submod]
        elif submod in packages:
            upstream = packages[submod]

        if upstream == ORIGIN:
            upstream = subprocess.check_output(['git', '-C', submod, 'remote', 'get-url', 'origin'], encoding='UTF-8').strip()
        elif upstream == GITHUB_HASKELL:
            upstream = github_haskell(Path(submod).name)
        elif upstream == IGNORE:
            continue

        if upstream is None:
            print(f'Unknown upstream for {submod}')
            raise ValueError('unknown upstream')
        else:
            print(f'Upstream of {submod} is {upstream}')
            add_remote(submod, 'upstream', upstream)

        origin = get_remote_url(submod, 'origin')
        m = re.match('https://gitlab.haskell.org/(.*)', origin)
        if m is not None:
            push = f'git@gitlab.haskell.org:{m.group(1)}'
            print(f'origin-push of {submod} is {push}')
            add_remote(submod, 'origin-push', push)

        name = Path(submod).name
        add_remote(submod, 'teo', f'git@github.com:TeofilC/{name}')

if __name__ == '__main__':
    main()

