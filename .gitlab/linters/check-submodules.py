"""
Ensure all submodules point to released (tagged) commits.
"""

from subprocess import run, PIPE
import re

def get_submodules():
    p = run(['git', 'submodule'], stdout=PIPE)
    for match in re.finditer('^.([0-9a-f]{40}) ([^ ]+) \(([^\)]+)\)',
                             p.stdout.decode('UTF-8'), re.M):
        commit = match.group(1)
        submodule = match.group(2)
        rev = match.group(3)
        yield (commit, submodule, rev)

checked_submodules = {
    'libraries/array',
    'libraries/binary',
    'libraries/bytestring',
    'libraries/Cabal',
    'libraries/containers',
    'libraries/deepseq',
    'libraries/directory',
    'libraries/filepath',
    'libraries/haskeline',
    'libraries/hpc',
    'libraries/mtl',
    'libraries/parsec',
    'libraries/pretty',
    'libraries/process',
    'libraries/stm',
    'libraries/text',
    'libraries/terminfo',
    'libraries/time',
    'libraries/transformers',
    'libraries/unix',
    'libraries/Win32'
}

def main():
    fail = False
    for (commit, submodule, rev) in get_submodules():
        if submodule in check_submodules:
            if re.match('.*-g[0-9a-f]+', rev):
                fail = True
                print('Error: Submodule {submodule} points to non-released commit {commit} (rev={rev})'.format(
                    commit=commit, submodule=submodule, rev=rev))

    if fail:
        sys.exit(1)

if __name__ == "__main__":
    main()
