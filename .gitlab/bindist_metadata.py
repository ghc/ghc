#!/usr/bin/env python3

import sys
import os
import shutil
import re
import ast
from pathlib import Path
import subprocess
import json
from typing import Dict, List, Set, Optional, NamedTuple

def run(args: List[str]) -> str:
    return subprocess.check_output(args, encoding='UTF-8')

def parse_hadrian_cfg(cfg: str) -> Dict[str,str]:
    res = {}
    for l in cfg.split('\n'):
        if l.startswith('#'):
            continue
        elif '=' in l:
            i = l.find('=')
            k = l[:i].strip()
            v = l[i+1:].strip()
            res[k] = v

    return res

def get_ghc_info(ghc: Path) -> Dict[str,str]:
    import ast
    out = run([ghc, '--info'])
    pairs = ast.literal_eval(out.strip())
    res = {}
    for k,v in pairs:
        if v == 'YES':
            v = True
        elif v == 'NO':
            v = False
        res[k] = v

    return res

def get_dynamic_deps(objfile: Path) -> Set[Path]:
    out = run(['ldd', objfile])
    return { Path(m.group(1)) for m in re.finditer('=> *([^ ]+)', out) }

def get_configure_cmdline() -> str:
    r = Path('config.log').read_text()
    m = re.search(r'  $ .+', r)
    return m

class Package(NamedTuple):
    name: str
    version: str

def find_providing_package(f: Path) -> Optional[Package]:
    if shutil.which('dpkg'):
        out = run(['dpkg-query', '--search', f]).strip()
        pkg,_file = out.split(':')

        out = run(['dpkg-query', '--show', pkg]).strip()
        _pkg,version = out.split()
        return Package(pkg, version)
    elif shutil.which('rpm'):
        out = run(['rpm', '-qf', f, '--queryformat=%{NAME} %{VERSION}\n']).strip()
        pkg,version = out.split()
        return Package(pkg, version)
    elif shutil.which('apk'):
        out = run(['apk', 'info', '--who-owns', f]).strip()
        pkg = re.find('is owned by ([.+])', out)

        # Determining the version of an installed package is far too difficult;
        # some day perhaps upstream will address
        # https://gitlab.alpinelinux.org/alpine/apk-tools/-/issues/10704
        db = Path('/lib/apk/db/installed').read_text()
        m = re.find(f'P:{pkg}\nV:([.+])\n', db)
        version = m.group(1)
        return Package(pkg, version)
    else:
        return None

def main() -> None:
    ghc = Path('_build/stage1/bin/ghc')
    ghc_pkg = Path('_build/stage1/bin/ghc-pkg')

    metadata = {}

    system_config = Path('.') / 'hadrian' / 'cfg' / 'system.config'
    cfg = parse_hadrian_cfg(system_config.read_text())

    ######
    # GHC build configuration
    ######
    metadata['ghc_version'] = cfg['project-version']
    metadata['git_commit_id'] = cfg['project-git-commit-id']
    metadata['tables_next_to_code'] = cfg['tables-next-to-code']
    metadata['unregisterised'] = cfg['ghc-unregisterised']
    metadata['build_triple'] = cfg['build-platform']
    metadata['host_triple'] = cfg['host-platform']
    metadata['target_triple'] = cfg['target-platform']
    metadata['build_flavour'] = os.environ.get('BUILD_FLAVOUR')
    metadata['configure_cmdline'] = get_configure_cmdline()

    opsys = cfg['build-os']

    ######
    # Information about the bootstrapping environment
    ######
    lsb_release = None
    if opsys == 'linux':
        lsb_release = run(['lsb_release'])

    metadata['bootstrap_environment'] = {
        'ghc': run([cfg['system-ghc'], '--version']).split('\n')[0],
        'cc': run([cfg['system-cc'], '--version']).split('\n')[0],
        'lsb_release': lsb_release,
    }

    ######
    # Information about the bootstrapping environment's packages
    ######
    metadata['dynamic_deps'] = None
    if opsys != 'darwin':
        dyn_deps = get_dynamic_deps(ghc)
        print(dyn_deps)
        deps = {
            dep.name: find_providing_package(dep)
            for dep in dyn_deps
            if not dep.is_relative_to(Path('.').resolve())
        }
        metadata['dynamic_deps'] = deps

    ######
    # The contents of the compiler's global package database
    ######
    def call_ghc_pkg(args: List[str]) -> str:
        return run([ghc_pkg, '--simple-output'] + args).strip()

    metadata['global_packages'] = {
        pkg: {
            'version': call_ghc_pkg(['field', pkg, 'version']),
            'extra-libraries': call_ghc_pkg(['field', pkg, 'extra-libraries']).split(),
        }
        for pkg in call_ghc_pkg(['list', '--names-only']).split()
    }

    ######
    # Information about the resulting compiler
    ######
    metadata['inplace_ghc_info'] = get_ghc_info(ghc)

    json.dump(metadata, sys.stdout, indent=2)

if __name__ == '__main__':
    main()
