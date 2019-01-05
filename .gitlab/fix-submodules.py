#!/usr/bin/env python3

"""
Fix submodule upstream URLs. This ensures that CI builds of GHC forks
clone their submodules from its usual location. Otherwise users would need to
fork all submodules before their CI builds would succeed.
"""

from typing import List, Dict
from pathlib import Path
import re

x = open('.gitmodules').read()
x = re.sub(r"url *= *\.\.", "url = https://gitlab.haskell.org/ghc", x)
open('.gitmodules', 'w').write(x)

import subprocess

def get_configs(config_file: Path) -> Dict[str, str]:
    args = ['git', 'config', '-f', config_file.as_posix(), '--list']
    out = subprocess.check_output(args)
    configs = {}
    for line in out.decode('UTF-8').split('\n'):
        if '=' in line:
            k,v = line.split('=')
            configs[k] = v

    return configs

def set_config(config_file: Path, key: str, value: str) -> None:
    args = ['git', 'config', '-f', config_file.as_posix(), '--replace', key, value]
    subprocess.check_call(args)

upstreams = {
    'utils/haddock': 'https://github.com/haskell/haddock'
}

modules_config = Path('.gitmodules')

def main():
    for k,v in get_configs(modules_config).items():
        match = re.match('submodule\.(.+)\.url', k)
        if match is not None:
            submod = match.group(1)
            if submod in upstreams:
                url = upstreams[submod]
            else:
                url = re.sub('\.\.', 'https://gitlab.haskell.org/ghc', v)

            print('Using {submod} from {url}'.format(submod=submod, url=url))
            set_config(modules_config, k, url)

if __name__ == '__main__':
    main()
