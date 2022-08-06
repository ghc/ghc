#!/usr/bin/env python3

import glob
import os
import os.path
import sys
import argparse
from textwrap import dedent
import subprocess
import re
import shutil

cwd = os.getcwd()

parser = argparse.ArgumentParser()
parser.add_argument('--validate', action='store_true', help='Run in validate mode')
parser.add_argument('--hadrian', action='store_true', help='Do not assume the make base build system')
args = parser.parse_args()

# Packages whose libraries aren't in the submodule root
EXCEPTIONS = {
    'libraries/containers/': 'libraries/containers/containers/'
}

def print_err(s):
    print(dedent(s), file=sys.stderr)

def die(mesg):
    print_err(mesg)
    sys.exit(1)

def check_boot_packages():
    # Check that we have all boot packages.
    for l in open('packages', 'r'):
        if l.startswith('#'):
            continue

        parts = [part for part in l.split(' ') if part]
        if len(parts) != 4:
            die("Error: Bad line in packages file: " + l)

        dir_ = parts[0]
        tag = parts[1]

        # If tag is not "-" then it is an optional repository, so its
        # absence isn't an error.
        if tag == '-':
            # We would like to just check for a .git directory here,
            # but in an lndir tree we avoid making .git directories,
            # so it doesn't exist. We therefore require that every repo
            # has a LICENSE file instead.
            license_path = os.path.join(EXCEPTIONS.get(dir_+'/', dir_), 'LICENSE')
            if not os.path.isfile(license_path):
                die("""\
                    Error: %s doesn't exist
                    Maybe you haven't run 'git submodule update --init'?
                    """ % license_path)

def autoreconf():
    # Run autoreconf on everything that needs it.
    processes = {}
    if os.name == 'nt':
        # Get the normalized ACLOCAL_PATH for Windows
        # This is necessary since on Windows this will be a Windows
        # path, which autoreconf doesn't know doesn't know how to handle.
        ac_local = os.getenv('ACLOCAL_PATH', '')
        ac_local_arg = re.sub(r';', r':', ac_local)
        ac_local_arg = re.sub(r'\\', r'/', ac_local_arg)
        ac_local_arg = re.sub(r'(\w):/', r'/\1/', ac_local_arg)
        reconf_cmd = 'ACLOCAL_PATH=%s autoreconf' % ac_local_arg
    else:
        reconf_cmd = 'autoreconf'

    for dir_ in ['.'] + glob.glob('libraries/*/'):
        if os.path.isfile(os.path.join(dir_, 'configure.ac')):
            print("Booting %s" % dir_)
            # Update config.sub in submodules
            if dir_ != '.' and os.path.isfile(os.path.join(dir_, 'config.sub')):
                shutil.copyfile('config.sub', os.path.join(dir_, 'config.sub'))
            processes[dir_] = subprocess.Popen(['sh', '-c', reconf_cmd], cwd=dir_)

    # Wait for all child processes to finish.
    fail = False
    for k,v in processes.items():
        code = v.wait()
        if code != 0:
            print_err('autoreconf in %s failed with exit code %d' % (k, code))
            fail = True

    if fail:
        sys.exit(1)

check_boot_packages()
autoreconf()
