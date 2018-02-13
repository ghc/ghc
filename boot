#!/usr/bin/env python3

import glob
import os
import os.path
import sys
import argparse
from textwrap import dedent
import subprocess
import re

cwd = os.getcwd()

parser = argparse.ArgumentParser()
parser.add_argument('--validate', action='store_true', help='Run in validate mode')
parser.add_argument('--required-tag', type=str, action='append', default=set())
parser.add_argument('--hadrian', action='store_true', help='Do not assume the make base build system')
args = parser.parse_args()

def print_err(s):
    print(dedent(s), file=sys.stderr)

def die(mesg):
    print_err(mesg)
    sys.exit(1)

def check_for_url_rewrites():
    if os.path.isdir('.git') and \
       subprocess.check_output('git config remote.origin.url'.split()).find(b'github.com') != -1 and \
       subprocess.call(['git', 'config', '--get-regexp', '^url.*github.com/.*/packages-.insteadOf']) != 0:
        # If we cloned from github, make sure the url rewrites are set.
        # Otherwise 'git submodule update --init' prints confusing errors.
        die("""\
            It seems you cloned this repository from GitHub. But your git config files
            don't contain the url rewrites that are needed to make this work (GitHub
            doesn't support '/' in repository names, so we use a different naming scheme
            for the submodule repositories there).

            Please run the following commands first:

              git config --global url."git://github.com/ghc/packages-".insteadOf     git://github.com/ghc/packages/
              git config --global url."http://github.com/ghc/packages-".insteadOf    http://github.com/ghc/packages/
              git config --global url."https://github.com/ghc/packages-".insteadOf   https://github.com/ghc/packages/
              git config --global url."ssh://git\@github.com/ghc/packages-".insteadOf ssh://git\@github.com/ghc/packages/
              git config --global url."git\@github.com:/ghc/packages-".insteadOf      git\@github.com:/ghc/packages/

            And then:

              git submodule update --init
              ./boot

            Or start over, and clone the GHC repository from the haskell server:

              git clone --recursive git://git.haskell.org/ghc.git

            For more information, see:
              * https://ghc.haskell.org/trac/ghc/wiki/Newcomers or
              * https://ghc.haskell.org/trac/ghc/wiki/Building/GettingTheSources#CloningfromGitHub
        """)

def check_boot_packages():
    # Check that we have all boot packages.
    import re
    for l in open('packages', 'r'):
        if l.startswith('#'):
            continue

        parts = l.split(' ')
        if len(parts) != 4:
            die("Error: Bad line in packages file: " + l)

        dir_ = parts[0]
        tag = parts[1]

        # If $tag is not "-" then it is an optional repository, so its
        # absence isn't an error.
        if tag in args.required_tag:
            # We would like to just check for a .git directory here,
            # but in an lndir tree we avoid making .git directories,
            # so it doesn't exist. We therefore require that every repo
            # has a LICENSE file instead.
            license_path = os.path.join(dir_, 'LICENSE')
            if not os.path.isfile(license_path):
                die("""\
                    Error: %s doesn't exist" % license_path)
                    Maybe you haven't run 'git submodule update --init'?
                    """)

# Create libraries/*/{ghc.mk,GNUmakefile}
def boot_pkgs():
    library_dirs = []

    for package in glob.glob("libraries/*/"):
        packages_file = os.path.join(package, 'ghc-packages')
        if os.path.isfile(packages_file):
            for subpkg in open(packages_file, 'r'):
                library_dirs.append(os.path.join(package, subpkg.strip()))
        else:
            library_dirs.append(package)

    for package in library_dirs:
        if package[-1] == '/':
            # drop trailing '/'
            package = package[:-1]

        dir_ = os.path.relpath(package, 'libraries')
        cabals = glob.glob(os.path.join(package, '*.cabal.in'))
        if len(cabals) == 0:
            cabals = glob.glob(os.path.join(package, '*.cabal'))

        if len(cabals) > 1:
            die('Too many .cabal files in %s' % package)
        elif len(cabals) == 1:
            cabal = cabals[0]

            if os.path.isfile(cabal):
                # strip both .cabal and .in
                pkg = os.path.splitext(os.path.splitext(os.path.basename(cabal))[0])[0]
                top = package

                ghc_mk = os.path.join(package, 'ghc.mk')
                print('Creating %s' % ghc_mk)
                with open(ghc_mk, 'w') as f:
                    f.write(dedent(
                        """\
                        {package}_PACKAGE = {pkg}
                        {package}_dist-install_GROUP = libraries
                        $(if $(filter {dir},$(PACKAGES_STAGE0)),$(eval $(call build-package,{package},dist-boot,0)))
                        $(if $(filter {dir},$(PACKAGES_STAGE1)),$(eval $(call build-package,{package},dist-install,1)))
                        $(if $(filter {dir},$(PACKAGES_STAGE2)),$(eval $(call build-package,{package},dist-install,2)))
                        """.format(package = package,
                                pkg = pkg,
                                dir = dir_)))


def autoreconf():
    # Run autoreconf on everything that needs it.
    processes = {}
    if os.name == 'nt':
        # Get the normalized ACLOCAL_PATH for Windows
        # This is necessary since on Windows this will be a Windows
        # path, which autoreconf doesn't know doesn't know how to handle.
        ac_local = os.environ['ACLOCAL_PATH']
        ac_local_arg = re.sub(r';', r':', ac_local)
        ac_local_arg = re.sub(r'\\', r'/', ac_local_arg)
        ac_local_arg = re.sub(r'(\w):/', r'/\1/', ac_local_arg)
        reconf_cmd = 'ACLOCAL_PATH=%s autoreconf' % ac_local_arg
    else:
        reconf_cmd = 'autoreconf'

    for dir_ in ['.'] + glob.glob('libraries/*/'):
        if os.path.isfile(os.path.join(dir_, 'configure.ac')):
            print("Booting %s" % dir_)
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

def check_build_mk():
    if not args.validate and not os.path.isfile("mk/build.mk"):
        print(dedent(
            """
            WARNING: You don't have a mk/build.mk file.

            By default a standard GHC build will be done, which uses optimisation
            and builds the profiling libraries. This will take a long time, so may
            not be what you want if you are developing GHC or the libraries, rather
            than simply building it to use it.

            For information on creating a mk/build.mk file, please see:
                http://ghc.haskell.org/trac/ghc/wiki/Building/Using#Buildconfiguration
            """))

check_for_url_rewrites()
if not args.hadrian:
    boot_pkgs()
autoreconf()
if not args.hadrian:
    check_build_mk()
