#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
bootstrap.py - bootstrapping utility for hadrian

See bootstrap/README.md for usage instructions.
"""

USAGE = """
This utility is only intended for use in building hadrian
on a new platform. If you already have a functional cabal-install
please rather run `cabal build hadrian .`. or `./hadrian/build`
"""

from enum import Enum
import hashlib
import logging
import json
from pathlib import Path
import platform
import shutil
import subprocess
import tempfile
import sys
from textwrap import dedent
from typing import Set, Optional, Dict, List, Tuple, \
                   NewType, BinaryIO, NamedTuple, TypeVar

#logging.basicConfig(level=logging.INFO)

BUILDDIR    = Path('_build')

BINDIR      = BUILDDIR / 'bin'            # binaries go there (--bindir)
DISTDIR     = BUILDDIR / 'dists'          # --builddir
UNPACKED    = BUILDDIR / 'unpacked'       # where we unpack final package tarballs
TARBALLS    = BUILDDIR / 'tarballs'       # where we download tarballks
PSEUDOSTORE = BUILDDIR / 'pseudostore'    # where we install packages
ARTIFACTS   = BUILDDIR / 'artifacts'      # Where we put the archive
TMPDIR      = BUILDDIR / 'tmp'            #
PKG_DB      = BUILDDIR / 'packages.conf'  # package db

PackageName = NewType('PackageName', str)
Version = NewType('Version', str)
SHA256Hash = NewType('SHA256Hash', str)

class PackageSource(Enum):
    HACKAGE = 'hackage'
    LOCAL = 'local'

url = str

BuiltinDep = NamedTuple('BuiltinDep', [
    ('package', PackageName),
    ('version', Version),
])

BootstrapDep = NamedTuple('BootstrapDep', [
    ('package', PackageName),
    ('version', Version),
    ('source', PackageSource),
    # source tarball SHA256
    ('src_sha256', Optional[SHA256Hash]),
    # `revision` is only valid when source == HACKAGE.
    ('revision', Optional[int]),
    ('cabal_sha256', Optional[SHA256Hash]),
    ('flags', List[str]),
])

BootstrapInfo = NamedTuple('BootstrapInfo', [
    ('builtin', List[BuiltinDep]),
    ('dependencies', List[BootstrapDep]),
])

FetchInfo = NamedTuple('FetchInfo', [
    ('url', url),
    ('sha256', SHA256Hash)
])

FetchPlan = Dict[Path, FetchInfo]

class Compiler:
    def __init__(self, ghc_path: Path):
        if not ghc_path.is_file():
            print(f'GHC {ghc_path} is not a file')
            sys.exit(1)

        self.ghc_path = ghc_path.resolve()

        info = self._get_ghc_info()
        self.version = info['Project version']
        #self.lib_dir = Path(info['LibDir'])
        #self.ghc_pkg_path = (self.lib_dir / 'bin' / 'ghc-pkg').resolve()
        self.ghc_pkg_path = (self.ghc_path.parent / 'ghc-pkg').resolve()
        if not self.ghc_pkg_path.is_file():
            raise TypeError(f'ghc-pkg {self.ghc_pkg_path} is not a file')

    def _get_ghc_info(self) -> Dict[str,str]:
        from ast import literal_eval
        p = subprocess_run([self.ghc_path, '--info'], stdout=subprocess.PIPE, check=True, encoding='UTF-8')
        out = p.stdout.replace('\n', '').strip()
        return dict(literal_eval(out))

PackageSpec = Tuple[PackageName, Version]

class BadTarball(Exception):
    def __init__(self, path: Path, expected_sha256: SHA256Hash, found_sha256: SHA256Hash):
        self.path = path
        self.expected_sha256 = expected_sha256
        self.found_sha256 = found_sha256

    def __str__(self):
        return '\n'.join([
            f'Bad tarball hash: {str(self.path)}',
            f'  expected: {self.expected_sha256}',
            f'  found:    {self.found_sha256}',
        ])

def package_url(package: PackageName, version: Version) -> str:
    return f'http://hackage.haskell.org/package/{package}-{version}/{package}-{version}.tar.gz'

def package_cabal_url(package: PackageName, version: Version, revision: int) -> str:
    return f'http://hackage.haskell.org/package/{package}-{version}/revision/{revision}.cabal'

def verify_sha256(expected_hash: SHA256Hash, f: Path):
    print(f"Verifying {f}...")
    h = hash_file(hashlib.sha256(), f.open('rb'))
    if h != expected_hash:
        raise BadTarball(f, expected_hash, h)

def read_bootstrap_info(path: Path) -> BootstrapInfo:
    obj = json.load(path.open())

    def bi_from_json(o: dict) -> BuiltinDep:
        return BuiltinDep(**o)

    def dep_from_json(o: dict) -> BootstrapDep:
        o['source'] = PackageSource(o['source'])
        return BootstrapDep(**o)

    builtin = [bi_from_json(dep) for dep in obj['builtin'] ]
    deps = [dep_from_json(dep) for dep in obj['dependencies'] ]

    return BootstrapInfo(dependencies=deps, builtin=builtin)

def check_builtin(dep: BuiltinDep, ghc: Compiler) -> None:
    subprocess_run([str(ghc.ghc_pkg_path), 'describe', f'{dep.package}-{dep.version}'],
                   check=True, stdout=subprocess.DEVNULL)
    print(f'Using {dep.package}-{dep.version} from GHC...')
    return

def resolve_dep(dep : BootstrapDep) -> Path:
    if dep.source == PackageSource.HACKAGE:

        tarball = TARBALLS / f'{dep.package}-{dep.version}.tar.gz'
        verify_sha256(dep.src_sha256, tarball)

        cabal_file = TARBALLS / f'{dep.package}.cabal'
        verify_sha256(dep.cabal_sha256, cabal_file)

        UNPACKED.mkdir(parents=True, exist_ok=True)
        shutil.unpack_archive(tarball.resolve(), UNPACKED, 'gztar')

        sdist_dir = UNPACKED / f'{dep.package}-{dep.version}'

        # Update cabal file with revision
        if dep.revision is not None:
            shutil.copyfile(cabal_file, sdist_dir / f'{dep.package}.cabal')

    elif dep.source == PackageSource.LOCAL:
        if dep.package == 'hadrian':
            sdist_dir = Path(sys.path[0]).parent.resolve()
        else:
            raise ValueError(f'Unknown local package {dep.package}')
    return sdist_dir


def install_dep(dep: BootstrapDep, ghc: Compiler) -> None:
    dist_dir = (DISTDIR / f'{dep.package}-{dep.version}').resolve()

    sdist_dir = resolve_dep(dep)

    install_sdist(dist_dir, sdist_dir, ghc, dep.flags)

def install_sdist(dist_dir: Path, sdist_dir: Path, ghc: Compiler, flags: List[str]):
    prefix = PSEUDOSTORE.resolve()
    flags_option = ' '.join(flags)
    setup_dist_dir = dist_dir / 'setup'
    setup = setup_dist_dir / 'Setup'

    build_args = [
        f'--builddir={dist_dir}',
    ]

    configure_args = build_args + [
        f'--package-db={PKG_DB.resolve()}',
        f'--prefix={prefix}',
        f'--bindir={BINDIR.resolve()}',
        f'--with-compiler={ghc.ghc_path}',
        f'--with-hc-pkg={ghc.ghc_pkg_path}',
        f'--flags={flags_option}',
    ]

    def check_call(args: List[str]) -> None:
        subprocess_run(args, cwd=sdist_dir, check=True)

    setup_dist_dir.mkdir(parents=True, exist_ok=True)


    setup_file = sdist_dir / 'Setup.hs'
    if not setup_file.exists():
        with open(setup_file, 'w') as setup_out:
            setup_out.write('import Distribution.Simple\nmain = defaultMain')

    # Note: we pass -i so GHC doesn't look for anything else
    # This should be fine for cabal-install dependencies.
    check_call([str(ghc.ghc_path), '--make', '-package-env=-', '-i', f'-odir={setup_dist_dir}', f'-hidir={setup_dist_dir}', '-o', setup, 'Setup'])
    check_call([setup, 'configure'] + configure_args)
    check_call([setup, 'build'] + build_args)
    check_call([setup, 'install'] + build_args)

def hash_file(h, f: BinaryIO) -> SHA256Hash:
    while True:
        d = f.read(1024)
        if len(d) == 0:
            return SHA256Hash(h.hexdigest())

        h.update(d)


# Cabal plan.json representation
UnitId = NewType('UnitId', str)
PlanUnit = NewType('PlanUnit', dict)

def bootstrap(info: BootstrapInfo, ghc: Compiler) -> None:
    if not PKG_DB.exists():
        print(f'Creating package database {PKG_DB}')
        PKG_DB.parent.mkdir(parents=True, exist_ok=True)
        subprocess_run([ghc.ghc_pkg_path, 'init', PKG_DB])

    for dep in info.builtin:
        check_builtin(dep, ghc)

    for dep in info.dependencies:
        install_dep(dep, ghc)

# Steps
#######################################################################

def linuxname(i, r):
  i = i.strip() # id
  r = r.strip() # release
  if i == '': return 'linux'
  else: return f"{i}-{r}".lower()

def macname(macver):
  # https://en.wikipedia.org/wiki/MacOS_version_history#Releases
  if macver.startswith('10.12.'): return 'sierra'
  if macver.startswith('10.13.'): return 'high-sierra'
  if macver.startswith('10.14.'): return 'mojave'
  if macver.startswith('10.15.'): return 'catalina'
  if macver.startswith('11.0.'): return 'big-sur'
  else: return macver

def archive_name(version):
    # Ask platform information
    machine = platform.machine()
    if machine == '': machine = "unknown"

    system = platform.system().lower()
    if system == '': system = "unknown"

    version = system
    if system == 'linux':
        try:
            i = subprocess_run(['lsb_release', '-si'], stdout=subprocess.PIPE, encoding='UTF-8')
            r = subprocess_run(['lsb_release', '-sr'], stdout=subprocess.PIPE, encoding='UTF-8')
            version = linuxname(i.stdout, r.stdout)
        except:
            try:
                with open('/etc/alpine-release') as f:
                    alpinever = f.read().strip()
                    return f'alpine-{alpinever}'
            except:
                pass
    elif system == 'darwin':
        version = 'darwin-' + macname(platform.mac_ver()[0])
    elif system == 'freebsd':
        version = 'freebsd-' + platform.release().lower()

    return f'hadrian-{version}-{machine}-{version}'

def make_archive(hadrian_path):

    print(f'Creating distribution tarball')

    # Get bootstrapped hadrian version
    # This also acts as smoke test
    p = subprocess_run([hadrian_path, '--numeric-version'], stdout=subprocess.PIPE, check=True, encoding='UTF-8')
    cabalversion = p.stdout.replace('\n', '').strip()

    # Archive name
    basename = ARTIFACTS.resolve() / (archive_name(cabalversion) + '-bootstrapped')

    # In temporary directory, create a directory which we will archive
    tmpdir = TMPDIR.resolve()
    tmpdir.mkdir(parents=True, exist_ok=True)

    rootdir = Path(tempfile.mkdtemp(dir=tmpdir))
    shutil.copy(hadrian_path, rootdir / 'hadrian')

    # Make archive...
    fmt = 'xztar'
    if platform.system() == 'Windows': fmt = 'zip'
    archivename = shutil.make_archive(basename, fmt, rootdir)

    return archivename

def fetch_from_plan(plan : FetchPlan, output_dir : Path):
  import urllib.request

  output_dir.resolve()
  output_dir.mkdir(parents=True, exist_ok=True)

  for path in plan:
    output_path = output_dir / path
    url = plan[path].url
    sha = plan[path].sha256
    if not output_path.exists():
      print(f'Fetching {url}...')
      with urllib.request.urlopen(url) as resp:
        shutil.copyfileobj(resp, output_path.open('wb'))
    verify_sha256(sha, output_path)

def gen_fetch_plan(info : BootstrapInfo) -> FetchPlan :
    sources_dict = {}
    for dep in info.dependencies:
      if dep.package != 'hadrian':
        sources_dict[f"{dep.package}-{dep.version}.tar.gz"] = FetchInfo(package_url(dep.package, dep.version), dep.src_sha256)
        if dep.revision is not None:
          sources_dict[f"{dep.package}.cabal"] = FetchInfo(package_cabal_url(dep.package, dep.version, dep.revision), dep.cabal_sha256)
    return sources_dict

def find_ghc(compiler) -> Compiler:
  # Find compiler
  if compiler is None:
      path = shutil.which('ghc')
      if path is None:
          raise ValueError("Couldn't find ghc in PATH")
      ghc = Compiler(Path(path))
  else:
      ghc = Compiler(compiler)
  return ghc


def main() -> None:
    import argparse
    parser = argparse.ArgumentParser(
        description="bootstrapping utility for hadrian.",
        epilog = USAGE,
        formatter_class = argparse.RawDescriptionHelpFormatter)
    parser.add_argument('-d', '--deps', type=Path, help='bootstrap dependency file (plan-bootstrap.json)')
    parser.add_argument('-w', '--with-compiler', type=Path,
                        help='path to GHC')
    parser.add_argument('-s', '--bootstrap-sources', type=Path,
                        help='Path to prefetched bootstrap sources tarball')

    subparsers = parser.add_subparsers(dest="command")

    parser_list = subparsers.add_parser('list-sources', help='list all sources required to download')
    parser_list.add_argument('-o','--output', type=Path, default='fetch_plan.json')

    parser_fetch = subparsers.add_parser('fetch', help='fetch all required sources from hackage (for offline builds)')
    parser_fetch.add_argument('-o','--output', type=Path, default='bootstrap-sources')
    parser_fetch.add_argument('-p','--fetch-plan', type=Path, default=None, help="A json document that lists the urls required for download (optional)")

    args = parser.parse_args()

    ghc = None

    if args.deps is None:
      if args.bootstrap_sources is None:
        # find appropriate plan in the same directory as the script
        ghc = find_ghc(args.with_compiler)
        args.deps = Path(sys.path[0]) / f"plan-bootstrap-{ghc.version.replace('.','_')}.json"
        print(f"defaulting bootstrap plan to {args.deps}")
      # We have a tarball with all the required information, unpack it and use for further 
      elif args.bootstrap_sources is not None and args.command != 'list-sources':
        print(f'Unpacking {args.bootstrap_sources} to {TARBALLS}')
        shutil.unpack_archive(args.bootstrap_sources.resolve(), TARBALLS, 'gztar')
        args.deps = TARBALLS / 'plan-bootstrap.json'
        print(f"using plan-bootstrap.json ({args.deps}) from {args.bootstrap_sources}")
      else:
        print("We need a bootstrap plan (plan-bootstrap.json) or a tarball containing bootstrap information")
        print("Perhaps pick an appropriate one from: ")
        for child in Path(sys.path[0]).iterdir:
          if child.match('plan-bootstrap-*.json'):
            print(child)
        sys.exit(1)
    info = read_bootstrap_info(args.deps)

    print(dedent("""
        DO NOT use this script if you have another recent cabal-install available.
        This script is intended only for bootstrapping hadrian on new
        architectures.
    """))


    if(args.command == 'fetch'):
        if args.fetch_plan is not None:
          plan = { path : FetchInfo(p["url"],p["sha256"]) for path, p in json.load(args.fetch_plan.open()).items() }
        else:
          plan = gen_fetch_plan(info)

        # In temporary directory, create a directory which we will archive
        tmpdir = TMPDIR.resolve()
        tmpdir.mkdir(parents=True, exist_ok=True)
 
        rootdir = Path(tempfile.mkdtemp(dir=tmpdir))
 
        fetch_from_plan(plan, rootdir)

        shutil.copyfile(args.deps, rootdir / 'plan-bootstrap.json')

        fmt = 'gztar'
        if platform.system() == 'Windows': fmt = 'zip'
 
        archivename = shutil.make_archive(args.output, fmt, root_dir=rootdir)

        print(f'Bootstrap sources saved to {archivename}')
        print(f'Use `bootstrap.py -d {args.deps} -s {archivename}` to continue')

    elif(args.command == 'list-sources'):
        plan = gen_fetch_plan(info)
        with open(args.output, 'w') as out:
          json.dump({path : val._asdict() for path,val in plan.items()}, out)
        print(f"Required hackage sources saved to {args.output}")
        tarfmt= "\n./"
        print(f"""
Download the files listed in {args.output} and save them to a tarball ($TARBALL), along with {args.deps}
The contents of $TARBALL should look like:

./
./plan-bootstrap.json
./{tarfmt.join(path for path in plan)}

Then use `bootstrap.py -s $TARBALL` to continue
Alternatively, you could use `bootstrap.py -d {args.deps} fetch -o $TARBALL` to download and generate the tarball, skipping this step
""")

    elif(args.command == None):
        if ghc is None:
          ghc = find_ghc(args.with_compiler)

        print(f'Bootstrapping hadrian with GHC {ghc.version} at {ghc.ghc_path}...')

        if args.bootstrap_sources is None:
          plan = gen_fetch_plan(info)
          fetch_from_plan(plan, TARBALLS)

        bootstrap(info, ghc)
        hadrian_path = (BINDIR / 'hadrian').resolve()

        archive = make_archive(hadrian_path)

        print(dedent(f'''
            Bootstrapping finished!

            The resulting hadrian executable can be found at

                {hadrian_path}

            It have been archived for distribution in

                {archive}

            You can use this executable to build GHC.
        '''))
    else:
      print(f"No such command: {args.command}")

def subprocess_run(args, **kwargs):
    "Like subprocess.run, but also print what we run"

    args_str = ' '.join(map(str, args))
    extras = ''
    if 'cwd' in kwargs:
        extras += f' cwd={kwargs["cwd"]}'
    print(f'bootstrap: running{extras} {args_str}')

    return subprocess.run(args, **kwargs)

if __name__ == '__main__':
    main()
