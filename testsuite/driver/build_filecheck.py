#!/usr/bin/env python3
"""
Build LLVM's FileCheck from a pinned LLVM release, for the filecheck_stderr
and filecheck_stdout checks of the testsuite (see testlib.py).

    python3 testsuite/driver/build_filecheck.py [--prefix DIR] [--jobs N]
                                                [--tarball FILE] [--keep]

Downloads llvm-project-<version>.src.tar.xz (about 180 MB), verifies its
SHA-256 against the pinned value, unpacks only the parts FileCheck needs,
configures with CMake, builds the FileCheck target and installs
<prefix>/bin/FileCheck, recording the version in <prefix>/VERSION. The
default prefix, <repo>/_build/filecheck, is where the driver looks first.

Needs cmake, a C++17 compiler and ninja or make. Where the distribution
packages FileCheck (Debian: llvm-NN-tools) this script is unnecessary, the
driver also searches PATH. To update: change LLVM_VERSION and LLVM_SHA256
(the release's asset digest on GitHub) and rerun.
"""

import argparse
import hashlib
import os
import shutil
import subprocess
import sys
import tarfile
import urllib.request
from pathlib import Path

LLVM_VERSION = '23.1.5'
LLVM_SHA256 = 'fb50b1eac93ddeb3aa8dab4cfe324dff74f4a9c2bdee6160c9e36188f8d06a6e'
LLVM_URL = ('https://github.com/llvm/llvm-project/releases/download/'
            'llvmorg-{0}/llvm-project-{0}.src.tar.xz'.format(LLVM_VERSION))

# The tarball holds the whole monorepo. FileCheck is llvm/utils/FileCheck,
# llvm/lib/FileCheck and llvm/lib/Support, but CMake configures the whole
# llvm/ tree, so keep it minus the directories that CMAKE_OPTIONS turn off.
# Support's APFloat includes headers from libc/.
SRC_ROOT = 'llvm-project-{}.src'.format(LLVM_VERSION)
WANTED = ('cmake/', 'third-party/', 'llvm/', 'libc/')
UNWANTED = ('llvm/test/', 'llvm/unittests/', 'llvm/docs/', 'llvm/examples/',
            'llvm/bindings/', 'llvm/benchmarks/', 'libc/test/')
CMAKE_OPTIONS = [
    '-DCMAKE_BUILD_TYPE=Release',
    '-DLLVM_INCLUDE_TESTS=OFF',
    '-DLLVM_INCLUDE_BENCHMARKS=OFF',
    '-DLLVM_INCLUDE_EXAMPLES=OFF',
    '-DLLVM_INCLUDE_DOCS=OFF',
    '-DLLVM_ENABLE_BINDINGS=OFF',
    '-DLLVM_ENABLE_ZLIB=OFF',
    '-DLLVM_ENABLE_ZSTD=OFF',
    '-DLLVM_ENABLE_LIBXML2=OFF',
    '-DLLVM_ENABLE_LIBEDIT=OFF',
    '-DLLVM_ENABLE_LIBPFM=OFF',
]
EXE = 'FileCheck.exe' if os.name == 'nt' else 'FileCheck'


def log(msg: str) -> None:
    print('build_filecheck: ' + msg, file=sys.stderr, flush=True)


def run(cmd) -> None:
    log(' '.join(cmd))
    subprocess.run(cmd, check=True)


def sha256_of(path: Path) -> str:
    h = hashlib.sha256()
    with path.open('rb') as f:
        for chunk in iter(lambda: f.read(1 << 20), b''):
            h.update(chunk)
    return h.hexdigest()


def fetch_tarball(work: Path, given) -> Path:
    if given is not None:
        path = Path(given)
    else:
        path = work / os.path.basename(LLVM_URL)
        if not path.exists():
            log('downloading ' + LLVM_URL)
            with urllib.request.urlopen(LLVM_URL) as response, path.open('wb') as f:
                shutil.copyfileobj(response, f)
    actual = sha256_of(path)
    if actual != LLVM_SHA256:
        sys.exit('{}: SHA-256 {} does not match the pinned {}'
                 .format(path, actual, LLVM_SHA256))
    return path


def wanted(member: tarfile.TarInfo) -> bool:
    name = member.name
    parts = name.split('/')
    if parts[0] != SRC_ROOT or '..' in parts or name.startswith('/'):
        return False
    rel = name[len(SRC_ROOT) + 1:]
    return (member.isfile() or member.isdir() or member.issym()) \
        and rel.startswith(WANTED) and not rel.startswith(UNWANTED)


def unpack(tarball: Path, work: Path) -> Path:
    src = work / SRC_ROOT
    if (src / 'llvm' / 'CMakeLists.txt').exists():
        log('using the unpacked sources in ' + str(src))
        return src
    log('unpacking ' + str(tarball))
    kwargs = {'filter': 'data'} if hasattr(tarfile, 'data_filter') else {}
    with tarfile.open(str(tarball), 'r|xz') as tar:
        for member in tar:
            if wanted(member):
                tar.extract(member, str(work), **kwargs)
    return src


def build(src: Path, build_dir: Path, jobs: int) -> Path:
    cmake = shutil.which('cmake')
    if cmake is None:
        sys.exit('cmake not found')
    generator = ['-G', 'Ninja'] if shutil.which('ninja') else []
    run([cmake, '-S', str(src / 'llvm'), '-B', str(build_dir)]
        + generator + CMAKE_OPTIONS)
    run([cmake, '--build', str(build_dir), '--target', 'FileCheck',
         '--parallel', str(jobs)])
    return build_dir / 'bin' / EXE


def install(exe: Path, prefix: Path) -> Path:
    bin_dir = prefix / 'bin'
    bin_dir.mkdir(parents=True, exist_ok=True)
    installed = bin_dir / EXE
    shutil.copy2(str(exe), str(installed))
    (prefix / 'VERSION').write_text(LLVM_VERSION + '\n')
    return installed


def is_installed(prefix: Path) -> bool:
    version = prefix / 'VERSION'
    return (prefix / 'bin' / EXE).exists() and version.exists() \
        and version.read_text().strip() == LLVM_VERSION


def main() -> None:
    repo = Path(__file__).resolve().parent.parent.parent
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('--prefix', type=Path, default=repo / '_build' / 'filecheck',
                        help='install into PREFIX/bin (default: %(default)s)')
    parser.add_argument('--jobs', '-j', type=int, default=os.cpu_count() or 2,
                        help='parallel compile jobs (default: %(default)s)')
    parser.add_argument('--tarball', metavar='FILE',
                        help='use this llvm-project source tarball instead of downloading')
    parser.add_argument('--keep', action='store_true',
                        help='keep the sources and the build tree under PREFIX/work')
    args = parser.parse_args()

    prefix = args.prefix.resolve()
    if is_installed(prefix):
        log('FileCheck {} is already installed in {}'.format(LLVM_VERSION, prefix))
        return
    work = prefix / 'work'
    work.mkdir(parents=True, exist_ok=True)
    tarball = fetch_tarball(work, args.tarball)
    src = unpack(tarball, work)
    exe = build(src, work / 'build', args.jobs)
    installed = install(exe, prefix)
    if not args.keep:
        shutil.rmtree(str(work))
    log('installed ' + str(installed))


if __name__ == '__main__':
    main()
