import os
import subprocess
import shutil
import tempfile
from pathlib import Path, PurePath
from term_color import Color, colored

from my_typing import *


PassFail = NamedTuple('PassFail',
                      [('passed', bool),
                       ('reason', str),
                       ('tag', Optional[str]),
                       ('stderr', Optional[str]),
                       ('stdout', Optional[str]),
                       ('hc_opts', Optional[str]),
                       ])

def badResult(result: PassFail) -> bool:
    return not result.passed

def passed(hc_opts=None) -> PassFail:
    return PassFail(passed=True,
                    reason='',
                    tag=None,
                    stderr=None,
                    stdout=None,
                    hc_opts=hc_opts)

def failBecause(reason: str,
                tag: Optional[str]=None,
                stderr: Optional[str]=None,
                stdout: Optional[str]=None
                ) -> PassFail:
    return PassFail(passed=False, reason=reason, tag=tag,
                    stderr=stderr, stdout=stdout, hc_opts=None)

def strip_quotes(s: str) -> str:
    # Don't wrap commands to subprocess.call/Popen in quotes.
    return s.strip('\'"')

def str_warn(s: str) -> str:
    return colored(Color.YELLOW, s)

def str_info(s: str) -> str:
    return colored(Color.BLUE, s)

def getStdout(cmd_and_args: List[str]):
    # Can't use subprocess.check_output, since we also verify that
    # no stderr was produced
    cp = subprocess.run([strip_quotes(cmd_and_args[0])] + cmd_and_args[1:], capture_output=True)
    r = cp.returncode
    stdout = cp.stdout
    stderr = cp.stderr
    if r != 0:
        raise Exception("Command failed: " + str(cmd_and_args))
    if stderr:
        raise Exception("stderr from command: %s\nStdOut(%s):\n%r\n%s\nOutput(%s):\n%r\n%s\n" % (cmd_and_args,str(len(stdout)), stdout, stdout.decode('utf-8'), str(len(stderr)), stderr, stderr.decode('utf-8')))
    return stdout.decode('utf-8')

def lndir(srcdir: Path, dstdir: Path, force_copy=False):
    # Create symlinks for all files in src directory.
    # Not all developers might have lndir installed.
    # os.system('lndir -silent {0} {1}'.format(srcdir, dstdir))
    for filename in srcdir.iterdir():
        base = filename.relative_to(srcdir)
        src = srcdir / base
        dst = dstdir / base
        if src.is_file():
            link_or_copy_file(src, dst, force_copy)
        else:
            dst.mkdir()
            lndir(src, dst)

# All possible test metric strings.
def testing_metrics():
    return { 'bytes allocated', 'peak_megabytes_allocated', 'max_bytes_used' }

# All performance counters we consider to be stable enough in CI to
# test for.
def stable_perf_counters():
    return { 'instructions:u' }

# Metrics which are testing residency information
def residency_testing_metrics():
    return { 'peak_megabytes_allocated', 'max_bytes_used' }

# On Windows, os.symlink is not defined with Python 2.7, but is in Python 3
# when using msys2, as GHC does. Unfortunately, only Administrative users have
# the privileges necessary to create symbolic links by default. Consequently we
# are forced to just copy instead.
#
# We define the following function to make this magic more
# explicit/discoverable. You are encouraged to use it instead of os.symlink.
def symlinks_work() -> bool:
    if os.getenv('FORCE_SYMLINKS') is not None:
        return True
    elif os.name == 'nt':
        # On Windows we try to create a symlink to test whether symlinks are
        # usable.
        works = False
        with tempfile.NamedTemporaryFile() as tmp:
            try:
                tmp.write(b'hello')
                os.symlink(tmp.name, '__symlink-test')
                works = True
            except OSError as e:
                print('Saw {} during symlink test; assuming symlinks do not work.'.format(e))
            finally:
                try:
                    os.unlink('__symlink-test')
                except:
                    pass

        return works
    else:
        return True

if not symlinks_work():
    def link_or_copy_file(src: Path, dst: Path, force_copy=False):
        shutil.copyfile(str(src), str(dst))
        shutil.copymode(str(src), str(dst))
else:
    def link_or_copy_file(src: Path, dst: Path, force_copy=False):
        if force_copy:
            shutil.copyfile(str(src), str(dst))
            shutil.copymode(str(src), str(dst))
        else:
            os.symlink(str(src), str(dst))

def memoize(f):
    """
    A decorator to memoize a nullary function.
    """
    def cached():
        if cached._cache is None:
            cached._cache = f()

        return cached._cache

    cached._cache = None
    return cached

# Print the matrix data in a tabular format.
def print_table(header_rows: List[List[str]], data_rows: List[List[str]], padding=2) -> None:
    # Calculate column widths then print each row.
    colWidths = [(0 if idx == 0 else padding) + max([len(cell) for cell in col])
                 for (idx, col) in enumerate(zip(*(header_rows + data_rows)))]
    col_fmts = ['{:>' + str(w) + '}' for w in colWidths]

    def printCols(cols):
        for row in cols:
            print(''.join([f.format(cell) for (f,cell) in zip(col_fmts, row)]))

    printCols(header_rows)
    print('-' * sum(colWidths))
    printCols(data_rows)

def shorten_metric_name(name: str) -> str:
    dic = {
        "runtime/bytes allocated": "run/alloc",
        "runtime/peak_megabytes_allocated": "run/peak",
        "runtime/max_bytes_used": "run/max",
        "compile_time/bytes allocated": "ghc/alloc",
        "compile_time/peak_megabytes_allocated": "ghc/peak",
        "compile_time/max_bytes_used": "ghc/max",
        "binary_size/if_compression(1)": "size/1",
        "binary_size/if_compression(2)": "size/2",
        "binary_size/if_compression(3)": "size/3",
    }
    return dic.get(name, name)
