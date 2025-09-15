"""
Utilities for linters
"""

import os
import sys
import re
import textwrap
import subprocess
from pathlib import Path
from typing import List, Optional, Callable, Sequence
from collections import namedtuple

def lint_failure(file, line_no: int, line_content: str, message: str):
    """ Print a lint failure message. """
    wrapper = textwrap.TextWrapper(initial_indent='  ',
                                   subsequent_indent='    ')
    body = wrapper.fill(message)
    msg = '''
    {file}:

             |
      {line_no:5d}  |  {line_content}
             |

    {body}
    '''.format(file=file, line_no=line_no,
               line_content=line_content,
               body=body)

    print(textwrap.dedent(msg))

def get_changed_files(base_commit: str, head_commit: str,
                      subdir: str = '.'):
    """ Get the files changed by the given range of commits. """
    cmd = ['git', 'diff', '--name-only',
           base_commit, head_commit, '--', subdir]
    files = subprocess.check_output(cmd)
    return files.decode('UTF-8').split('\n')

def get_tracked_files(subdir: str = '.'):
    """ Get the files tracked by git in the given subdirectory. """
    if not Path(subdir).exists():
        raise Exception("Regex linter executed with nonexistent target directory '{}'".format(subdir))
    cmd = ['git', 'ls-tree', '--name-only', '-r', 'HEAD', subdir]
    files = subprocess.check_output(cmd)
    return files.decode('UTF-8').split('\n')

Warning = namedtuple('Warning', 'path,line_no,line_content,message')

class Linter(object):
    """
    A :class:`Linter` must implement :func:`lint`, which looks at the
    given path and calls :func:`add_warning` for any lint issues found.
    """
    def __init__(self):
        self.warnings = [] # type: List[Warning]
        self.path_filters = [] # type: List[Callable[[Path], bool]]

    def add_warning(self, w: Warning):
        self.warnings.append(w)

    def add_path_filter(self, f: Callable[[Path], bool]) -> "Linter":
        self.path_filters.append(f)
        return self

    def do_lint(self, path: Path):
        if all(f(path) for f in self.path_filters):
            self.lint(path)

    def lint(self, path: Path):
        raise NotImplementedError

class LineLinter(Linter):
    """
    A :class:`LineLinter` must implement :func:`lint_line`, which looks at
    the given line from a file and calls :func:`add_warning` for any lint
    issues found.
    """
    def lint(self, path: Path):
        if path.is_file():
            try:
                with path.open('r') as f:
                    for line_no, line in enumerate(f):
                        self.lint_line(path, line_no+1, line)
            # We don't want to explicitly exclude every single binary file in the test suite
            except UnicodeDecodeError as e:
                pass
            except Exception as e:
                print('Exception occurred while linting file: {}'.format(path))
                raise e

    def lint_line(self, path: Path, line_no: int, line: str):
        raise NotImplementedError

class RegexpLinter(LineLinter):
    """
    A :class:`RegexpLinter` produces the given warning message for
    all lines matching the given regular expression.
    """
    def __init__(self, regex: str, message: str):
        LineLinter.__init__(self)
        self.re = re.compile(regex)
        self.message = message

    def lint_line(self, path: Path, line_no: int, line: str):
        if self.re.search(line):
            w = Warning(path=path, line_no=line_no, line_content=line[:-1],
                        message=self.message)
            self.add_warning(w)

def run_linters(linters: Sequence[Linter],
                subdir: str = '.') -> None:
    import argparse
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers()

    subparser = subparsers.add_parser('commits', help='Lint a range of commits')
    subparser.add_argument('base', help='Base commit')
    subparser.add_argument('head', help='Head commit')
    subparser.set_defaults(get_linted_files=lambda args:
                            get_changed_files(args.base, args.head, subdir))

    subparser = subparsers.add_parser('files', help='Lint the given files')
    subparser.add_argument('file', nargs='+', help='File to lint')
    subparser.set_defaults(get_linted_files=lambda args: args.file)

    subparser = subparsers.add_parser('tracked', help="Lint files tracked by Git")
    subparser.set_defaults(get_linted_files=lambda args:
                            get_tracked_files(subdir))

    args = parser.parse_args()

    linted_files = args.get_linted_files(args)
    for path in linted_files:
        if path.startswith('testsuite/tests/linters'):
            continue
        for linter in linters:
            linter.do_lint(Path(path))

    warnings = [warning
                for linter in linters
                for warning in linter.warnings]
    warnings = sorted(warnings, key=lambda x: (x.path, x.line_no))
    for w in warnings:
        lint_failure(w.path, w.line_no, w.line_content, w.message)

    if len(warnings) > 0:
        sys.exit(1)
