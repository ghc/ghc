#!/usr/bin/env python3

# A linter to warn for ASSERT macros which are separated from their argument
# list by a space, which Clang's CPP barfs on

from pathlib import Path
from linter import run_linters, Linter, Warning

from typing import List, Tuple
import re

INCLUDE_RE = re.compile('# *include ([<"][^">]+[>"])')

def get_includes(file: Path) -> List[Tuple[int, str]]:
    txt = file.read_text()
    return [ (line_no+1, m.group(1) )
             for (line_no, line) in enumerate(txt.split('\n'))
             for m in [INCLUDE_RE.match(line)]
             if m is not None
             if m.group(1) != "rts/PosixSource.h"]

def in_rts_dir(path: Path) -> bool:
    return len(path.parts) > 0 and path.parts[0] == 'rts'

class RtsHIncludeOrderLinter(Linter):
    """
    Verify that "PosixSource.h" is always the first #include in source files to
    ensure __USE_MINGW_ANSI_STDIO is defined before system headers are
    #include'd.
    """
    def __init__(self):
        Linter.__init__(self)
        self.add_path_filter(in_rts_dir)
        self.add_path_filter(lambda path: path.suffix == '.c')

    def lint(self, path: Path):
        # We do allow a few small headers to precede Rts.h
        ALLOWED_HEADERS = {
            '"ghcconfig.h"',
            '"ghcplatform.h"',
        }

        includes = get_includes(path)
        headers = [x[1] for x in includes]
        lines = path.read_text().split('\n')

        if '"PosixSource.h"' in headers:
            for line_no, header in includes:
                if header == '"PosixSource.h"':
                    break
                elif header in ALLOWED_HEADERS:
                    continue

                self.add_warning(Warning(
                    path=path,
                    line_no=line_no,
                    line_content=lines[line_no-1],
                    message="PosixSource.h must be first header included in each file"))

class PrivateIncludeLinter(Linter):
    """
    Verify that system headers are not #include'd in <BeginPrivate.h> blocks as this
    can result in very hard-to-diagnose linking errors due to hidden library functions.
    """
    def __init__(self):
        Linter.__init__(self)
        self.add_path_filter(in_rts_dir)
        self.add_path_filter(lambda path: path.suffix == '.h')

    def lint(self, path: Path):
        private = False
        lines = path.read_text().split('\n')
        for line_no, include in get_includes(path):
            if include == '"BeginPrivate.h"':
                private = True
            elif include == '"EndPrivate.h"':
                private = False
            elif private:
                self.add_warning(Warning(
                    path=path,
                    line_no=line_no,
                    line_content=lines[line_no-1],
                    message='System header %s found inside of <BeginPrivate.h> block' % include))

linters = [
    RtsHIncludeOrderLinter(),
    PrivateIncludeLinter(),
]

if __name__ == '__main__':
    run_linters(linters)
