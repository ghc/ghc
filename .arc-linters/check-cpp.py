#!/usr/bin/env python3

# A linter to warn for ASSERT macros which are separated from their argument
# list by a space, which Clang's CPP barfs on

import sys
import logging
import os
import re
import json

def setup_logging(logger):
    """
    ``arc lint`` makes it quite tricky to catch debug output from linters.
    Log to a file to work around this.
    """
    hdlr = logging.FileHandler('linter.log', 'w')
    logger.addHandler(hdlr)
    logger.setLevel(logging.DEBUG)
    return logger

logger = logging.getLogger()
#setup_logging(logger)
logger.debug(sys.argv)

def add_warning(severity, message, line):
    entry = {
        'severity': severity,
        'message': message,
        'line': line
    }
    warnings.append(entry)

class Linter(object):
    def __init__(self):
        self.warnings = []

    def add_warning(self, **entry):
        self.warnings.append(entry)

    def lint(self, path):
        pass

class LineLinter(Linter):
    def lint(self, path):
        if os.path.isfile(path):
            with open(path, 'rb') as f:
                for lineno, line in enumerate(f):
                    self.lint_line(lineno+1, line)

    def lint_line(self, lineno, line):
        pass

class RegexpLinter(LineLinter):
    def __init__(self, regex, **warning):
        LineLinter.__init__(self)
        self.re = re.compile(regex)
        self.warning = warning

    def lint_line(self, lineno, line):
        if self.re.search(line):
            warning = {
                'line': lineno,
            }
            warning.update(self.warning)
            self.add_warning(**warning)

linters = [
    RegexpLinter(br'ASSERT\s+\(',
                 message='CPP macros should not have a space between the macro name and their argument list'),
    RegexpLinter(br'#ifdef\s+',
                 message='`#if defined(x)` is preferred to `#ifdef x`',
                 severity='warning'),
    RegexpLinter(br'#if\s+defined\s+',
                 message='`#if defined(x)` is preferred to `#if defined x`',
                 severity='warning'),
    RegexpLinter(br'#ifndef\s+',
                 message='`#if !defined(x)` is preferred to `#ifndef x`',
                 severity='warning'),
]

if __name__ == '__main__':
    path = sys.argv[1]
    for linter in linters:
        linter.lint(path)

    warnings = [warning
                for linter in linters
                for warning in linter.warnings]
    logger.debug(warnings)
    print(json.dumps(warnings))
