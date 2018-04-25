#!/usr/bin/env python3
from typing import Dict, List, Set, NamedTuple

import os
import subprocess
import ast

import extra_files
extra_src_files = extra_files.extra_src_files # type: Dict[str, List[str]]

found_tests = set() # type: Set[str]
fixed_tests = set() # type: Set[str]

def extras(name: str) -> str:
    return 'extra_files(%s)' % (extra_src_files[name],)

def list_extras(name: str, col: int) -> str:
    return extras(name) + ',\n' + ' ' * (col + 1)

def find_all_T_files(basedir: bytes) -> List[bytes]:
    result = [] # type: List[bytes]
    for dirpath, dirnames, filenames in os.walk(basedir):
        for f in filenames:
            if f.endswith(b'.T'):
                result.append(os.path.join(dirpath, f))
    return result

# Delete del bytes from (line, col) and then insert the string ins there.
Fixup = NamedTuple('Fixup', [('line',   int),
                             ('col',    int),
                             ('delete', int),
                             ('insert', str)])

class TestVisitor(ast.NodeVisitor):
    def __init__(self) -> None:
        self.fixups = [] # type: List[Fixup]

    def visit_Call(self, node: ast.AST) -> None:
        self.generic_visit(node)
        assert isinstance(node, ast.Call)

        if isinstance(node.func, ast.Name) and node.func.id == 'test':
            assert(len(node.args) == 4)
            name_expr, setup, test_fn, args = node.args
            if not(isinstance(name_expr, ast.Str)):
                return
            name = name_expr.s
            if name in extra_src_files:
                found_tests.add(name)
                if isinstance(setup, ast.Name):
                    if setup.id == 'normal':
                        # Kill it
                        self.fixups.append(Fixup(
                            line=setup.lineno, col=setup.col_offset,
                            delete=len(setup.id), insert=extras(name)))
                    else:
                        # Make a lit
                        self.fixups.append(Fixup(
                            line=setup.lineno, col=setup.col_offset,
                            delete=0,
                            insert='[' + list_extras(name, setup.col_offset)))
                        self.fixups.append(Fixup(
                            line=setup.lineno,
                            col=setup.col_offset + len(setup.id),
                            delete=0, insert=']'))
                    fixed_tests.add(name)
                elif isinstance(setup, ast.List):
                    # Insert into list at start
                    if not setup.elts:
                        ins = extras(name) # no need for comma, newline
                        # Don't try to delete the list because someone
                        # might have written "[   ]" for some reason
                    else:
                        ins = list_extras(name, setup.col_offset)
                    self.fixups.append(Fixup(
                        line=setup.lineno, col=setup.col_offset + 1,
                        delete=0, insert=ins))
                    fixed_tests.add(name)
                else:
                    assert False # we fixed them all manually already

basedir = subprocess.check_output(['git', 'rev-parse', '--show-toplevel'])
basedir = basedir[0:-1]         # delete trailing newline
print(basedir)
for f in find_all_T_files(basedir):
    print(f)
    text = open(f).read()
    mod = ast.parse(text)
    tv = TestVisitor()
    tv.visit(mod)

    lines = text.split('\n')
    if not tv.fixups:
        # Don't rewrite files unnecessarily
        # (libraries/Win32 has Windows line endings)
        continue
    for fixup in reversed(tv.fixups):
        l = list(lines[fixup.line-1])
        l[fixup.col:fixup.col + fixup.delete] = fixup.insert
        lines[fixup.line-1] = ''.join(l)
    open(f, 'w').write('\n'.join(lines))
