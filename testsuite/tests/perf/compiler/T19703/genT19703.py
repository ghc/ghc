#!/usr/bin/env python
# -*- coding: utf-8 -*-

from textwrap import dedent

def write_file(fname: str, contents: str):
    print(contents, file=open(fname, 'w'))

def gen_fam():
    write_file('Fam.hs', dedent('''
        {-# LANGUAGE TypeFamilies #-}
        module Fam where
        type family Fam a
    '''))

def gen_join(n: int):
    write_file('T19703.hs', dedent('''
        {-# LANGUAGE TypeFamilies #-}
        module Join where
    ''') + "\n".join(f"import M{n}" for n in range(n)))

def gen_instances(mod_name: str, n_instances: int):
    out = dedent('''
            {{-# LANGUAGE TypeFamilies #-}}
            module {mod_name} where
            import Fam
        '''.format(mod_name=mod_name))
    out += '\n'.join(
            dedent('''
            data T{i} = T{i}
            type instance Fam T{i} = Int
            ''').format(i=str(i))
            for i in range(n_instances))
    write_file('{}.hs'.format(mod_name), out)

gen_fam()
n = 1000
for i in range(n):
    gen_instances(f'M{i}', n_instances=1)

gen_join(n)
