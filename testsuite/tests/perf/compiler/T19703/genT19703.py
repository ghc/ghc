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

def gen_join():
    write_file('T19703.hs', dedent('''
        {-# LANGUAGE TypeFamilies #-}
        module Join where
        import A
        import B
    '''))

def gen_instances(mod_name: str, n: int):
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
            for i in range(n))
    write_file('{}.hs'.format(mod_name), out)

gen_fam()
gen_instances('A', n=5000)
gen_instances('B', n=10000)
gen_join()
