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
    write_file(f'{mod_name}.hs', dedent(f'''
            {{-# LANGUAGE TypeFamilies #-}}
            module {mod_name} where
            import Fam
        ''') + '\n'.join(
            dedent(f'''
            data T{i} = T{i}
            type instance Fam T{i} = Int
            ''')
            for i in range(n)
        ))

gen_fam()
gen_instances('A', n=5000)
gen_instances('B', n=10000)
gen_join()
