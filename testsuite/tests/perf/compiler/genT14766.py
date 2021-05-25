#!/usr/bin/env python3
# -*- coding: utf-8 -*-

N_VARS = 100
N_BINDS = 50

tyvars = ' '.join(f'v{i}' for i in range(N_VARS))

print(f'''
{{-# LANGUAGE PartialTypeSignatures #-}}
{{-# OPTIONS_GHC -Wno-partial-type-signatures #-}}

module T14766 where

newtype T {tyvars} = T ()

''')

holes = ' '.join('_' for i in range(N_VARS))
for i in range(N_BINDS):
    print(f'v{i} :: T {holes}')
    print(f'v{i} = T ()')


