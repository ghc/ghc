#!/usr/bin/env python3
# -*- coding: utf-8 -*-

N_VARS = 100
N_BINDS = 50

tyvars = ' '.join('v{i}'.format(i=i) for i in range(N_VARS))

print('''
{{-# LANGUAGE PartialTypeSignatures #-}}
{{-# OPTIONS_GHC -Wno-partial-type-signatures #-}}

module T14766 where

newtype T {tyvars} = T ()

'''.format(tyvars=tyvars))

holes = ' '.join('_' for i in range(N_VARS))
for i in range(N_BINDS):
    print('v{i} :: T {holes}'.format(i=i, holes=holes))
    print('v{i} = T ()'.format(i=i))


