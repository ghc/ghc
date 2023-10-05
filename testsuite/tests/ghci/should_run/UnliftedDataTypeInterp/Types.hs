{-# LANGUAGE UnliftedDatatypes, StandaloneKindSignatures #-}
{-# OPTIONS_GHC -fobject-code #-}
module Types where

import GHC.Exts

type T :: UnliftedType
data T = T0_1
       | T1I Int
       | T0_2
       | T2BI Bool Int
       | T0_3
       | T3CIB Char Int Bool
       | T0_4
