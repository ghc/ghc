{-# LANGUAGE RebindableSyntax, NPlusKPatterns #-}

module RepPolyNPlusK where

import Prelude
  ( Integer, Bool, undefined )
import GHC.Exts

fromInteger :: forall rep (a :: TYPE rep). Integer -> a
fromInteger = undefined

(+) :: forall rep (a :: TYPE rep). a -> a -> a
(+) = undefined

(-) :: forall rep (a :: TYPE rep). a -> a -> a
(-) = undefined

(>=) :: forall rep (a :: TYPE rep). a -> a -> Bool
(>=) = undefined

foo :: forall rep1 (a :: TYPE rep1). a -> ()
foo (bndr_a+2) = ()
