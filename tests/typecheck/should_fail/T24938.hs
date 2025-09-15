{-# LANGUAGE TypeFamilyDependencies, PartialTypeSignatures #-}

module T24938 where

import Prelude (Int, String, undefined)

data Eq a b where
  Refl :: Eq a a

type family Mt a = r | r -> a

anyM :: Mt a
anyM = undefined

useIntAndRaise :: Mt Int -> a
useIntAndRaise = undefined

type family Nt a = r | r -> a

use :: Nt a -> a
use = undefined

anyN :: Nt a
anyN = undefined

foo p (e :: Eq (Mt Int) (Nt String)) =
  (case e of
    Refl ->
      let bar x =
            if p then useIntAndRaise x
            else use x
      in
        bar) anyM
