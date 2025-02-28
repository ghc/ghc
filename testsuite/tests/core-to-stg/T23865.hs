{-# LANGUAGE TypeFamilies, GADTs #-}
module T23865 where

import Data.Kind

data Checked
data Unchecked
type Result :: Type -> Type
data family Result check
data instance Result Checked = CheckedResult
newtype instance Result Unchecked = UncheckedResult (() -> ())

class Checking (check :: Type) where
   switchCheck :: CheckSingleton check

data CheckSingleton check where
   Checked :: CheckSingleton Checked
   Unchecked :: CheckSingleton Unchecked

k :: forall check. Checking check => check -> ()
k _ = let w :: CheckSingleton check
          w = switchCheck

          f :: Result check
          f = case w of
                 Checked -> CheckedResult
                 Unchecked -> UncheckedResult id
         in case (w, f) of
               (Unchecked, UncheckedResult xf) -> xf ()
               (_, _) -> ()
