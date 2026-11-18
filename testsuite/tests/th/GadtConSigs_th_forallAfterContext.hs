{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module GadtConSigs_th_forallAfterContext where

import Language.Haskell.TH (Dec, Q)

decls :: Q [Dec]
decls = [d| data G a where
              G7 :: forall a. Eq a => forall b. Show b => (a, b) -> G a |]
