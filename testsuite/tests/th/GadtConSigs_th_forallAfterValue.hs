{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module GadtConSigs_th_forallAfterValue where

import Language.Haskell.TH (Dec, Q)

decls :: Q [Dec]
decls = [d| data F a where
              MkF :: a -> forall b. b -> F a |]
