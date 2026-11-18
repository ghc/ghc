{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module GadtConSigs_th_contextAfterValue where

import Language.Haskell.TH (Dec, Q)

decls :: Q [Dec]
decls = [d| data F a where
              MkF :: Ord a => a -> Eq a => F a |]
