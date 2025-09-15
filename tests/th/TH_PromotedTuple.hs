{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module TH_PromotedTuple where

import Language.Haskell.TH

data Equal a b where
  Refl :: Equal a a

equal :: Equal '(Int, False) $(do ty <- [t| '(Int, False) |]
                                  reportWarning (show ty)
                                  return ty)

equal = Refl
