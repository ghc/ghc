{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module T5886a where

import Language.Haskell.TH hiding (Type)
import Data.Kind (Type)

class C α where
  type AT α ∷ Type

bang ∷ DecsQ
bang = return [InstanceD Nothing [] (AppT (ConT ''C) (ConT ''Int))
                [TySynInstD (TySynEqn Nothing (AppT (ConT ''AT) (ConT ''Int)) (ConT ''Int))]]
