{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module T5886a where

import Language.Haskell.TH

class C α where
  type AT α ∷ ★

bang ∷ DecsQ
bang = return [InstanceD [] (AppT (ConT ''C) (ConT ''Int))
                [TySynInstD ''AT [TySynEqn [ConT ''Int] (ConT ''Int)]]]
