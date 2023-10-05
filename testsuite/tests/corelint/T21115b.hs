{-# LANGUAGE LambdaCase, MagicHash, TemplateHaskell #-}

module T21115b where

import GHC.Exts (Double#, Int#)
import Language.Haskell.TH.Syntax

foo :: Double# -> Int#
foo =
  $( return $ LamCaseE
      [ Match (LitP $ DoublePrimL 0.0) (NormalB $ LitE $ IntPrimL 2) []
      , Match (LitP $ DoublePrimL 2.0) (NormalB $ LitE $ IntPrimL 3) []
      , Match WildP                    (NormalB $ LitE $ IntPrimL 5) []
      ]
   )
