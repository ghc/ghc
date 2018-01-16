{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

class C a
instance C Int

type D a = C a

$(return [])

main = print $(
  do isCInst <- isInstance ''C [ConT ''Int]
     isDInst <- isInstance ''D [ConT ''Int]
     lift (isCInst,isDInst))
