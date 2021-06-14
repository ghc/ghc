{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module THSpliceType where

import Language.Haskell.TH

foo :: Q Exp
foo =
  let myType = [t| Int |]
    in [| id @ $myType |]
