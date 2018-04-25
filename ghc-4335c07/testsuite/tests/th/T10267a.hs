{-# LANGUAGE TemplateHaskell #-}

module T10267a where

import Language.Haskell.TH

varX :: Q Exp
varX = [| x |]
