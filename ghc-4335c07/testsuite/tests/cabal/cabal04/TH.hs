{-# LANGUAGE TemplateHaskell #-}
module TH where

import Language.Haskell.TH

spliceMe = [| (\xs -> tail xs ++ init xs) |]
