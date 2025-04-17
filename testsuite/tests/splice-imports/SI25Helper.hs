{-# LANGUAGE TemplateHaskell #-}
module SI25Helper where

import Language.Haskell.TH

genCode :: String -> Q Exp
genCode s = [| s ++ " from helper" |]

-- Function that will be used in an inner splice
nestedCode :: String -> Q Exp
nestedCode s = [| genCode s |]
