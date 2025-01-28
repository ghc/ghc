{-# LANGUAGE TemplateHaskell #-}

module T13776 where

import Language.Haskell.TH

spliceTy1 :: $(conT ''(,) `appT` conT ''Int `appT` conT ''Int)
spliceTy1 = (1,2)

spliceTy2 :: $(conT ''[] `appT` conT ''Int)
spliceTy2 = []

spliceTy3 :: $(conT ''(->)) [Int] Int
spliceTy3 = sum

spliceExp1 :: (Int, Int)
spliceExp1 = $(conE '(,) `appE` litE (integerL 1) `appE` litE (integerL 1))

spliceExp2 :: [Int]
spliceExp2 = $(conE '[])

splicePat1 :: (Int, Int) -> ()
splicePat1 $(conP '(,) [litP (integerL 1), litP (integerL 1)]) = ()

splicePat2 :: [Int] -> ()
splicePat2 $(conP '[] []) = ()
