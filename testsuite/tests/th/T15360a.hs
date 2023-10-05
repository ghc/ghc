{-# LANGUAGE TemplateHaskell #-}
module T15360a where

import Language.Haskell.TH

data T a b c = Mk a b c

bar :: $( return $ AppT (InfixT (ConT ''Int) ''T (ConT ''Bool)) (ConT ''Double) )
bar = Mk 5 True 3.14

baz :: $( return $ AppT (ParensT (ConT ''Maybe)) (ConT ''Int) )
baz = Just 5
