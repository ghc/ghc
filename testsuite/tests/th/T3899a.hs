{-# LANGUAGE TemplateHaskell #-}
module T3899a where


import Language.Haskell.TH
import Control.Monad

data Cons a b = Cons a b
data Nil = Nil

nestedTuple n = do
     xs <- replicateM n (newName "x")
     return $ LamE [foldr (\v prev -> ConP 'Cons [VarP v,prev]) (ConP 'Nil []) xs]
                   (TupE $ map VarE xs)
