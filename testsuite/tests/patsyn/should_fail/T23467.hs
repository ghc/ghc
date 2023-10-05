{-# LANGUAGE PatternSynonyms #-}

module T23467 where

data ConData = ConData { _pars :: Int }
data Decl = ConDecl ConData

pattern Con :: Decl  -- The correct type would be  Int -> Decl
pattern Con { pars } = ConDecl (ConData pars)

foo :: Decl -> Int
foo (Con { pars }) = pars
