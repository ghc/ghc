{-# LANGUAGE PatternSynonyms, GADTs #-}
{-# OPTIONS_GHC -fwarn-missing-pattern-synonym-signatures #-}

{-
Test the printing of pattern synonym types (pprPatSynType)
We test all valid combinations of:
    universal type variables      yes/no
    "required" context            yes/no
    existential type variables    yes/no
    "provided" context            yes/no
-}

module T11213 where

data Ex         where MkEx       :: a -> Ex
data ExProv     where MkExProv   :: (Show a) => a -> ExProv
data UnivProv a where MkUnivProv :: (Show a) => a -> UnivProv a

pattern P         <-  True
pattern Pe    x   <-  MkEx x
pattern Pu    x   <-  x
pattern Pue   x y <- (x, MkEx y)
pattern Pur   x   <-  [x, 1]
pattern Purp  x y <- ([x, 1], MkUnivProv y)
pattern Pure  x y <- ([x, 1], MkEx y)
pattern Purep x y <- ([x, 1], MkExProv y)
pattern Pep   x   <-  MkExProv x
pattern Pup   x   <-  MkUnivProv x
pattern Puep  x y <- (MkExProv x, y)
