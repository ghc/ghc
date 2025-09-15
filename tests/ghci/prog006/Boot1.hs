{-# LANGUAGE Haskell2010 #-}
module Boot where

import A

data Data = forall n. Class n => D n
