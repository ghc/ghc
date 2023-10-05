{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module Main where

class Show a => Thing a b
instance Show a => Thing a b

class (Show a => Thing a ()) => PseudoShow a b
instance PseudoShow a b

pseudoShow :: PseudoShow a () => a -> String
pseudoShow = show


{-
    [G] PseudoShow a ()
(and hence by SC)
    [G] Show a => Thing a ()
(and hence by SC)
    [G] Show a => Show a
The latter loops
-}

