
{-# LANGUAGE NoDatatypeContexts #-}

module T3811e where

data (Show a, Read a) => D a = D a
