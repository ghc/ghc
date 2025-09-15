{-# LANGUAGE DatatypeContexts, TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecated-flags #-}

module T19775 where

data Ord a => T a = MkT (Maybe a)

foo = MkT @Int
