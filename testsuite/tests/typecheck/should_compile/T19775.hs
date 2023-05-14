{-# OPTIONS_GHC -Wno-deprecated-flags #-}
{-# LANGUAGE DatatypeContexts, TypeApplications #-}

module T19775 where

data Ord a => T a = MkT (Maybe a)

foo = MkT @Int
