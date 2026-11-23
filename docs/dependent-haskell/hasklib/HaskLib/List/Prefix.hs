{-# LANGUAGE NoExperimentalDependentTypes #-}
{-# OPTIONS_GHC -fwrite-source-equations #-}

-- | A list prefix with a structural size bound.
module HaskLib.List.Prefix (takeSize) where

import PreludeMapLengthSize (Size(..))

-- | Take at most the given number of elements.
takeSize :: Size -> [a] -> [a]
takeSize Zero _ = []
takeSize (Succ _) [] = []
takeSize (Succ n) (x : xs) = x : takeSize n xs
