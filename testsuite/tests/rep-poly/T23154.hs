{-# LANGUAGE PartialTypeSignatures #-}

module T23154 where

import GHC.Exts

f x = x :: (_ :: (TYPE (_ _)))
