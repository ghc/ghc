
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -Werror #-}

-- Trac #2806

module Foo where

import GHC.Base

foo :: Int
foo = 3
    where (I# _x) = 4

