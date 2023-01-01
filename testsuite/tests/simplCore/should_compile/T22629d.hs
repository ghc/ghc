module T22629d where

import Data.List.NonEmpty as NE

import T22629d_Lib

-- getNumbers should get a specialization and W/Wed here.
-- So we check specialise output for $s$wgetNumbers
{-# NOINLINE foo #-}
foo = NE.head getNumbers :: Int

