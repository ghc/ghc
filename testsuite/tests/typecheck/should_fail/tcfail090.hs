{-# LANGUAGE MagicHash #-}

module ShouldFail where

import GHC.Base

my_undefined :: a  -- This one has kind *, not OpenKind
my_undefined = undefined

die :: Int -> ByteArray#
die _ = my_undefined
