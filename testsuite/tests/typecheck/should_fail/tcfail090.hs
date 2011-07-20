{-# LANGUAGE MagicHash #-}

module ShouldFail where

import GHC.Base

die :: Int -> ByteArray#
die _ = undefined
