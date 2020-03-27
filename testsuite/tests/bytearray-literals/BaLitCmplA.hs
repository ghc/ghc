{-# language MagicHash #-}
{-# language QuasiQuotes #-}

module BaLitCmplA
  ( func
  ) where

import BaLitCmplTH (ascii)
import GHC.Exts

func :: ByteArray# -> Int#
func x = case x of
  [ascii|hello|] -> 42#
  _ -> 45#
