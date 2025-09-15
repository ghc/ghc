{-# LANGUAGE UnboxedTuples, MagicHash #-}
module T13509 where

import GHC.Exts
import Data.Word

selectPower :: Word# -> (# Word#, Word# #)
selectPower base = go base
  where
--    go :: Word# -> (# Word#, Word# #)
    go pw = case timesWord2# pw pw of
        (# 0##, pw2 #)
          -> let (# n, pw2n #) = go pw2 in
            case timesWord2# pw pw2n of
              (# 0##, pw2n1 #) -> (#n `timesWord#` 2## `plusWord#` 1##, pw2n1 #)
              _ -> (# n `timesWord#` 2##, pw2n #)
        _           -> (# 1##, pw #)
