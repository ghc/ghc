{-# LANGUAGE ImplicitParams, MagicHash #-}

module T21327 where

import GHC.Exts

foo () = (?p :: Int#)
