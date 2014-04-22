{-# LANGUAGE OverloadedRecordFields, DataKinds, MagicHash #-}

import GHC.Prim (Proxy#, proxy#)
import GHC.Records

data T = MkT { foo :: Int } | MkT2 { bar :: Bool }
  deriving Show

x = MkT 42

-- This should generate a suitable runtime error
main = print (setField (proxy# :: Proxy# "bar") x True)
