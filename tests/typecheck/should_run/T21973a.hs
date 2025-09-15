{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Main (main) where

import Data.Kind
import GHC.Exts

class (Monoid (Share a), Eq (Share a)) => ClassDecode a where
  type Share a :: Type
  decoderWithShare :: Share a -> Decoder a

class (Eq (Currency e), ClassDecode (Tx e)) => ClassLedger e where
  type Currency e :: Type
  type Tx e :: Type

newtype Decoder a = Decoder (String -> a)

{-# NOINLINE decode #-}
decode :: ClassDecode a => String -> a
decode str =
  case decoderWithShare mempty of
    Decoder f -> f str

data MyLedger c

newtype MyTx c = MyTx
  { currency :: c
  } deriving (Show, Read)

instance (Eq c) => ClassLedger (MyLedger c) where
  type Currency (MyLedger c) = c
  type Tx (MyLedger c) = MyTx c

instance (Eq [c], ClassLedger (MyLedger c)) => ClassDecode (MyTx c) where
  type Share (MyTx c) = [c]
  {-# NOINLINE decoderWithShare #-}
  decoderWithShare :: [c] -> Decoder (MyTx c)
  decoderWithShare (s :: [c]) =
    Decoder $ \str -> error $ show (s == s)

main :: IO ()
main = print (noinline decode (noinline show (currency (MyTx "USD"))) :: MyTx String)
