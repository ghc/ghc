{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Main (main) where

import Data.Kind
import GHC.Exts


data D a = MkD
  deriving Eq

class Def a where
  def :: a
instance Def (D a) where
  def = MkD

type family Share a where
  Share Char = Char


class ( Share a ~ a, Def a ) => ClassDecode a where
instance ClassLedger c => ClassDecode (D c) where

class (Eq e, ClassDecode (D e)) => ClassLedger e where
instance Eq c => ClassLedger c where


decoderWithShare2 :: ClassLedger a => a -> Bool
decoderWithShare2 d = d == d


decode :: forall a. (ClassLedger a, ClassDecode a) => Bool
decode = decoderWithShare2 @a (def @(Share a))

main :: IO ()
main = print (decode @(D Char))
