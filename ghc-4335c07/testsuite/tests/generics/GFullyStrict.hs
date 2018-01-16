{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Data.Proxy (Proxy(..))
import GHC.Generics

main :: IO ()
main = do
  print (fullyStrict (Proxy :: Proxy (StrictMaybe Bool)))
  print (fullyStrict (Proxy :: Proxy (Maybe Bool)))

data StrictMaybe a = StrictNothing | StrictJust !a
  deriving (FullyStrict, Generic)

instance FullyStrict Bool
instance FullyStrict a => FullyStrict (Maybe a)

class FullyStrict a where
  fullyStrict :: proxy a -> Bool
  default fullyStrict :: (GFullyStrict (Rep a)) => proxy a -> Bool
  fullyStrict _ = gfullyStrict (Proxy :: Proxy (Rep a p))

class GFullyStrict f where
  gfullyStrict :: proxy (f p) -> Bool

instance GFullyStrict V1 where
  gfullyStrict _ = True

instance GFullyStrict U1 where
  gfullyStrict _ = True

instance FullyStrict c => GFullyStrict (Rec0 c) where
  gfullyStrict _ = fullyStrict (Proxy :: Proxy c)

instance GFullyStrict f => GFullyStrict (D1 c f) where
  gfullyStrict _ = gfullyStrict (Proxy :: Proxy (f p))

instance GFullyStrict f => GFullyStrict (C1 c f) where
  gfullyStrict _ = gfullyStrict (Proxy :: Proxy (f p))

instance (GFullyStrict f, Selector c) => GFullyStrict (S1 c f) where
  gfullyStrict _ = gfullyStrict (Proxy :: Proxy (f p))
                && selDecidedStrictness (undefined :: S1 c f p) /= DecidedLazy

instance (GFullyStrict f, GFullyStrict g) => GFullyStrict (f :+: g) where
  gfullyStrict _ =
    gfullyStrict (Proxy :: Proxy (f p)) && gfullyStrict (Proxy :: Proxy (g p))

instance (GFullyStrict f, GFullyStrict g) => GFullyStrict (f :*: g) where
  gfullyStrict _ =
    gfullyStrict (Proxy :: Proxy (f p)) && gfullyStrict (Proxy :: Proxy (g p))
