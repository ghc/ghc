{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module T27124a where

import Data.String (IsString(..))

newtype Wrap a = Wrap a deriving (Eq, Show)

instance IsString a => IsString (Wrap a) where
  fromString = Wrap . fromString

instance {-# INCOHERENT #-} IsString (Wrap Bool) where
  fromString _ = Wrap False

f :: (Eq a, IsString a) => Wrap a -> Bool
f "hello" = True
f _       = False

main :: IO ()
main = do
  print (f (Wrap ("hello" :: String)))
  print (f (Wrap ("world" :: String)))
