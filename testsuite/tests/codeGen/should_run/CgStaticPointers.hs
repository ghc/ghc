{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StaticPointers     #-}

-- | A test to use symbols produced by the static form.
module Main(main) where

import Data.Typeable
import GHC.StaticPtr

main :: IO ()
main = do
  print $ lookupKey (static (id . id)) (1 :: Int)
  print $ lookupKey (static method :: StaticPtr (Char -> Int)) 'a'
  print $ deRefStaticPtr (static g)
  print $ deRefStaticPtr p0 'a'
  print $ deRefStaticPtr (static t_field) $ T 'b'

lookupKey :: StaticPtr a -> a
lookupKey p = case unsafeLookupStaticPtr (staticKey p) of
  Just p -> deRefStaticPtr p
  Nothing -> error $ "couldn't find " ++ show (staticPtrInfo p)

g :: String
g = "found"

p0 :: Typeable a => StaticPtr (a -> a)
p0 = static (\x -> x)

data T a = T { t_field :: a }
  deriving Typeable

class C1 a where
  method :: a -> Int

instance C1 Char where
  method = const 0
