{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StaticPointers     #-}

-- | A test to use symbols produced by the static form.
module Main(main) where

import Data.Typeable
import GHC.StaticPtr

main :: IO ()
main = do
  lookupKey (static (id . id)) >>= \f -> print $ f (1 :: Int)
  lookupKey (static method :: StaticPtr (Char -> Int)) >>= \f -> print $ f 'a'
  print $ deRefStaticPtr (static g)
  print $ deRefStaticPtr p0 'a'
  print $ deRefStaticPtr (static t_field) $ T 'b'
 where
  g :: String
  g = "found"

lookupKey :: StaticPtr a -> IO a
lookupKey p = unsafeLookupStaticPtr (staticKey p) >>= \case
  Just p -> return $ deRefStaticPtr p
  Nothing -> error $ "couldn't find " ++ show (staticPtrInfo p)

p0 :: Typeable a => StaticPtr (a -> a)
p0 = static (\x -> x)

data T a = T { t_field :: a }
  deriving Typeable

class C1 a where
  method :: a -> Int

instance C1 Char where
  method = const 0
