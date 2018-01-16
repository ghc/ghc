-- Sven Panne found this example; a mistake in typechecking 
-- lazy patterns made functions too strict in a version of
-- GHC 6.7

module Main where

import Foreign

-- Strangely enough, this works if newtype is used...
data Elem a = Elem a

instance Storable a => Storable (Elem a) where
   sizeOf ~(Elem r) = 3 * sizeOf r
   alignment ~(Elem r) = alignment r
   peek ptr = do r <- peekElemOff (castPtr ptr) 0; return (Elem r)
   poke ptr (Elem r) = poke (castPtr ptr) r

main :: IO ()
main = do
   putStrLn "*** main 1"
   allocaBytes 100 $ \buf -> do
      poke buf (Elem 12345)
      putStrLn "*** main 2"
      Elem x <- peekElemOff buf 0
      print (x :: Int)
      putStrLn "*** main 3"

