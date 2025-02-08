module Main (main) where

import GHC.IO (unsafePerformIO)
import qualified Data.Array.IO as Array
import Data.Array.Base  ( unsafeWrite )
import Data.Word
import Control.Monad

data BCInstr = C0 | C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9
   | C10 | C11 | C12 | C13 | C14 | C15 | C16 | C17 | C18 | C19
   | C20 | C21 | C22 | C23 | C24 | C25 | C26 | C27 | C28 | C29
   | C30 | C31 | C32 | C33 | C34 | C35 | C36 | C37 | C38 | C39
   | C40 | C41 | C42 | C43 | C44 | C45 | C46 | C47 | C48 | C49
   | C50 | C51 | C52 | C53 | C54 | C55 | C56 | C57 | C58 | C59
   | C60 | C61 | C62 | C63 | C64 | C65 | C66 | C67 | C68 | C69
   | C70 | C71 | C72 | C73 | C74 | C75 | C76

assembleI :: BCInstr -> Int -> IO Int
assembleI i = case i of
  C4  -> emit; C5  -> emit; C6  -> emit; C7  -> emit; C8  -> emit; C9  -> emit; C10 -> emit
  C11 -> emit; C12 -> emit; C13 -> emit; C14 -> emit; C15 -> emit; C16 -> emit; C17 -> emit
  C18 -> emit; C19 -> emit; C20 -> emit; C21 -> emit; C22 -> emit; C23 -> emit; C24 -> emit
  C25 -> emit; C26 -> emit; C27 -> emit; C28 -> emit; C29 -> emit; C30 -> emit; C31 -> emit
  C32 -> emit; C33 -> emit; C34 -> emit; C35 -> emit; C36 -> emit; C37 -> emit; C38 -> emit
  C39 -> emit; C40 -> emit; C41 -> emit; C42 -> emit; C43 -> emit; C44 -> emit; C45 -> emit
  C46 -> emit; C47 -> emit; C48 -> emit; C49 -> emit; C50 -> emit; C51 -> emit; C52 -> emit
  C53 -> emit; C54 -> emit; C55 -> emit; C56 -> emit; C57 -> emit; C58 -> emit; C59 -> emit
  C60 -> emit; C61 -> emit; C62 -> emit; C63 -> emit; C64 -> emit; C65 -> emit; C66 -> emit
  C67 -> emit; C68 -> emit; C69 -> emit; C70 -> emit; C71 -> emit; C72 -> emit; C73 -> emit
  C74 -> emit; C75 -> emit; C76 -> emit

emit :: Int -> IO Int
emit i = do
  mapM6 (\w1 -> case l of
    [] -> writeIsn w1 i
    _  -> largeArg (fromIntegral @Word @Word64 w1) i) l
{-# INLINE emit #-}

isn_array :: Array.IOUArray Int Word
isn_array = unsafePerformIO $ Array.newArray_ (0, 2)
{-# NOINLINE isn_array #-}

runInstrs :: [BCInstr] -> IO Int
runInstrs instrs = foldM (\a i -> assembleI i a) 0 instrs
{-# NOINLINE runInstrs #-}

writeIsn :: Word -> Int -> IO Int
writeIsn w nisn = do
  unsafeWrite isn_array nisn w
  return (nisn + 1)
{-# INLINE writeIsn #-}

mapM6 :: (Word -> IO Int) -> [Word] -> IO Int
mapM6 _ [] = return 0
mapM6 f [x] = f x
mapM6 f [x,y] = (+) <$> f x <*> f y
{-# INLINE mapM6 #-}

largeArg :: Word64 -> Int -> IO Int
largeArg w i =
   do i1 <- writeIsn (fromIntegral @Word64 @Word w) i
      i2 <- writeIsn (fromIntegral @Word64 @Word w) i1
      i3 <- writeIsn (fromIntegral @Word64 @Word w) i2
      return i3
{-# INLINE largeArg #-}

l :: [a]
l = []
{-# NOINLINE l #-}

main :: IO ()
main = do
  putStrLn "hi"
  !_ <- runInstrs [ C12 ]
  putStrLn "bye"

