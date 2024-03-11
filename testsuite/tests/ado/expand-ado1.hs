{-# LANGUAGE ApplicativeDo,ScopedTypeVariables #-}
module Test where

-- Test that type errors aren't affected by ApplicativeDo
rrrr1 :: IO Int
rrrr1 = do
  x <- getChar
  y <- getChar 'a' -- type error
  return (x,y)



rrrr2 :: IO Int
rrrr2 = do
  x <- getChar
  y <- getChar 'a' -- type error
  print (x,y)

-- g :: IO (Int,Int)
-- g = do
--   x <- getChar
--   y <- getChar
--   return (y,x)

-- h :: IO (Int,Int)
-- h = do
--   x1 <- getChar
--   x2 <- getChar
--   x3 <- const (return ()) x1
--   x4 <- getChar
--   x5 <- getChar x4
--   return (x2,x4)
