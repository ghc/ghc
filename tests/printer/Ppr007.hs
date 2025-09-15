{-# LANGUAGE ApplicativeDo #-}
module Ppr007 where

g :: IO ()
g = do
  x <- getChar
  'a' <- return (3::Int) -- type error
  return ()
