{-# LANGUAGE ApplicativeDo #-}
module ShouldFail where

g :: IO ()
g = do
  x <- getChar
  'a' <- return (3::Int) -- type error
  return ()
