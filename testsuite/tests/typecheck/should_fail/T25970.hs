
{-# LANGUAGE TypeFamilies #-}
module T25970 where

y :: IO ()
y = putStrLn "y"


type family K a where
  K a = Bool

x :: IO (K b)
x = do
  y
  pure () -- The error should point here or on the whole do block

x' :: IO (K b)
x' = y >> pure ()
