{-# OPTIONS_GHC -O2 -fforce-recomp #-}
module T18086 where

-- Should have strictness signature <L,U>x, emphasis on the exceptional
-- divergence result.
m :: IO ()
m = do
  putStrLn "foo"
  error "bar"
