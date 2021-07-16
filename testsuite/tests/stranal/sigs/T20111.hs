{-# LANGUAGE TypeApplications #-}

module T20111 where

import Control.Exception

data R = R {field :: ()}

r0 :: R
r0 = R{field = error "unused field"}

main :: IO a
main = f r0

f :: R -> IO a
f x = do
  x `seq` catch @SomeException (putStrLn "done") (const $ pure ())
  f x

