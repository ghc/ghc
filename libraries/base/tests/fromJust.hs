module Main where

-- Trac #15559: Add HasCallStack to fromJust

import Data.Maybe ( fromJust )

main :: IO ()
main = do
  _ <- fromJust Nothing `seq` return ()
  putStrLn "Should see a stacktrace instead of this"
