module T26376 where

import Data.Bifunctor (first)

works x y = first (const x) <$> y

main :: IO ()
main = do
  let fails x y = first (const x) <$> y
  return ()
