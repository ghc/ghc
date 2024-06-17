module CodeblockHighlight where

-- | A simple function that adds two integers.
--
-- Its implementation is straightforward:
--
-- ```haskell
-- add :: Int -> Int -> Int
-- add x y = x + y
-- ```
add :: Int -> Int -> Int
add x y = x + y

-- | Run this program as follows:
--
-- ```bash
-- runghc CodeBlockHighlight.hs
-- ```
main :: IO ()
main = putStrLn "Hello, Haskell!"
