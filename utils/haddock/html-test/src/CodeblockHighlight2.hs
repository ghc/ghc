module CodeblockHighlight2 where

-- | The following codeblocks start at different indentation in the source, but
-- that should not be visible in the rendered html:
--
-- ```haskell
-- add :: Int -> Int -> Int
-- add x y = x + y
-- ```
--
--    ```haskell
--    mul :: Int -> Int -> Int
--    mul x y = x * y
--    ```
--
--```haskell
--pow :: Int -> Int -> Int
--pow x y = x ^ y
--```
add :: Int -> Int -> Int
add x y = x + y

-- | The triple backtick notation can also be used without mentioning a language
--
-- ```
-- runghc CodeBlockHighlight2.hs
-- ```
main :: IO ()
main = putStrLn "Hello, Haskell!"
