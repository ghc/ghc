-- This test is a very simple exercise of continuation capture and restore.

import ContIO

example :: IO [Integer]
example = do
  tag <- newPromptTag
  reset tag $ do
    n <- shift tag $ \k -> do
      a <- k (pure 2)
      b <- k (pure 3)
      pure (a ++ b)
    pure [n]

main :: IO ()
main = print =<< example
