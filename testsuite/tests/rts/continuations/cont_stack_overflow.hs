-- This test is run with RTS options that instruct GHC to use a small stack
-- chunk size (2k), which ensures this test exercises multi-chunk continuation
-- captures and restores.

import Control.Monad (unless)
import ContIO

data Answer
  = Done Int
  | Yield (IO Int -> IO Answer)

getAnswer :: Answer -> Int
getAnswer (Done n)  = n
getAnswer (Yield _) = error "getAnswer"

main :: IO ()
main = do
  tag <- newPromptTag
  Yield k <- prompt tag $
    Done <$> buildBigCont tag 6000
  n <- getAnswer <$> k (getAnswer <$> k (pure 0))
  unless (n == 36006000) $
    error $ "produced wrong value: " ++ show n

buildBigCont :: PromptTag Answer
             -> Int
             -> IO Int
buildBigCont tag size
  | size <= 0 = control0 tag (\k -> pure (Yield k))
  | otherwise = do
      n <- buildBigCont tag (size - 1)
      pure $! n + size
