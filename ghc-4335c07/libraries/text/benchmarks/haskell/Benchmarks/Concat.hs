{-# LANGUAGE OverloadedStrings #-}

module Benchmarks.Concat (benchmark) where

import Control.Monad.Trans.Writer
import Criterion (Benchmark, bgroup, bench, whnf)
import Data.Text as T

benchmark :: IO Benchmark
benchmark = return $ bgroup "Concat"
  [ bench "append" $ whnf (append4 "Text 1" "Text 2" "Text 3") "Text 4"
  , bench "concat" $ whnf (concat4 "Text 1" "Text 2" "Text 3") "Text 4"
  , bench "write"  $ whnf (write4  "Text 1" "Text 2" "Text 3") "Text 4"
  ]

append4, concat4, write4 :: Text -> Text -> Text -> Text -> Text

{-# NOINLINE append4 #-}
append4 x1 x2 x3 x4 = x1 `append` x2 `append` x3 `append` x4

{-# NOINLINE concat4 #-}
concat4 x1 x2 x3 x4 = T.concat [x1, x2, x3, x4]

{-# NOINLINE write4 #-}
write4 x1 x2 x3 x4 = execWriter $ tell x1 >> tell x2 >> tell x3 >> tell x4
