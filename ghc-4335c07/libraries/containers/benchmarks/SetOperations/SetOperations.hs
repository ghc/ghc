{-# LANGUAGE BangPatterns #-}

module SetOperations (benchmark) where

import Criterion.Main (bench, defaultMain, whnf)
import Data.List (partition)

benchmark :: ([Int] -> container) -> Bool -> [(String, container -> container -> container)] -> IO ()
benchmark fromList swap methods = do
  defaultMain $ [ bench (method_str++"-"++input_str) $ whnf (method input1) input2 | (method_str, method) <- methods, (input_str, input1, input2) <- inputs ]

  where
    n, s, t :: Int
    n = 100000
    s {-small-} = n `div` 10
    t {-tiny-} = round $ sqrt $ fromIntegral n

    inputs = [ (mode_str, left, right)
             | (mode_str, (left, right)) <- [ ("disj_nn", disj_nn), ("disj_ns", disj_ns), ("disj_nt", disj_nt)
                                            , ("common_nn", common_nn), ("common_ns", common_ns), ("common_nt", common_nt)
                                            , ("mix_nn", mix_nn), ("mix_ns", mix_ns), ("mix_nt", mix_nt)
                                            , ("block_nn", block_nn), ("block_ns", block_ns)
                                            ]

             , (mode_str, left, right) <- replicate 2 (mode_str, left, right) ++
                                          replicate (if swap && take 4 mode_str /= "diff" && last mode_str /= last (init mode_str) then 2 else 0)
                                            (init (init mode_str) ++ [last mode_str] ++ [last (init mode_str)], right, left)
             ]

    all_n = fromList [1..n]

    !disj_nn = seqPair $ (all_n, fromList [n+1..n+n])
    !disj_ns = seqPair $ (all_n, fromList [n+1..n+s])
    !disj_nt = seqPair $ (all_n, fromList [n+1..n+t])
    !common_nn = seqPair $ (all_n, fromList [2,4..n])
    !common_ns = seqPair $ (all_n, fromList [0,1+n`div`s..n])
    !common_nt = seqPair $ (all_n, fromList [0,1+n`div`t..n])
    !mix_nn = seqPair $ fromLists $ partition ((/= 0) . (`mod` 2)) [1..n+n]
    !mix_ns = seqPair $ fromLists $ partition ((/= 0) . (`mod` (1 + n`div`s))) [1..s+n]
    !mix_nt = seqPair $ fromLists $ partition ((/= 0) . (`mod` (1 + n`div`t))) [1..t+n]
    !block_nn = seqPair $ fromLists $ partition ((>= t) . (`mod` (t * 2))) [1..n+n]
    !block_ns = seqPair $ fromLists $ partition ((>= t) . (`mod` (t * (1 + n`div`s)))) [1..s+n]

    fromLists (xs, ys) = (fromList xs, fromList ys)
    seqPair pair@(xs, ys) = xs `seq` ys `seq` pair
