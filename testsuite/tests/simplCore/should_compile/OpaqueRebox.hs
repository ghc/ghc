module OpaqueRebox where

f :: Int -> Int
f x = x `seq` (x + 1)
{-# OPAQUE f #-}

-- Since no W/W happens for f because it is OPAQUE, the worker for g does do
-- the dreaded reboxing of p similar to what is mentioned in
-- https://gitlab.haskell.org/ghc/ghc/-/issues/13143
g :: Bool -> Bool -> Bool -> Int -> Int
g = \w w1 w2 p ->
  let fail_ = case w1 of
              False -> case w2 of
                 False -> g w True w2 p
                 True  -> f (f p)
              True -> error "patError"
  in case w of
        False -> case w1 of
          False -> fail_
          True -> case w2 of
            False -> p + 1
            True -> fail_
        True -> case w1 of
          False -> fail_
          True -> case w2 of
            _ -> f p
