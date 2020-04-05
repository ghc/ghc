{-# LANGUAGE UnboxedTuples #-}

-- See Note [Float coercions (unlifted)] in GHC.Core.Opt.Simplify
-- This one gave a CoreLint error when compiled optimised
--
-- See also #1718, of which this is a simplified version

module ShouldCompile where

bar :: Bool -> Int
bar x = case (case x of { True -> (# 2,3 #); False -> error "urk" }) of
                (# p,q #) -> p+q
