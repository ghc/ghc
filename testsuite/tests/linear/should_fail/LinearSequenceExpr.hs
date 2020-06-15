{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedLists #-}

module LinearSequenceExpr where

f :: Char #-> Char #-> [Char]
f x y = [x .. y]
-- This ought to fail, because `fromList` in base, is unrestricted
