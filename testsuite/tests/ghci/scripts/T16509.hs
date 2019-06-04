{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module PatternPanic where

pattern TestPat :: (Int, Int)
pattern TestPat <- (isSameRef -> True, 0)

isSameRef :: Int -> Bool
isSameRef e | 0 <- e = True
isSameRef _ = False
