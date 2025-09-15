module T9441 where
-- Core output should show only one recursive Bind Rec { .. }
import GHC.Exts (build)

{-# INLINE reverse' #-}
reverse' :: [a] -> [a]
reverse' xs = build $ \c n -> foldl (\a x -> x `c` a) n xs

appRev :: [a] -> [a] -> [a]
appRev xs ys = xs ++ reverse' ys

revAppRev :: [a] -> [a] -> [a]
revAppRev xs ys = reverse' xs ++ reverse' ys
