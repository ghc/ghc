{-# OPTIONS_GHC -O2 -fforce-recomp -dno-typeable-binds #-}

module T18401 where

-- | A safe version of `init`.
-- @safeInit [] = Nothing@
-- @safeInit xs = Just $ init xs@
safeInit :: [a] -> Maybe [a]
safeInit xs = case si xs of
  (False, _) -> Nothing
  (_, ys) -> Just ys

si :: [a] -> (Bool, [a])
si xs0 = foldr go stop xs0 Nothing
  where
    stop Nothing = (False, [])
    stop _ = (True, [])
    go x r Nothing = (True, snd (r (Just x)))
    go x r (Just p) = (True, p : snd (r (Just x)))

