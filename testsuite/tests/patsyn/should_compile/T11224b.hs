{-# LANGUAGE PatternSynonyms, GADTs, RankNTypes #-}

module T11224b where

data T b where
  MkT :: a -> b -> T b

-- Should be fine!
-- pattern P :: c -> d -> T d
pattern P :: forall d. forall c. c -> d -> T d
pattern P x y <- MkT x y






