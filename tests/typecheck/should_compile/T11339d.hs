{-# LANGUAGE NoMonoLocalBinds, RankNTypes #-}

-- Ross Paterson's example from
-- https://prime.haskell.org/wiki/MonomorphicPatternBindings

module T11339d where

import Control.Monad.ST

newtype ListMap m a b = ListMap ([a] -> m [b])

runMap :: (forall s. ListMap (ST s) a b) -> [a] -> [b]
runMap lf as = runST (f as)
             where
               ListMap f = lf
