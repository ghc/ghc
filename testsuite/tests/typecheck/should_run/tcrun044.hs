{-# LANGUAGE TypeFamilies, ConstraintKinds #-}

import qualified Data.Set as S
import GHC.Prim ( Constraint )

class RMonad m where
    type RMonadCtxt m a :: Constraint
    returnR :: (RMonadCtxt m a) => a -> m a
    bindR :: (RMonadCtxt m a, RMonadCtxt m b) => m a -> (a -> m b) -> m b


instance RMonad [] where
    type RMonadCtxt [] a = ()
    returnR x = [x]
    bindR = flip concatMap


instance RMonad S.Set where
    type RMonadCtxt S.Set a = Ord a
    returnR x = S.singleton x
    bindR mx fxmy = S.fromList [y | x <- S.toList mx, y <- S.toList (fxmy x)]


main = do
    print $ (returnR 1 ++        returnR 2) `bindR` 
            (\x -> returnR (x + 1) ++        returnR (x + 2))
    print $ (returnR 1 `S.union` returnR 2) `bindR` 
            (\x -> returnR (x + 1) `S.union` returnR (x + 2))
