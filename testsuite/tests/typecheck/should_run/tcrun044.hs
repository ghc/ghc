{-# LANGUAGE TypeFamilies, ConstraintKind #-}

import qualified Data.Set as S

-- You can't write this as an associated type synonym
-- because it is indexed in more arguments than those
-- bound by the class
--
-- A better solution would be index it only in "m"
-- but then how do we write the instance for []?
type family RMonadCtxt m a :: Constraint

class RMonad m where
    returnR :: (RMonadCtxt m a) => a -> m a
    bindR :: (RMonadCtxt m a, RMonadCtxt m b) => m a -> (a -> m b) -> m b

type instance RMonadCtxt [] a = ()

instance RMonad [] where
    returnR x = [x]
    bindR = flip concatMap

type instance RMonadCtxt S.Set a = Ord a

instance RMonad S.Set where
    returnR x = S.singleton x
    bindR mx fxmy = S.fromList [y | x <- S.toList mx, y <- S.toList (fxmy x)]


main = do
    print $ (returnR 1 ++        returnR 2) `bindR` (\x -> returnR (x + 1) ++        returnR (x + 2))
    print $ (returnR 1 `S.union` returnR 2) `bindR` (\x -> returnR (x + 1) `S.union` returnR (x + 2))
