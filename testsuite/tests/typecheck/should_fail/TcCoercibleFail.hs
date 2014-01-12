{-# LANGUAGE RoleAnnotations, RankNTypes, ScopedTypeVariables #-}

import GHC.Prim (coerce, Coercible)
import Data.Ord (Down)

newtype Age = Age Int deriving Show

type role Map nominal _
data Map a b = Map a b deriving Show

foo1 = coerce $ one :: ()

foo2 :: forall m. Monad m => m Age
foo2 = coerce $ (return one :: m Int)

foo3 = coerce $ Map one () :: Map Age ()

foo4 = coerce $ one :: Down Int

newtype Void = Void Void
foo5 = coerce :: Void -> ()

-- Do not test this; fills up memory
--newtype VoidBad a = VoidBad (VoidBad (a,a))
--foo5 = coerce :: (VoidBad ()) -> ()


-- This shoul fail with a context stack overflow
newtype Fix f = Fix (f (Fix f))
foo6 = coerce :: Fix (Either Int) -> Fix (Either Age)
foo7 = coerce :: Fix (Either Int) -> ()


one :: Int
one = 1
main = return ()
