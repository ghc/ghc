{-# LANGUAGE RebindableSyntax, MultiParamTypeClasses,
             FlexibleInstances #-}

-- Tests of the do-notation for the restricted monads
-- We demonstrate that all ordinary monads are restricted monads,
-- and show the frequently requested implementation
-- of MonadPlus in terms of Data.Set.
-- 
-- The tests are based on the code
-- http://okmij.org/ftp/Haskell/types.html#restricted-datatypes

module DoRestrictedM where

import Data.List
import Prelude (const, String, ($), (.), Maybe(..))
import qualified Prelude
import qualified Data.Set as Set

-- Defining the restricted monad
class MN2 m a where
    return  :: a -> m a
    fail    :: String -> m a

class (MN2 m a, MN2 m b) => MN3 m a b where
    (>>=) :: m a -> (a -> m b) -> m b

m1 >> m2 = m1 >>= (const m2)

-- All regular monads are the instances of the restricted monad

newtype RegularM m a = RegularM{unRM :: m a}

instance Prelude.Monad m => MN2 (RegularM m) a where
    return = RegularM . Prelude.return
    fail   = RegularM . Prelude.fail

instance Prelude.Monad m => MN3 (RegularM m) a b where
    m >>= f = RegularM ((Prelude.>>=) (unRM m) (unRM . f))

-- We try to inject Maybe (as the regular monad) into Restricted Monad

test1s () =  return "a" >>= (\x -> return $ "b" ++ x)
test1f () =  fail ""    >>= (\x -> return $ "b" ++ x)

-- the same with the do-notation

test1s_do () = do
  x <- return "a"
  return $ "b" ++ x


{-
whose inferred type is
   *DoRestrictedM> :t test1s
   test1s :: (MN3 m [Prelude.Char] [Prelude.Char]) => () -> m [Prelude.Char]
-}

test1sr :: Maybe String
test1sr = unRM $ test1s ()
-- Just "ba"

test1fr :: Maybe String
test1fr = unRM $ test1f ()
-- Nothing

test1sr_do :: Maybe String
test1sr_do = unRM $ test1s_do ()
-- Just "ba"

-- As often requested, we implement a MonadPlus `monad'
-- in terms of a Set. Set requires the Ord constraint.

newtype SMPlus a = SMPlus{unSM:: Set.Set a}

instance MN2 SMPlus a where
    return = SMPlus . Set.singleton
    fail x = SMPlus $ Set.empty

instance Prelude.Ord b => MN3 SMPlus a b where
    m >>= f = SMPlus (Set.fold (Set.union . unSM . f) Set.empty (unSM m))

-- We cannot forget the Ord constraint, because the typechecker
-- will complain (and tell us exactly what we have forgotten).

-- Now we can instantiate the previously written test1s and test1d 
-- functions for this Set monad:

test2sr :: Set.Set String
test2sr = unSM $ test1s ()
-- fromList ["ba"]

test2fr :: Set.Set String
test2fr = unSM $ test1f ()
-- fromList []

test2sr_do :: Set.Set String
test2sr_do = unSM $ test1s_do ()
-- fromList ["ba"]

