-- Test purpose:
-- Ensure that MonadFail warnings are issued correctly if the warning flag
-- is enabled

{-# OPTIONS_GHC -Wmissing-monadfail-instances #-}

module MonadFailWarnings where

import Control.Monad.Fail
import Control.Monad.ST
import Data.Functor.Identity



-- should warn, because the do-block gets a general Monad constraint,
-- but should have MonadFail
general :: Monad m => m a
general = do
    Just x <- undefined
    undefined



-- should NOT warn, because the constraint is correct
general' :: MonadFail m => m a
general' = do
    Just x <- undefined
    undefined



-- should warn, because Identity isn't MonadFail
identity :: Identity a
identity = do
    Just x <- undefined
    undefined



-- should NOT warn, because IO is MonadFail
io :: IO a
io = do
    Just x <- undefined
    undefined



-- should warn, because (ST s) is not MonadFail
st :: ST s a
st = do
    Just x <- undefined
    undefined



-- should warn, because (r ->) is not MonadFail
reader :: r -> a
reader = do
    Just x <- undefined
    undefined



-- should NOT warn, because matching against newtype
newtype Newtype a = Newtype a
newtypeMatch :: Identity a
newtypeMatch = do
    Newtype x <- undefined
    undefined



-- should NOT warn, because Data has only one constructor
data Data a = Data a
singleConMatch :: Identity a
singleConMatch = do
    Data x <- undefined
    undefined



-- should NOT warn, because Maybe' has a MonadFail instance
data Maybe' a = Nothing' | Just' a
instance Functor Maybe' where fmap = undefined
instance Applicative Maybe' where pure = undefined; (<*>) = undefined
instance Monad Maybe' where (>>=) = undefined
instance MonadFail Maybe' where fail = undefined
customFailable :: Maybe' a
customFailable = do
    Just x <- undefined
    undefined


-- should NOT warn, because patterns always match
wildcardx, explicitlyIrrefutable, wildcard_, tuple :: Monad m => m a
wildcardx = do
    x <- undefined
    undefined
explicitlyIrrefutable = do
    ~(x:y) <- undefined
    undefined
wildcard_ = do
    _ <- undefined
    undefined
tuple = do
    (a,b) <- undefined
    undefined
