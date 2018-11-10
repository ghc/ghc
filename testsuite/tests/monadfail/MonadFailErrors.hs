-- Test purpose:
-- Break properly if MonadFail is live

module MonadFailWarnings where

import Control.Monad.Fail
import Control.Monad.ST
import Data.Functor.Identity



general :: Monad m => m a
general = do
    Just x <- undefined
    undefined



general' :: MonadFail m => m a
general' = do
    Just x <- undefined
    undefined



identity :: Identity a
identity = do
    Just x <- undefined
    undefined



io :: IO a
io = do
    Just x <- undefined
    undefined



st :: ST s a
st = do
    Just x <- undefined
    undefined



reader :: r -> a
reader = do
    Just x <- undefined
    undefined



newtype Newtype a = Newtype a
newtypeMatch :: Identity a
newtypeMatch = do
    Newtype x <- undefined
    undefined



data Data a = Data a
singleConMatch :: Identity a
singleConMatch = do
    Data x <- undefined
    undefined



data Maybe' a = Nothing' | Just' a
instance Functor Maybe' where fmap = undefined
instance Applicative Maybe' where pure = undefined; (<*>) = undefined
instance Monad Maybe' where (>>=) = undefined
instance MonadFail Maybe' where fail = undefined
customFailable :: Maybe' a
customFailable = do
    Just x <- undefined
    undefined


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
