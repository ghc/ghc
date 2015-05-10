{-# LANGUAGE NoImplicitPrelude, FlexibleInstances #-}
module Expression (
    module Control.Monad.Reader,
    Ways,
    Packages,
    TargetDir,
    Predicate,
    Expression,
    Environment (..),
    interpret,
    whenPredicate, (?), stage, notStage, package,
    configKeyValue, configKeyValues,
    configKeyYes, configKeyNo, configKeyNonEmpty
    ) where

import Base hiding (arg, args, Args, TargetDir)
import Ways
import Oracles
import Package
import Control.Monad.Reader

data Environment = Environment
     {
        getStage   :: Stage,
        getBuilder :: Builder,
        getPackage :: Package
     }

defaultEnvironment :: Environment
defaultEnvironment = Environment
    {
        getStage   = error "Stage not set in the environment",
        getBuilder = error "Builder not set in the environment",
        getPackage = error "Package not set in the environment"
    }

type Expression m a = ReaderT Environment m a

type Ways      m = Expression m [Way]
type Packages  m = Expression m [Package]
type Predicate m = Expression m Bool
type TargetDir m = Expression m FilePath

instance (Monad m, Monoid a) => Monoid (Expression m a) where
    mempty  = return mempty
    mappend = liftM2 mappend

interpret :: (Monad m, Monoid a) => Expression m a -> Environment -> m a
interpret = runReaderT

whenPredicate :: (Monad m, Monoid a) => Predicate m -> Expression m a -> Expression m a
whenPredicate predicate expr = do
    bool <- predicate
    if bool then expr else return mempty

(?) :: (Monad m, Monoid a) => Predicate m -> Expression m a -> Expression m a
(?) = whenPredicate

infixr 8 ?

stage :: Monad m => Stage -> Predicate m
stage s = liftM (s ==) (asks getStage)

notStage :: Monad m => Stage -> Predicate m
notStage = liftM not . stage

package :: Monad m => Package -> Predicate m
package p = liftM (p ==) (asks getPackage)

configKeyValue :: String -> String -> Predicate Action
configKeyValue key value = liftM (value ==) (lift $ askConfig key)

-- checks if there is at least one match
configKeyValues :: String -> [String] -> Predicate Action
configKeyValues key values = liftM (flip elem $ values) (lift $ askConfig key)

configKeyYes :: String -> Predicate Action
configKeyYes key = configKeyValue key "YES"

configKeyNo :: String -> Predicate Action
configKeyNo key = configKeyValue key "NO"

configKeyNonEmpty :: String -> Predicate Action
configKeyNonEmpty key = configKeyValue key ""
