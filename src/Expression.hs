{-# LANGUAGE NoImplicitPrelude, FlexibleInstances #-}
module Expression (
    module Control.Monad.Reader,
    Ways,
    Predicate,
    Expression,
    Environment (..), defaultEnvironment,
    interpret,
    whenPredicate, (?), (??), stage, notStage, builder, notBuilder, package,
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

type Expression a = ReaderT Environment Action a

type Ways      = Expression [Way]
type Predicate = Expression Bool

instance Monoid a => Monoid (Expression a) where
    mempty  = return mempty
    mappend = liftM2 mappend

interpret :: Environment -> Expression a -> Action a
interpret = flip runReaderT

whenPredicate :: Monoid a => Predicate -> Expression a -> Expression a
whenPredicate predicate expr = do
    bool <- predicate
    if bool then expr else return mempty

(?) :: Monoid a => Predicate -> Expression a -> Expression a
(?) = whenPredicate

(??) :: Monoid a => Predicate -> (Expression a, Expression a) -> Expression a
p ?? (t, f) = p ? t <> (liftM not p) ? f

infixr 8 ?

stage :: Stage -> Predicate
stage s = liftM (s ==) (asks getStage)

notStage :: Stage -> Predicate
notStage = liftM not . stage

builder :: Builder -> Predicate
builder b = liftM (b ==) (asks getBuilder)

notBuilder :: Builder -> Predicate
notBuilder = liftM not . builder

package :: Package -> Predicate
package p = liftM (p ==) (asks getPackage)

configKeyValue :: String -> String -> Predicate
configKeyValue key value = liftM (value ==) (lift $ askConfig key)

-- checks if there is at least one match
configKeyValues :: String -> [String] -> Predicate
configKeyValues key values = liftM (flip elem $ values) (lift $ askConfig key)

configKeyYes :: String -> Predicate
configKeyYes key = configKeyValue key "YES"

configKeyNo :: String -> Predicate
configKeyNo key = configKeyValue key "NO"

configKeyNonEmpty :: String -> Predicate
configKeyNonEmpty key = liftM not $ configKeyValue key ""
