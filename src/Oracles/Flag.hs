{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}

module Oracles.Flag (
    module Control.Monad,
    module Prelude,
    Flag (..), 
    test, when, unless, not, (&&), (||), (<?>)
    ) where

import Control.Monad hiding (when, unless)
import qualified Prelude
import Prelude hiding (not, (&&), (||))
import Base
import Oracles.Base

data Flag = LaxDeps
          | DynamicGhcPrograms
          | GccIsClang
          | GccLt46
          | CrossCompiling
          | Validating
          | SupportsPackageKey
          | SolarisBrokenShld

-- TODO: Give the warning *only once* per key
test :: Flag -> Action Bool
test flag = do
    (key, defaultValue) <- return $ case flag of
        LaxDeps            -> ("lax-dependencies"     , False)
        DynamicGhcPrograms -> ("dynamic-ghc-programs" , False)
        GccIsClang         -> ("gcc-is-clang"         , False)
        GccLt46            -> ("gcc-lt-46"            , False)
        CrossCompiling     -> ("cross-compiling"      , False)
        Validating         -> ("validating"           , False)
        SupportsPackageKey -> ("supports-package-key" , False)
        SolarisBrokenShld  -> ("solaris-broken-shld"  , False)
    let defaultString = if defaultValue then "YES" else "NO"
    value <- askConfigWithDefault key $
        do putLoud $ "\nFlag '"
                ++ key
                ++ "' not set in configuration files. "
                ++ "Proceeding with default value '"
                ++ defaultString
                ++ "'.\n"
           return defaultString
    return $ value == "YES"

class ToCondition a where
    toCondition :: a -> Condition

instance ToCondition Condition where
    toCondition = id

instance ToCondition Bool where
    toCondition = return

instance ToCondition Flag where
    toCondition = test

when :: (ToCondition a, Monoid m) => a -> Action m -> Action m
when x act = do
    bool <- toCondition x
    if bool then act else mempty

unless :: (ToCondition a, Monoid m) => a -> Action m -> Action m
unless x act = do
    bool <- toCondition x
    if bool then mempty else act

-- Infix version of when
(<?>) :: (ToCondition a, Monoid m) => a -> Action m -> Action m
(<?>) = when

class Not a where
    type NotResult a
    not :: a -> NotResult a

instance Not Bool where
    type NotResult Bool = Bool
    not = Prelude.not

instance Not Condition where
    type NotResult Condition = Condition
    not = fmap not

instance Not Flag where
    type NotResult Flag = Condition
    not = not . toCondition

class AndOr a b where
    type AndOrResult a b
    (&&) :: a -> b -> AndOrResult a b
    (||) :: a -> b -> AndOrResult a b

infixr 3 &&
infixr 2 ||

instance AndOr Bool Bool where
    type AndOrResult Bool Bool = Bool
    (&&) = (Prelude.&&)
    (||) = (Prelude.||)

instance ToCondition a => AndOr Condition a where
    type AndOrResult Condition a = Condition
    x && y = (&&) <$> x <*> toCondition y
    x || y = (||) <$> x <*> toCondition y

instance ToCondition a => AndOr Flag a where
    type AndOrResult Flag a = Condition
    x && y = toCondition x && y
    x || y = toCondition x || y

-- TODO: need more instances to handle Bool as first argument of (&&), (||)
