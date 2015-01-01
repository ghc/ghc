{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, ConstraintKinds #-}

module Oracles.Flag (
    module Control.Monad,
    module Prelude,
    Flag (..), 
    Condition, test, when, unless, not, (&&), (||)
    ) where

import Control.Monad hiding (when, unless)
import qualified Prelude
import Prelude hiding (not, (&&), (||))
import Base
import Oracles.Base

data Flag = LaxDeps | DynamicGhcPrograms
          | GccIsClang | GccLt46 | CrossCompiling | Validating
          | SupportsPackageKey

test :: Flag -> Action Bool
test flag = do
    (key, defaultValue) <- return $ case flag of
        LaxDeps            -> ("lax-dependencies"     , False) -- TODO: move flags to a separate file
        DynamicGhcPrograms -> ("dynamic-ghc-programs" , False)
        GccIsClang         -> ("gcc-is-clang"         , False)
        GccLt46            -> ("gcc-lt-46"            , False)
        CrossCompiling     -> ("cross-compiling"      , False)
        Validating         -> ("validating"           , False)
        SupportsPackageKey -> ("supports-package-key" , False)
    let defaultString = if defaultValue then "YES" else "NO"
    value <- askConfigWithDefault key $
        do putLoud $ "\nFlag '" -- TODO: Give the warning *only once* per key
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
when x args = do
    bool <- toCondition x
    if bool then args else mempty

unless :: (ToCondition a, Monoid m) => a -> Action m -> Action m
unless x args = do
    bool <- toCondition x
    if bool then mempty else args

class Not a where
    type NotResult a
    not :: a -> NotResult a

instance Not Bool where
    type NotResult Bool = Bool
    not = Prelude.not

instance Not Condition where
    type NotResult Condition = Condition
    not x = not <$> (toCondition x)

instance Not Flag where
    type NotResult Flag = Condition
    not x = not (toCondition x)

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
    x && y = (Prelude.&&) <$> toCondition x <*> toCondition y
    x || y = (Prelude.||) <$> toCondition x <*> toCondition y

instance ToCondition a => AndOr Flag a where
    type AndOrResult Flag a = Condition
    x && y = toCondition x && y
    x || y = toCondition x || y


