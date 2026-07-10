{-# LANGUAGE ExistentialQuantification, RecordWildCards, ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

-- | This module implements the Key/Value types, to abstract over hetrogenous data types.
module Development.Shake.Internal.Value(
    Value, newValue, fromValue,
    Key, newKey, fromKey, typeKey,
    ShakeValue
    ) where

import Development.Shake.Classes
import Development.Shake.Internal.Errors
import Data.Typeable

import Unsafe.Coerce


-- | Define an alias for the six type classes required for things involved in Shake rules.
--   Using this alias requires the @ConstraintKinds@ extension.
--
--   To define your own values meeting the necessary constraints it is convenient to use the extensions
--   @GeneralizedNewtypeDeriving@ and @DeriveDataTypeable@ to write:
--
-- > newtype MyType = MyType (String, Bool) deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
--
--   Shake needs these instances on keys and values. They are used for:
--
-- * 'Show' is used to print out keys in errors, profiling, progress messages
--   and diagnostics.
--
-- * 'Typeable' is used because Shake indexes its database by the
--   type of the key and value involved in the rule (overlap is not
--   allowed for type classes and not allowed in Shake either).
--
-- * 'Eq' and 'Hashable' are used on keys in order to build hash maps
--   from keys to values.  'Eq' is used on values to test if the value
--   has changed or not (this is used to support unchanging rebuilds,
--   where Shake can avoid rerunning rules if it runs a dependency,
--   but it turns out that no changes occurred.)  The 'Hashable'
--   instances are only use at runtime (never serialised to disk),
--   so they do not have to be stable across runs.
--   Hashable on values is not used, and only required for a consistent interface.
--
-- * 'Binary' is used to serialize keys and values into Shake's
--   build database; this lets Shake cache values across runs and
--   implement unchanging rebuilds.
--
-- * 'NFData' is used to avoid space and thunk leaks, especially
--   when Shake is parallelized.
type ShakeValue a = (Show a, Typeable a, Eq a, Hashable a, Binary a, NFData a)

-- We deliberately avoid Typeable instances on Key/Value to stop them accidentally
-- being used inside themselves
data Key = forall a . Key
    {keyType :: TypeRep
    ,keyShow :: a -> String
    ,keyRnf :: a -> ()
    ,keyEq :: a -> a -> Bool
    ,keyHash :: Int -> a -> Int
    ,keyValue :: a
    }

data Value = forall a . Value
    {valueType :: TypeRep
    ,valueShow :: a -> String
    ,valueRnf :: a -> ()
    ,valueValue :: a
    }


newKey :: forall a . ShakeValue a => a -> Key
newKey = Key (typeRep (Proxy :: Proxy a)) show rnf (==) hashWithSalt

newValue :: forall a . (Typeable a, Show a, NFData a) => a -> Value
newValue = Value (typeRep (Proxy :: Proxy a)) show rnf

typeKey :: Key -> TypeRep
typeKey Key{..} = keyType

fromKey :: forall a . Typeable a => Key -> a
fromKey Key{..}
    | keyType == resType = unsafeCoerce keyValue
    | otherwise = throwImpure $ errorInternal $ "fromKey, bad cast, have " ++ show keyType ++ ", wanted " ++ show resType
    where resType = typeRep (Proxy :: Proxy a)

fromValue :: forall a . Typeable a => Value -> a
fromValue Value{..}
    | valueType == resType = unsafeCoerce valueValue
    | otherwise = throwImpure $ errorInternal $ "fromValue, bad cast, have " ++ show valueType ++ ", wanted " ++ show resType
    where resType = typeRep (Proxy :: Proxy a)

instance Show Key where
    show Key{..} = keyShow keyValue

instance Show Value where
    show Value{..} = valueShow valueValue

instance NFData Key where
    rnf Key{..} = keyRnf keyValue

instance NFData Value where
    rnf Value{..} = valueRnf valueValue

instance Hashable Key where
    hash Key{..} = keyHash (hash keyType) keyValue
    hashWithSalt salt Key{..} = keyHash (hashWithSalt salt keyType) keyValue

instance Eq Key where
    Key{keyType=at,keyValue=a,keyEq=eq} == Key{keyType=bt,keyValue=b}
        | at /= bt = False
        | otherwise = eq a (unsafeCoerce b)
