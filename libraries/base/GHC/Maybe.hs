{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Maybe type
module GHC.Maybe
   ( Maybe (..)
   , maybe
   , isJust
   , isNothing
   , fromMaybe
   )
where

import GHC.Num.Integer () -- See Note [Depend on GHC.Num.Integer] in GHC.Base
import GHC.Types
import GHC.Classes

default ()

-------------------------------------------------------------------------------
-- Maybe type
-------------------------------------------------------------------------------

-- | The 'Maybe' type encapsulates an optional value.  A value of type
-- @'Maybe' a@ either contains a value of type @a@ (represented as @'Just' a@),
-- or it is empty (represented as 'Nothing').  Using 'Maybe' is a good way to
-- deal with errors or exceptional cases without resorting to drastic
-- measures such as 'Prelude.error'.
--
-- The 'Maybe' type is also a monad.  It is a simple kind of error
-- monad, where all errors are represented by 'Nothing'.  A richer
-- error monad can be built using the 'Data.Either.Either' type.
--
data  Maybe a  =  Nothing | Just a
  deriving ( Eq  -- ^ @since 2.01
           , Ord -- ^ @since 2.01
           )

-- | The 'maybe' function takes a default value, a function, and a 'Maybe'
-- value.  If the 'Maybe' value is 'Nothing', the function returns the
-- default value.  Otherwise, it applies the function to the value inside
-- the 'Just' and returns the result.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> maybe False odd (Just 3)
-- True
--
-- >>> maybe False odd Nothing
-- False
--
-- Read an integer from a string using 'Text.Read.readMaybe'. If we succeed,
-- return twice the integer; that is, apply @(*2)@ to it. If instead
-- we fail to parse an integer, return @0@ by default:
--
-- >>> import Text.Read ( readMaybe )
-- >>> maybe 0 (*2) (readMaybe "5")
-- 10
-- >>> maybe 0 (*2) (readMaybe "")
-- 0
--
-- Apply 'Prelude.show' to a @Maybe Int@. If we have @Just n@, we want to show
-- the underlying 'Int' @n@. But if we have 'Nothing', we return the
-- empty string instead of (for example) \"Nothing\":
--
-- >>> maybe "" show (Just 5)
-- "5"
-- >>> maybe "" show Nothing
-- ""
--
maybe :: b -> (a -> b) -> Maybe a -> b
maybe n _ Nothing  = n
maybe _ f (Just x) = f x

-- | The 'isJust' function returns 'True' iff its argument is of the
-- form @Just _@.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> isJust (Just 3)
-- True
--
-- >>> isJust (Just ())
-- True
--
-- >>> isJust Nothing
-- False
--
-- Only the outer constructor is taken into consideration:
--
-- >>> isJust (Just Nothing)
-- True
--
isJust         :: Maybe a -> Bool
isJust Nothing = False
isJust _       = True

-- | The 'isNothing' function returns 'True' iff its argument is 'Nothing'.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> isNothing (Just 3)
-- False
--
-- >>> isNothing (Just ())
-- False
--
-- >>> isNothing Nothing
-- True
--
-- Only the outer constructor is taken into consideration:
--
-- >>> isNothing (Just Nothing)
-- False
--
isNothing         :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

-- | The 'fromMaybe' function takes a default value and a 'Maybe'
-- value.  If the 'Maybe' is 'Nothing', it returns the default value;
-- otherwise, it returns the value contained in the 'Maybe'.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> fromMaybe "" (Just "Hello, World!")
-- "Hello, World!"
--
-- >>> fromMaybe "" Nothing
-- ""
--
-- Read an integer from a string using 'Text.Read.readMaybe'. If we fail to
-- parse an integer, we want to return @0@ by default:
--
-- >>> import Text.Read ( readMaybe )
-- >>> fromMaybe 0 (readMaybe "5")
-- 5
-- >>> fromMaybe 0 (readMaybe "")
-- 0
--
fromMaybe     :: a -> Maybe a -> a
fromMaybe d x = case x of {Nothing -> d;Just v  -> v}
