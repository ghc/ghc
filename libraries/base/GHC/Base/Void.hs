{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDeriving #-}

module GHC.Base.Void
    ( Void
    , absurd
    , vacuous
    ) where

-- ghc-prim
import GHC.Classes

-- base
import GHC.Base.Functor
import GHC.Base.Semigroup

-- | Uninhabited data type
--
-- @since 4.8.0.0
data Void deriving
  ( Eq      -- ^ @since 4.8.0.0
  , Ord     -- ^ @since 4.8.0.0
  )

-- | @since 4.9.0.0
instance Semigroup Void where
    a <> _ = a
    stimes _ a = a

-- | Since 'Void' values logically don't exist, this witnesses the
-- logical reasoning tool of \"ex falso quodlibet\".
--
-- >>> let x :: Either Void Int; x = Right 5
-- >>> :{
-- case x of
--     Right r -> r
--     Left l  -> absurd l
-- :}
-- 5
--
-- @since 4.8.0.0
absurd :: Void -> a
absurd a = case a of {}

-- | If 'Void' is uninhabited then any 'Functor' that holds only
-- values of type 'Void' is holding no values.
-- It is implemented in terms of @fmap absurd@.
--
-- @since 4.8.0.0
vacuous :: Functor f => f Void -> f a
vacuous = fmap absurd

