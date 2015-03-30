{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module SingletonsBug where

import Control.Applicative
import Data.Traversable (for)
import GHC.Exts( Constraint )

-----------------------------------
-- From 'constraints' library
-- import Data.Constraint (Dict(..))
data Dict :: Constraint -> * where
  Dict :: a => Dict a

-----------------------------------
-- From 'singletons' library
-- import Data.Singletons hiding( withSomeSing )

class SingI (a :: k) where
  -- | Produce the singleton explicitly. You will likely need the @ScopedTypeVariables@
  -- extension to use this method the way you want.
  sing :: Sing a

data family Sing (a :: k)

data KProxy (a :: *) = KProxy

data SomeSing (kproxy :: KProxy k) where
  SomeSing :: Sing (a :: k) -> SomeSing ('KProxy :: KProxy k)

-- SingKind :: forall k. KProxy k -> Constraint
class (kparam ~ 'KProxy) => SingKind (kparam :: KProxy k) where
  -- | Get a base type from a proxy for the promoted kind. For example,
  -- @DemoteRep ('KProxy :: KProxy Bool)@ will be the type @Bool@.
  type DemoteRep kparam :: *

  -- | Convert a singleton to its unrefined version.
  fromSing :: Sing (a :: k) -> DemoteRep kparam

  -- | Convert an unrefined type to an existentially-quantified singleton type.
  toSing   :: DemoteRep kparam -> SomeSing kparam

withSomeSing :: SingKind ('KProxy :: KProxy k)
             => DemoteRep ('KProxy :: KProxy k)
             -> (forall (a :: k). Sing a -> r)
             -> r
withSomeSing = error "urk"

-----------------------------------

data SubscriptionChannel = BookingsChannel
type BookingsChannelSym0 = BookingsChannel
data instance Sing (z_a5I7 :: SubscriptionChannel) where
   SBookingsChannel :: Sing BookingsChannel

instance SingKind ('KProxy :: KProxy SubscriptionChannel) where
  type DemoteRep ('KProxy :: KProxy SubscriptionChannel) = SubscriptionChannel
  fromSing SBookingsChannel = BookingsChannel
  toSing BookingsChannel = SomeSing SBookingsChannel

instance SingI BookingsChannel where
  sing = SBookingsChannel

type family T (c :: SubscriptionChannel) :: *
type instance T 'BookingsChannel = Bool

witnessC :: Sing channel -> Dict (Show (T channel), SingI channel)
witnessC SBookingsChannel = Dict

forAllSubscriptionChannels
  :: forall m r. (Applicative m)
  => (forall channel. (SingI channel, Show (T channel)) => Sing channel -> m r)
  -> m r
forAllSubscriptionChannels f =
  withSomeSing BookingsChannel $ \(sChannel) ->
    case witnessC sChannel of
      Dict -> f sChannel

