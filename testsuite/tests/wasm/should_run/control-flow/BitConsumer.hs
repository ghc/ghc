{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module BitConsumer
  ( BitConsumer
  , ConsumptionResult(..)
  , runWithBits
  , eventsFromBits

  , rangeSelect
  , inverseRangeSelect
  )
where

-- A "bit consumer" simulates a computation.
-- It can be run by supplying it with a sequence of Booleans.
-- The Booleans determine the results of observations that
-- drive control-flow decisions (a Boolean in an `if` and
-- an integer in a `switch`).

import ControlTestMonad

import GHC.Utils.Panic

data ConsumptionResult stmt exp a
    = Produced { pastEvents :: [Event stmt exp], value :: a }
    | Halted { pastEvents :: [Event stmt exp] }
    | Failed { pastEvents :: [Event stmt exp], msg :: String }

instance Functor (ConsumptionResult s e) where
  fmap f (Produced events a) = Produced events (f a)
  fmap _ (Halted events) = Halted events
  fmap _ (Failed events msg) = Failed events msg

instance (Show exp, Show stmt, Show a) => Show (ConsumptionResult stmt exp a) where
  show (Produced events a) = show events ++ " -> " ++ show a
  show (Halted events) = show events ++ " EXHAUSTS"
  show (Failed events msg) = show events ++ "  FAILED: " ++ msg

reverseEvents :: ConsumptionResult stmt exp a -> ConsumptionResult stmt exp a
reverseEvents (Produced events a) = Produced (reverse events) a
reverseEvents (Halted events) = Halted (reverse events)
reverseEvents (Failed events msg) = Failed (reverse events) msg


newtype BitConsumer stmt exp a =
    BC { unBC :: [Bool] -> [Event stmt exp] -> (ConsumptionResult stmt exp a, [Bool]) }

instance Functor (BitConsumer stmt exp) where
  fmap f ma = BC $ \bits past -> update $ unBC ma bits past
    where update (l, r) = (fmap f l, r)

instance Applicative (BitConsumer stmt exp) where
  pure a = BC $ \bits past -> (Produced past a, bits)
  mf <*> ma = do { f <- mf; f <$> ma }

instance Monad (BitConsumer stmt exp) where
  m >>= k = BC $ \bits past ->
                 case unBC m bits past of
                   (Produced past' a, bits') -> unBC (k a) bits' past'
                   (Halted past, bits') -> (Halted past, bits')
                   (Failed past msg, bits') -> (Failed past msg, bits')

instance MonadFail (BitConsumer stmt exp) where
  fail msg = BC $ \bits past -> (Failed past msg, bits)


runWithBits :: BitConsumer stmt exp a -> [Bool] -> ConsumptionResult stmt exp a
-- ^ Run with Booleans determining decisions, return final
-- state with oldest event first
runWithBits m bits = reverseEvents $ fst $ unBC m bits []

eventsFromBits :: BitConsumer stmt exp () -> [Bool] -> [Event stmt exp]
eventsFromBits bc = pastEvents . runWithBits bc


instance ControlTestMonad stmt exp (BitConsumer stmt exp) where
  evalPredicate lbl =
      BC $ \bits past -> case bits of
                           bit : bits' -> (Produced (Predicate lbl bit : past) bit, bits')
                           [] -> (Halted past, bits)

  evalEnum lbl range =
      BC $ \bits past -> case rangeSelect range bits of
                           Just (i, bits') -> (Produced (Switch lbl range i : past) i, bits')
                           Nothing -> (Halted past, bits)

  takeAction lbl = BC $ \bits past -> (Produced (Action lbl : past) (), bits)


rangeSelect :: (Integer, Integer) -> [Bool] -> Maybe (Integer, [Bool])
rangeSelect (lo, limit) bits | lo == pred limit = Just (lo, bits)
rangeSelect _ [] = Nothing
rangeSelect (lo, limit) (bit : bits) =
    rangeSelect (if bit then (lo, mid) else (mid, limit)) bits
  where mid = (lo + limit) `div` 2

inverseRangeSelect :: (Integer, Integer) -> Integer -> [Bool]
inverseRangeSelect (lo, limit) i
    | lo == pred limit = if i == lo then [] else panic "inverseRangeSelect"
    | otherwise = if i < mid then True : inverseRangeSelect (lo, mid) i
                  else False : inverseRangeSelect (mid, limit) i
  where mid = (lo + limit) `div` 2
