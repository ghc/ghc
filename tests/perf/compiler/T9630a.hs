{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------
-- | Modified from cereal, which is
-- Copyright   : Lennart Kolmodin, Galois Inc. 2009
-- License     : BSD3-style

module T9630a (
      Serialize(..), GSerialize (..), Putter, Get
    ) where

import Data.ByteString.Builder (Builder)
import Data.ByteString as B
import GHC.Generics
import Control.Applicative (Applicative (..), (<$>))

class Serialize t where
    put :: Putter t
    get :: Get t

instance Serialize () where
    put ()  = pure ()
    get     = pure ()

-- Generics

class GSerialize f where
    gPut :: Putter (f a)
    gGet :: Get (f a)

instance (GSerialize a, GSerialize b) => GSerialize (a :*: b) where
    gPut (a :*: b) = gPut a *> gPut b
    gGet = (:*:) <$> gGet  <*> gGet

instance GSerialize a => GSerialize (M1 i c a) where
    gPut = gPut . unM1
    gGet = M1 <$> gGet

instance Serialize a => GSerialize (K1 i a) where
    gPut = put . unK1
    gGet = K1 <$> get


-- Put

data PairS a = PairS a !Builder

newtype PutM a = Put { unPut :: PairS a }

type Put = PutM ()

type Putter a = a -> Put

instance Functor PutM where
        fmap f m = Put $ let PairS a w = unPut m in PairS (f a) w

instance Applicative PutM where
        pure a = Put (PairS a mempty)

        m <*> k = Put $
            let PairS f w  = unPut m
                PairS x w' = unPut k
            in PairS (f x) (w `mappend` w')

-- Get

data Result r = Fail String B.ByteString
              | Partial (B.ByteString -> Result r)
              | Done r B.ByteString


newtype Get a = Get
  { unGet :: forall r. Input -> Buffer -> More
                    -> Failure r -> Success a r
                    -> Result r }

type Input  = B.ByteString
type Buffer = Maybe B.ByteString

type Failure   r = Input -> Buffer -> More -> [String] -> String -> Result r
type Success a r = Input -> Buffer -> More -> a                  -> Result r

data More
  = Complete
  | Incomplete (Maybe Int)
    deriving (Eq)


instance Functor Get where
    fmap p m =        Get $ \ s0 b0 m0 kf ks ->
      unGet m s0 b0 m0 kf $ \ s1 b1 m1 a     -> ks s1 b1 m1 (p a)

instance Applicative Get where
    pure a = Get $ \ s0 b0 m0 _ ks -> ks s0 b0 m0 a

    f <*> x =         Get $ \ s0 b0 m0 kf ks ->
      unGet f s0 b0 m0 kf $ \ s1 b1 m1 g     ->
      unGet x s1 b1 m1 kf $ \ s2 b2 m2 y     -> ks s2 b2 m2 (g y)
