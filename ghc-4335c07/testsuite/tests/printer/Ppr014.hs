{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- from https://ocharles.org.uk/blog/guest-posts/2014-12-19-existential-quantification.html

data HashMap k v = HM  -- ... -- actual implementation

class Hashable v where
  h :: v -> Int

data HashMapM hm = HashMapM
  { empty  :: forall k v . hm k v
  , lookup :: Hashable k => k -> hm k v -> Maybe v
  , insert :: Hashable k => k -> v -> hm k v -> hm k v
  , union  :: Hashable k => hm k v -> hm k v -> hm k v
  }


data HashMapE = forall hm . HashMapE (HashMapM hm)

-- public
mkHashMapE :: Int -> HashMapE
mkHashMapE = HashMapE . mkHashMapM

-- private
mkHashMapM :: Int -> HashMapM HashMap
mkHashMapM salt = HashMapM { {- implementation -} }

instance Hashable String where

type Name = String
data Gift = G String

giraffe :: Gift
giraffe = G "giraffe"

addGift :: HashMapM hm -> hm Name Gift -> hm Name Gift
addGift mod gifts =
  let
    HashMapM{..} = mod
  in
    insert "Ollie" giraffe gifts

-- -------------------------------

santa'sSecretSalt = undefined
sendGiftToOllie = undefined
traverse_ = undefined

sendGifts =
  case mkHashMapE santa'sSecretSalt of
    HashMapE (mod@HashMapM{..}) ->
      let
        gifts = addGift mod empty
      in
        traverse_ sendGiftToOllie $ lookup "Ollie" gifts
