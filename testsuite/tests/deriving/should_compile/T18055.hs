{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Bug where

import Data.Kind

-----

data Block m = Block

class NoThunks m where

newtype AllowThunk a = AllowThunk a

class GetHeader blk where
  data family Header blk :: Type

instance GetHeader (Block m) where
  newtype Header (Block m) = BlockHeader { main :: Header m }
    deriving NoThunks via AllowThunk (Header (Block m))

-----

class C a where
  data D a

class X a b

instance C (Maybe a) where
  data D (Maybe a) deriving (X a)

instance C [a] where
  newtype D [a] = MkDList Bool

newtype MyList a = MkMyList [a]

instance C (MyList a) where
  newtype D (MyList a) = MkDMyList Bool
    deriving (X a) via D [a]
