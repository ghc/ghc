{-# LANGUAGE PolyKinds, TypeFamilies, MagicHash, DataKinds, TypeInType, RankNTypes #-}

module T11473 where
import GHC.Exts
import GHC.Types

type family Boxed (a :: k) :: *
type instance Boxed Char# = Char
type instance Boxed Char  = Char

class BoxIt (a :: TYPE lev) where
    boxed :: a -> Boxed a

instance BoxIt Char# where boxed x = C# x
instance BoxIt Char  where boxed = id

-- This should be an error: there is no way we can produce code for both Lifted
-- and Unlifted levities
hello :: forall (lev :: Levity). forall (a :: TYPE lev). BoxIt a => a -> Boxed a
hello x = boxed x
