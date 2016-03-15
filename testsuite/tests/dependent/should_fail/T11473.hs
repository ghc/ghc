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

hello :: forall (r :: RuntimeRep). forall (a :: TYPE r). BoxIt a => a -> Boxed a
hello x = boxed x
{-# NOINLINE hello #-}

main :: IO ()
main = do
    print $ boxed 'c'#
    print $ boxed 'c'
    print $ hello 'c'
    print $ hello 'c'#
