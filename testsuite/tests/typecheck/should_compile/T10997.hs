module T10997 where

import T10997a

{- With ghc-7.10.2:

    The interface for ‘T10997a’
    Declaration for Just'
    Pattern synonym Just':
    Iface type variable out of scope:  k
    Cannot continue after interface file error
-}

bar :: (Showable a) => Maybe a -> Maybe a
bar (Just' a) = Just' a
