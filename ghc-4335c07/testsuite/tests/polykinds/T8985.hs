{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies, GADTs, TypeOperators #-}

module T8905 where

data X (xs :: [k]) = MkX
data Y :: (k -> *) -> [k] -> * where
  MkY :: f x -> Y f (x ': xs)

type family F (a :: [[*]]) :: *
type instance F xss = Y X xss

works :: Y X '[ '[ ] ] -> ()
works (MkY MkX) = ()

fails :: F '[ '[ ] ] -> ()
fails (MkY MkX) = ()
