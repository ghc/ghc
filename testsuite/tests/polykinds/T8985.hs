{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies, GADTs, TypeOperators #-}

module T8985 where

import Data.Kind (Type)

data X (xs :: [k]) = MkX
data Y :: (k -> Type) -> [k] -> Type where
  MkY :: f x -> Y f (x ': xs)

type family F (a :: [[Type]]) :: Type
type instance F xss = Y X xss

works :: Y X '[ '[ ] ] -> ()
works (MkY MkX) = ()

fails :: F '[ '[ ] ] -> ()
fails (MkY MkX) = ()
