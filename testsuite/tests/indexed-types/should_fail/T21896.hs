{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds, TypeFamilies, TypeFamilyDependencies, PolyKinds #-}
module Bug where

data T = Foo | Bar

type family F (ns :: T) (ret :: k) = (r :: k) | r -> ret where
    F Foo r = r
    F Bar r = r
