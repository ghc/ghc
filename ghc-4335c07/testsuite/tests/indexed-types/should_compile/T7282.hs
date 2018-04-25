 {-# OPTIONS -XTypeFamilies -XDataKinds -XPolyKinds #-}

module T7282 where

class Foo (xs :: [k]) where
     type Bar xs :: *

instance Foo '[] where
     type Bar '[] = Int
