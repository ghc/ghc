{-# LANGUAGE PatternSynonyms #-}
class C a where
    method :: a

instance C Int where
    pattern P = ()
