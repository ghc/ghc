{-# LANGUAGE TypeFamilies #-}
module Bar where
import qualified T2435Foo as Foo
instance Foo.C Int where type T Int = Int
