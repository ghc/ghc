{-# LANGUAGE GADTSyntax, TemplateHaskell, KindSignatures #-}

module T5882 where
data Foo :: * -> * where
   Foo :: a -> Foo a

$( [d|  data Bar :: * -> * where
           Bar :: a -> Bar a
  |] )

f (Bar x) = Foo x
