module T16288A where

import T16288C

data License

class Pretty a where
  pretty :: a -> Doc

instance Pretty License where
  pretty _  = pretV

bar :: (Pretty a) => a -> Doc
bar w = foo (pretty (u w w w w))

u :: a -> a -> a -> a -> a
u = u
