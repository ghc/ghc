-- !! This is fine in Haskell 1.4
--
module Foo ( Baz(..) ) where 

class Baz a where
    opx :: Int -> Bar -> a -> a

data Bar = Bar X
data X = Y
