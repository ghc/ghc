--!! The class export shouldn't be allowed to succeed
--
module Foo ( Baz(..) ) where 

class Baz a where
    opx :: Int -> Bar -> a -> a

data Bar = Bar X
data X = Y
