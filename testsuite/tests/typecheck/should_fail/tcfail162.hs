


-- Kind error message should not contain bangs

module ShouldFail where

import Foreign.ForeignPtr

data Foo = Foo {-# UNPACK #-} !(ForeignPtr)

 
