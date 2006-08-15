{-# OPTIONS -fffi -fglasgow-exts #-}

-- Kind error messsage should not contain bangs

module ShouldFail where

import Foreign.ForeignPtr

data Foo = Foo {-# UNPACK #-} !(ForeignPtr)

 
