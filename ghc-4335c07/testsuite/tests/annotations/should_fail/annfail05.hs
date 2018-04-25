module Annfail05 where

import Annfail05_Help
-- Testing annotating with a value that is not Typeable or Data-able

{-# ANN module NoInstances #-}

{-# ANN type Foo NoInstances #-}
data Foo = Bar

{-# ANN f NoInstances #-}
f x = x