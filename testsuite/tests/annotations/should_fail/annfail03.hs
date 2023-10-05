{-# LANGUAGE DeriveDataTypeable #-}

module Annfail03 where
-- Testing annotating something with a value defined + Typeabled / Dataed in same module

import Data.Data
import Data.Typeable

data InModule = InModule
              deriving (Typeable, Data)

{-# ANN module InModule #-}

{-# ANN type Foo InModule #-}
data Foo = Bar

{-# ANN f InModule #-}
f x = x