module T18497_Foo where

import {-# SOURCE #-} T18497_Bar

data X = SomeX Y Y | NoX

foo :: X
foo = SomeX blah (woop NoX)
