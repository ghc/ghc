module T18497_Bar where

import T18497_Foo

data Y = SomeY X | NoY

blah :: Y
blah = NoY

blip :: Y
blip = SomeY foo

woop NoX         = NoY
woop (SomeX y _) = y
