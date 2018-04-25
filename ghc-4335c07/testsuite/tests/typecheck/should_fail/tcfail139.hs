-- Should be rejected by Haskell 98

module Foo  where 

type Foo = Double
instance Bounded Foo
