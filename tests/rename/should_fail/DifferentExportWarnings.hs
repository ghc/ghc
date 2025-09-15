{-# LANGUAGE PatternSynonyms #-}
module DifferentExportWarnings (
    {-# DEPRECATED "test" #-} Foo(..),
    {-# DEPRECATED "test" #-} Foo(Foo1),
    {-# WARNING "test" #-} Foo(Foo2),
    {-# WARNING in "x-cat" "test2" #-} Bar(Dummy),
    {-# WARNING in "x-cat2" "test2" #-} Bar,
    {-# WARNING "test3-a" #-} module DifferentExportWarningsA,
    {-# WARNING "test3-b" #-} module DifferentExportWarningsA,
    {-# WARNING "test3-b" #-} x
  ) where

import DifferentExportWarningsA (x, y)

data Foo = Foo1 | Foo2
data Bar = Bar1 | Bar2
pattern Dummy = Bar1