{-# LANGUAGE PatternSynonyms #-}
module PprExportWarning (
        {-# WARNING "Just because I can but a really long message" #-}
        Foo(..),
        {-# DEPRECATED "Just because" #-}
        reallyreallyreallyreallyreallyreallyreallyreallylongname,
        {-# DEPRECATED "Just because" #-} Bar(Bar1, Bar2),
        {-# WARNING "Just because" #-} name,
        {-# DEPRECATED ["Reason",
                        "Another reason"] #-}
        Baz,
        {-# DEPRECATED [ ] #-} module GHC,
        {-# WARNING "Dummy Pattern" #-} pattern Dummy,
        Foo'(..),
        reallyreallyreallyreallyreallyreallyreallyreallylongname',
        Bar'(Bar1, Bar2), name', Baz', module Data.List, pattern Dummy'
    ) where
import GHC
import Data.List
data Foo = Foo1 | Foo2 | Foo3
reallyreallyreallyreallyreallyreallyreallyreallylongname = undefined
data Bar = Bar1 | Bar2 | Bar3
name = undefined
data Baz
pattern Dummy = Foo1
data Foo' = Foo1 | Foo2 | Foo3
reallyreallyreallyreallyreallyreallyreallyreallylongname' = undefined
data Bar' = Bar1 | Bar2 | Bar3
name' = undefined
data Baz'
pattern Dummy' = Foo1
