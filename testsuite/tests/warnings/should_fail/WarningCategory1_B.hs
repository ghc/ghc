module WarningCategory1_B where

{-# WARNING in "x-dangerous" foo "foo is dangerous" ;
            in "x-mostly-harmless" bar "bar is mostly harmless" #-}
foo :: ()
foo = foo

bar :: ()
bar = undefined

{-# WARNING in "x-harmless" baz "baz is harmless" #-}
baz :: ()
baz = ()

{-# WARNING quux "quux has no category" #-}
quux :: ()
quux = ()

{-# DEPRECATED plugh "plugh is deprecated" #-}
plugh :: ()
plugh = ()
