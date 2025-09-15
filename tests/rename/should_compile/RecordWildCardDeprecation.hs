{-# LANGUAGE RecordWildCards #-}
module RecordWildCardDeprecation where

import RecordWildCardDeprecation_aux

f (Foo { .. }) = let a = x in a

g (Foo { .. }) = let a = y in a

h (Foo { .. }) = let a = z in a