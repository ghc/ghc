{-# LANGUAGE MagicHash #-}
module T18032 where

import GHC.Exts

-- Different byte content: eqAddr# must be False (0), neAddr# must be True (1)
a = I# (eqAddr# "foo"# "bar"#)
b = I# (neAddr# "foo"# "bar"#)

-- Same variable on both sides: eqAddr# must be True (1), neAddr# must be False (0)
c = let s = "baz"# in I# (eqAddr# s s)
d = let s = "baz"# in I# (neAddr# s s)
