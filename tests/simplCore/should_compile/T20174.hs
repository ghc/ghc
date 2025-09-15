{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -ddump-simpl -dsuppress-all -dno-typeable-binds -O2 #-}

module Test (foobar, foobar2, foobar3) where

import GHC.Exts

bar :: String
bar = unpackCString# "bar"#

foobar :: String
foobar = unpackAppendCString# "foo"# bar

foobar2 :: String
foobar2 = unpackAppendCString# "foo"# (unpackCString# "bar"#)

foobar3 :: String
foobar3 = unpackAppendCString# "foo"# (unpackAppendCString# "bar"# [])
