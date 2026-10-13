{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -DTEST #-}
{-# OPTIONS_GHC -Wno-such-warning #-} -- warned about exactly once
#if defined(TEST)
{-# LANGUAGE MagicHash #-}
#endif

module Test2464 where

import GHC.Exts

f :: Int# -> Int#
f x = x +# 1#
