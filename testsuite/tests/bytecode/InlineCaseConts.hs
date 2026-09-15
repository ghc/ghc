{-# LANGUAGE MagicHash #-}
-- The case scrutinees are primops the interpreter implements inline, so their
-- alternatives are compiled into the parent BCO instead of separate case
-- continuation BCOs. See Note [Inlined case continuations].
module InlineCaseConts where

import GHC.Exts

f :: Int# -> Int# -> Int
f x y = case x +# y of
          0# -> I# 1#
          r -> case r *# 2# of
                 s -> I# s
