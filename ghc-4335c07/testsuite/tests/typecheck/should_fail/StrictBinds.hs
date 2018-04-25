{-# LANGUAGE MagicHash #-}

module StrictBinds where

import GHC.Exts

foo = let x = 3# +# y
          y = x in
      True
