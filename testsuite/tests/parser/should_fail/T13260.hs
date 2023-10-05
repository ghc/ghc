{-# LANGUAGE MagicHash #-}

module T13260 where

  g y = case y of
    "a"# -> True
    _    -> False
