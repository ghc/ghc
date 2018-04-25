{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
module T11616 where

class Whoami a where
  whoami :: String

instance Whoami Int where
  whoami = "int"

instance Whoami Bool where
  whoami = "[y/n]"

instance Whoami Maybe where
  whoami = "call me maybe"

whoisint :: String
whoisint = whoami @Int
