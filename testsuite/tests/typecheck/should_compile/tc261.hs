{-# LANGUAGE TypeOperators #-}
module TcOK where

newtype (f <.> g) a = Compose (f (g a))
