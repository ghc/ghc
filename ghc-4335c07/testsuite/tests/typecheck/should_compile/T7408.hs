{-# OPTIONS_GHC -Wall -Werror#-}
module T7408 (Context, mkContext) where
import Foreign

newtype Context = Context (Ptr ())
foreign import ccall mkContext :: IO Context
