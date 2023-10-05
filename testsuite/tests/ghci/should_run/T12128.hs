{-
    This code produces an empty case statement, which
    panics the bytecode generator after trac #11155.
-}

module ShouldCompile where

import GHC.TypeLits (Symbol)
import Unsafe.Coerce

instance Read Symbol where
     readsPrec = unsafeCoerce (readsPrec :: Int -> ReadS String)

data Bar = TyCon !Symbol deriving (Read)
