{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Prelude
import qualified Foreign.Storable as Storable
import qualified Control.Monad.State.Strict as S
import Control.Monad.IO.Class
import Foreign.Marshal.Alloc       (mallocBytes)

newtype Foo a = Foo a

intSize :: Int
intSize = Storable.sizeOf (undefined :: Int)

-- This 'go' loop should allocate nothing, because it specialises
-- for the shape of the state.  But in 8.4 it did (#14936)

slow :: Int -> IO ()
slow i = do let go 0 = pure ()
                go j = do Foo (!a, !off) <- S.get
                          S.put (Foo (a+1, off))
                          go (j - 1)
            S.evalStateT (go i) (Foo ((0::Int),(intSize::Int)))

main = do { slow (10 ^ 7); print "Done" }

