{-# Language TemplateHaskell #-}

module T12561A where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Data.IORef

t1 = do
  c1 <- [|| (1::Int) + 2 ||]
  c2 <- [|| 3 + $$(return c1) ||]
  return c2

t2 :: Q (TExp Int)
t2 = do
  r <- runIO $ newIORef undefined
  c1 <- [|| \x -> (1::Int) +
                  $$(do
                     xv <- [||x||]
                     runIO $ writeIORef r xv
                     return xv) ||]
  runIO $ readIORef r
