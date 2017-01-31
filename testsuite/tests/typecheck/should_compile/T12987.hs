{-# LANGUAGE TypeInType #-}

module T12987 where

import GHC.Exts

class NUM (a :: TYPE rep) where add :: a -> a -> a
