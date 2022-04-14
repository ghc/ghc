{-# OPTIONS_GHC -fobject-code #-}

module GetTy (getTy) where

import Lib

getTy :: Ty -> Int
getTy (Ty (Just n)) = n
getTy (Ty Nothing) = error "uh oh"
