{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module T12457_aux where

import GHC.Internal.TH.Lib
import GHC.Internal.TH.Syntax

class C a where
  type Assoc a
  m :: a -> Int
  n :: a -> Int

instance DeriveTH C where
  deriveTH _p head = do -- head will be [| C (T a)|]
    -- runIO $ print head
    let ConT c `AppT` (ConT t `AppT` VarT a) = head
    x <- newName "x"
    x2 <- newName "x"
    addTopDecls =<< [d|
      $(varP x) = 12
      $(varP x2) = 23 |]
    q <- [d|
      instance C $(varT a) => C ($(conT t) $(varT a)) where
        type Assoc ($(conT t) $(varT a)) = Char
        m :: a -> Int
        m _ = $(varE x) + 42
        {-# INLINE m #-}
        n :: a -> Int
        n _ = $(varE x2) + 13 |]
    -- runIO $ print q
    return q
