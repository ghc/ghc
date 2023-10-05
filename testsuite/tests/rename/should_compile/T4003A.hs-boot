{-# LANGUAGE Haskell2010 #-}
module T4003A where

import Data.Data

data HsExpr i

instance Data i => Data (HsExpr i)
