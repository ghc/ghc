module ExportWarnings_base (
    x, 
    {-# DEPRECATED "warn" #-} T(..), 
    {-# WARNING "no warn" #-} B(B1), 
    B(B2), 
    {-# DEPRECATED [] #-} V
  ) where

x :: Bool
x = True

data T = T1 {y :: Int} | T2 {y :: Int}
data B = B1 | B2
newtype V = V ()