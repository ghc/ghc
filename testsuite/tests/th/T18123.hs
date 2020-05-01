{-# LANGUAGE TemplateHaskell, StandaloneDeriving #-}
module T18123 where

import Language.Haskell.TH

data Point = MkPoint { _x, _y :: Double }
data Rect = MkRect { _p1, _p2 :: Point }

let
    deriveEq :: Name -> DecsQ
    deriveEq name = [d| deriving instance Eq $(conT name) |]
 in
    foldMap deriveEq [ ''Point, ''Rect ]
