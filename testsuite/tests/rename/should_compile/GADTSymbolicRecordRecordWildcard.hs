{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE RecordWildCards #-}

module GADTSymbolicRecordRecordWildcard where

data G a where
  (:&) :: {fld1 :: Int, fld2 :: a} -> G a

f :: G Int -> Int
f (:&) {..} = fld1 + fld2
