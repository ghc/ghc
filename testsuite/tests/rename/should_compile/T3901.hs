{-# LANGUAGE RecordWildCards, TransformListComp, NamedFieldPuns #-}

module T3901 where

import GHC.Exts (groupWith)

data Rec = Rec {a :: Int} deriving (Show)

recs1 = [a | Rec {a=a} <- [Rec 1], then group by a using groupWith]

recs2 = [a | Rec {a} <- [Rec 1], then group by a using groupWith]

recs3 = [a | Rec {..} <- [Rec 1], then group by a using groupWith]

recs4 :: [[Int]]
recs4 = [a | Rec {..} <- [Rec 1], then group by a using groupWith]
