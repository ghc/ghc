{-# LANGUAGE RecordWildCards, MonadComprehensions, NamedFieldPuns #-}

module T3901 where

data Rec = Rec {a :: Int} deriving (Show)

recs1 = [a | Rec {a=a} <- [Rec 1], then group by a]

recs2 = [a | Rec {a} <- [Rec 1], then group by a]

recs3 = [a | Rec {..} <- [Rec 1], then group by a]

recs4 :: [[Int]]
recs4 = [a | Rec {..} <- [Rec 1], then group by a]
