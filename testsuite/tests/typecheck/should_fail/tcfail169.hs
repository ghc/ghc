
-- Trac #958

module ShoulFail where

data Succ a = S a 	-- NB: deriving Show omitted
data Seq a = Cons a (Seq (Succ a)) | Nil deriving Show

