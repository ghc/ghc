module T18923 (mergeRec, Rec) where

mayMerge :: Maybe b -> Maybe b -> Maybe b
mayMerge Nothing  y        = y
mayMerge x        Nothing  = x
mayMerge (Just x) (Just y) = Just y

data Rec = Rec { v0,v1,v2,v3,v4,v5,v6,v7 :: !(Maybe Bool) }

mergeRec :: Rec -> Rec -> Rec
mergeRec
  (Rec a0 a1 a2 a3 a4 a5 a6 a7)
  (Rec b0 b1 b2 b3 b4 b5 b6 b7) =
    Rec (mayMerge a0 b0) (mayMerge a1 b1) (mayMerge a2 b2) (mayMerge a3 b3)
        (mayMerge a4 b4) (mayMerge a5 b5) (mayMerge a6 b6) (mayMerge a7 b7)

