module T25749 where

data D = K0 | K1 { fld :: Int }

foo :: D -> Int
foo K0 = 3
foo d
  | let i = fld d
  = let j = fld d
    in i + j + k
  where k = fld d

bar :: D -> Int
bar d@(K1 {})
  | let i | let i' = fld d = i'
  = let j = fld d in i + j + k
  where k = fld d
bar _ = 3
