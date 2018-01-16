module ShouldCompile where

-- !!! test the overlapping patterns detection.

-- f1 overlaps
f1 "ab" = []
f1 "ab" = []
f1 _ = []

-- f2 overlaps
f2 "ab" = []
f2 ('a':'b':[]) = []
f2 _ = []

-- f3 overlaps
f3 ('a':'b':[]) = []
f3 "ab" = []
f3 _ = []

-- f4 doesn't overlap
f4 "ab" = []
f4 ('a':'b':'c':[]) = []
f4 _ = []

-- f5 doesn't overlap
f5 ('a':'b':'c':[]) = []
f5 "ab" = []
f5 _ = []

-- f6 doesn't overlap
f6 "ab" = []
f6 ('a':[]) = []
f6 _ = []
