{-# LANGUAGE DuplicateRecordFields, TemplateHaskell, NoMonomorphismRestriction #-}

data S = MkS { x :: Int }
data T = MkT { x :: Int }

-- This tests what happens when an ambiguous record update is used in
-- a splice: since it can't be represented in TH, it should error
-- cleanly, rather than panicking or silently using one field.
foo = [e| (MkS 3) { x = 3 } |]

main = return ()
