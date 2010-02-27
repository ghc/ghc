
{-# LANGUAGE EmptyDataDecls #-}

-- This used to give warnings:
--     ld: atom sorting error for _Main_MyType_closure_tbl and _Main_MyType2_closure_tbl in q.o
--     ld: atom sorting error for _Main_MyType_closure_tbl and _Main_MyType2_closure_tbl in q.o
--     ld: atom sorting error for _Main_MyType_closure_tbl and _Main_MyType2_closure_tbl in q.o
-- when compiling on OS X (trac #2578).

module Main (main) where

data MyType
data MyType2

main :: IO ()
main = print ()

