{-# LANGUAGE LambdaCase #-}

module ParserLambdaCase where

f1 = \case "1" -> 1
f2 = \ {- comment1 {- comment2 -} -} case "1" -> 1; "2" -> 2
f3 = \ -- comment
       case "1" -> 1
            "2" -> 2
f4 = \casex -> casex
f5 = \ case { "1" -> 1; "2" -> 2 }

