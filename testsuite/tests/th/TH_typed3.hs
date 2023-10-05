{-# LANGUAGE TemplateHaskell #-}

module Main where

import Language.Haskell.TH

-- test parenthesis around splice
main = do
    print $( typedSpliceE $ typedBracketE [| 'z' |] )
    print $( typedSpliceE $ appE [| id |] (typedBracketE [| 'z' |]) )
