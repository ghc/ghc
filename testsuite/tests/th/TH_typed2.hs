{-# LANGUAGE TemplateHaskell #-}

module Main where

import Language.Haskell.TH

main = print $( typedSpliceE $ typedBracketE [| 'y' |] )
