{-# LANGUAGE TemplateHaskell #-}

module Main where

data D = C { f :: () }

$( const mempty f )
