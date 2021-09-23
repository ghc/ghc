{-# LANGUAGE TemplateHaskell #-}

module Main where

data D = C ()

$( const mempty C )
