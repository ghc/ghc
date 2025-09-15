{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module Main where

data family F a

data instance F () = C ()

$( const mempty C )
