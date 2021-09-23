{-# LANGUAGE TemplateHaskell #-}

module Main where

class C t where
  meth :: t ()

$( const mempty (meth :: forall t. C t => t ()) )
