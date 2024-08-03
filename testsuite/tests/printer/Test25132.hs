{-# LANGUAGE MultilineStrings #-}

module Test25132 where

import Data.Proxy

v :: Proxy """
           this is a
           multiline
           string
           """
v = Proxy @"""
           this is a
           multiline
           string
           """
