-- Test for Trac #2188

module TH_scope where

f g = [d| f :: Int
          f = g
          g :: Int
          g = 4 |]
