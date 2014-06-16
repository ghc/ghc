{-# LANGUAGE QuasiQuotes #-}
module Test where

import QQ

f :: [pq| foo |]    -- Expands to Int -> Int
[pq| blah |]	    -- Expands to f x = x

h [pq| foo |] = f [pq| blah |] * 8	
	-- Expands to h (Just x) = f (x+1) * 8



