-- Test overlapping pattern warnings

module ShouldCompile where

f x = case x of 
	Just (~1) -> 0
        Just _    -> 1	-- This one cannot match
        Nothing   -> 2
