-- Check that declarations in a bracket shadow the top-level 
-- declarations, rather than clashing with them.

module TH_bracket1 where

foo = 1
bar = [d| foo = 1 |]
