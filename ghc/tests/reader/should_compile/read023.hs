module ShouldCompile where

-- M.<keyword> isn't a qualified identifier
f  = Just.let x=id in x
f' = Just.\1 where (.\) = ($)

-- M.{as,hiding,qualified} *are* qualified identifiers
g  = ShouldCompile.as
g' = (ShouldCompile.!)

as x = x
(!) x = x
