module ShouldCompile where

-- M.<keyword> isn't a qualified identifier
f  = Just.let x=id in x

-- ---------------------------------------------------------------------------
-- we changed the behaviour of this one in GHC, but the following test
-- is strictly speaking legal Haskell:

-- f' = Just.\1 where (.\) = ($)

-- -----------------------------------------------------
-- M.{as,hiding,qualified} *are* qualified identifiers:

g  = ShouldCompile.as

-- ---------------------------------------------------------------------------
-- special symbols (!, -) can be qualified to make varids.

g' = (ShouldCompile.!)

as x = x
(!) x = x
