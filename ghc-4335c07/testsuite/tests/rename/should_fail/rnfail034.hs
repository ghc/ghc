-- !!! qualified names aren't allowed in local binds either
-- (Haskell 98 (revised) section 5.5.1)
module M where
g x = let M.y = x + 1 in M.y
