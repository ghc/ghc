module T22494 where

-- After simplification we should get foo more or less as-is
-- and not
--     lvl = error "wombat"
--     foo = case lvl of { ... }

foo = case error "wombat" of { True -> "fred"; False -> "bill" }
