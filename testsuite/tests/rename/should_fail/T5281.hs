-- The issue here is getting an error message with a decent source location
module T5281 where

x = catch   -- This use of catch 
            -- (a) comes from the implicitly imported Prelue
            -- (b) this Prelude.catch is deprecated


