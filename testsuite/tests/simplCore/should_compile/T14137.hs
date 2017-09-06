module T14137 where

-- The point of this test is that we should inline 'thunk'
-- into j's RHS, and we can do so quite aggressively, even
-- when we aren't optimising. See the ticket.
--
-- It's not a big deal, because in the end FloatIn
-- does the same job, only later

f xs = let thunk = length xs
           j = Just thunk
           g 0 = j
           g n = g (n-1)
       in
       g 7
