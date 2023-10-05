module LayoutIn3a where

--Layout rule applies after 'where','let','do' and 'of'

--In this Example: rename 'x' after 'let'  to 'anotherX'.

foo x = let x = 12 in (
                                    x            ) where   y = 2
                                                           --there is a comment.
                                                           w = x
                                                             where
                                                               x = let y = 5 in y + 3

