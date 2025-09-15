module LayoutIn3b where

--Layout rule applies after 'where','let','do' and 'of'

--In this Example: rename 'x' after 'let'  to 'anotherX'.

foo x = let anotherX = 12 in (             anotherX            ) where   y = 2
                                                                         --there is a comment.
                                                                         w = x
                                                                           where
                                                                             x = let y = 5 in y + 3

