module LayoutIn4 where

--Layout rule applies after 'where','let','do' and 'of'

--In this Example: rename 'ioFun' to  'io'

main = ioFun "hello" where ioFun s= do  let  k = reverse s
 --There is a comment
                                        s <- getLine
                                        let  q = (k ++ s)
                                        putStr q
                                        putStr "foo"

