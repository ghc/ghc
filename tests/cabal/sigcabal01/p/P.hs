module P where

import Map

foo = do
    let x = insert 0 "foo"
          . delete 1
          . insert 1 undefined
          . insert (6 :: Int) "foo"
          $ empty
    print (member 1 x)
    print (toList x)
    print x
