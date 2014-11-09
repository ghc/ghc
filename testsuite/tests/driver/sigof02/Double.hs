import Map
import MapAsSet

main = do
    let x = insert 0 "foo"
          . delete 1
          . insert 1 undefined
          . insert (6 :: Int) "foo"
          $ empty
    print (member 1 x)
    print (keysSet x)
    print (toList x)
    print x
