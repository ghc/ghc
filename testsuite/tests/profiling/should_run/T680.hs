foo :: (() -> () -> [()] -> [()]) -> () -> [()] -> [()]
foo k =
  \_ xs -> concatMap ($ [head xs]) [bar]
 where
  bar =
    let k' = k undefined undefined
    in  \xs ->
          let k'' = [k' xs]
          in  (() : (foldr1 (>>) k''))

k :: () -> [()] -> [()]
k = foo (\_ -> k)

--k a = foo (\_ -> k) a

-- all the work should happen in r
r :: ()
r = k undefined [] !! 4000

main = print r
