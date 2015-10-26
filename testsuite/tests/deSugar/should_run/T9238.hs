compareDouble :: Double -> Double -> Ordering
compareDouble x y =
       case (isNaN x, isNaN y) of
       (True, True)   -> EQ
       (True, False)  -> LT
       (False, True)  -> GT
       (False, False) ->
          -- Make -0 less than 0
          case (x == 0, y == 0, isNegativeZero x, isNegativeZero y) of
          (True, True, True, False) -> LT
          (True, True, False, True) -> GT
          _                         -> x `compare` y

main = do
    let l = [-0, 0]
    print [ (x, y, compareDouble x y) | x <- l, y <- l ]
