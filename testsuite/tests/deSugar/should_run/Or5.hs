{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

main = do
  print ((f1 3) == 1)
  print ((f1 5) == 3)

  print ((f2 [0,2,4]) == 1)
  print ((f2 [1,3]) == 2)

  print ((f3 4 4) == True)
  print ((f3 3 8) == True)

  print (f4 LT True  == 1)
  print (f4 LT False == 2)
  print (f4 EQ True  == 1)
  print (f4 EQ False == 3)

  print (a3 == 3)
  print (a4 == True)
  print (a5 == True)
  print (a6 == False)

  print backtrack

f1 x = case x of
  3 -> 1
  4 -> 2
  3;4;5 -> 3

f2 y = case y of
  (_:2:_ ; 1:_) | length y /= 2 -> 1
  ([1,2] ; 1:3:_)-> 2
  _ ; _ -> 3

f3 :: (Eq a, Show a) => a -> a -> Bool
f3 a (((== a) -> True) ; (show -> "8")) = True
f3 _ _ = False

f4 :: Ordering -> Bool -> Int
f4 (LT; EQ) True  = 1
f4 LT       False = 2
f4 _        False = 3
{-# NOINLINE f4 #-}

a3 = (\(1 ; 2) -> 3) 1
a4 = (\(Left 0 ; Right 1) -> True) (Right 1)
a5 = (\(([1] ; [2, _]) ; ([3, _, _] ; [4, _, _, _])) -> True) [4, undefined, undefined, undefined]
a6 = (\(1 ; 2 ; 3) -> False) 3

backtrack :: String
backtrack = case (True, error "backtracking") of
  ((True, _) ; (_, True))
    | id False -> error "inaccessible"
  _ -> error "no backtracking"
