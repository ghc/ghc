main = print (length take_list)
 where
    take_list :: [Int]
    take_list = takeWhile (\ x -> x < 6) given_list

    given_list :: [Int]
    given_list = [1,2,3,4,5,6,7,8,9]
