--!!! Testing Enum

module TestEnum where

-- test for derived instances

data T = C1 | C2 | C3 | C4 | C5 | C6 | C7 deriving (Eq, Ord, Enum, Show)

test1 = show $ [C1 .. ]
test2 = show $ [C1 .. C4]
test3 = show $ [C1, C3 ..]
test4 = show $ [C1, C3 .. C6]
test5 = show $ [C7, C5 .. ]
test6 = show $ [C7, C5 .. C2]
test7 = show $ map fromEnum [C1 .. ]
test8 = show (map toEnum [0..6]  :: [T])

test9  = show (toEnum (-1) :: T)  -- should fail
test10 = show (toEnum 7    :: T)  -- should fail

test11 = show $ take 7 (iterate succ C1)
test12 = show $ take 7 (iterate pred C7)

test13 = show $ succ C7 -- should fail
test14 = show $ pred C1 -- should fail

-- test for built in Enum instances

test20 = show $ ['a' ..]
test21 = show $ ['a' ..'z']
test22 = show $ ['a', 'd' ..]
test23 = show $ ['a', 'd' .. 'z']
test24 = show $ ['z','y'..'a']
test25 = show $ map fromEnum ['a' ..]
test26 = show $ map fromEnum ['a', 'd' ..]
test27 = show $ map fromEnum ['a'..'z']
test28 = show (map toEnum [fromEnum 'a'..fromEnum 'z'] :: [Char])

test30 = show (take 50 $ [1..]::[Int])
test31 = show ([1..10]::[Int])
test32 = show (take 50 $ [1, 3 ..]::[Int])
test33 = show ([1, 3 .. 10]::[Int])
test34 = show ([10,9..1]::[Int])
test35 = show (map fromEnum [1..10]::[Int])
test36 = show (map toEnum [fromEnum 1..fromEnum 10]::[Int])


test40 = show (take 50 $ [1..]::[Integer])
test41 = show ([1..10]::[Integer])
test42 = show (take 50 $ [1, 3 ..]::[Integer])
test43 = show ([1, 3 .. 10]::[Integer])
test44 = show ([10,9..1]::[Integer])
test45 = show (map fromEnum [1..10]::[Int])
test46 = show (map toEnum [fromEnum 1..fromEnum 10]::[Integer])

-- All these tests use integers because roundoff errors have
-- such bizarre effects on the printed number.
test50 = show (take 50 $ [1..]::[Float])
test51 = show ([1..10]::[Float])
test52 = show (take 50 $ [1, 2 ..]::[Float])
test53 = show ([1, 2 .. 20]::[Float])
test54 = show ([20,19..10]::[Float])
test55 = show (map fromEnum ([1..10]::[Float]))
test56 = show (map toEnum [fromEnum 1..fromEnum 10]::[Float])


test60 = show (take 50 $ [1..]::[Double])
test61 = show ([1..10]::[Double])
test62 = show (take 50 $ [1, 2 ..]::[Double])
test63 = show ([1, 2 .. 20]::[Double])
test64 = show ([20,19..10]::[Double])
test65 = show (map fromEnum ([1..10]::[Double]))
test66 = show (map toEnum [fromEnum 1..fromEnum 10]::[Double])



