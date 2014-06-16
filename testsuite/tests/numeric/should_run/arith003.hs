-- $Id: arith003.hs,v 1.2 2002/01/25 13:40:39 simonmar Exp $
--
-- !!! test Int/Integer arithmetic operations from the Prelude.
--

main
  = putStr
       (
	showit (do_ops int_ops) ++
	showit (do_ops integer_ops)
    	)

showit :: (Show a, Integral a) => [(String, a, a, a)] -> String
showit stuff = concat
       [ str ++ " " ++ show l ++ " " ++ show r ++ " = " ++ show result ++ "\n"
         | (str, l, r, result) <- stuff
       ]

do_ops :: Integral a => [((a -> a -> a), String, [(a,a)])]
	-> [(String, a, a, a)]
do_ops ops = [ (str, l, r, l `op` r) | (op,str,args) <- ops, (l,r) <- args ]

small_operands, non_min_operands, operands, non_max_operands
   :: Integral a => [a]
small_operands  = [ 0, 1, -1, 2, -2 ]
operands = small_operands ++ [ fromIntegral minInt, fromIntegral maxInt ]
non_min_operands = small_operands ++ [ fromIntegral maxInt ]
non_max_operands = small_operands ++ [ fromIntegral minInt ]

large_operands :: [ Integer ]
large_operands = operands ++ 
   [ fromIntegral minInt - 1,
     fromIntegral maxInt + 1,
     fromIntegral minInt * 2,
     fromIntegral maxInt * 2,
     fromIntegral minInt ^ 2, 
     fromIntegral maxInt ^ 2
   ]

integer_ops :: [((Integer -> Integer -> Integer), String, [(Integer,Integer)])]
integer_ops = [ 
  ((+),  "(+)",  both_large),
  ((-),  "(-)",  both_large),
  (div,  "div",  large_non_zero_r),
  (mod,  "mod",  large_non_zero_r),
  (quot, "quot", large_non_zero_r),
  (rem,  "rem",  large_non_zero_r),
  (gcd,  "gcd",  large_either_non_zero),
  (lcm,  "lcm",  large_either_non_zero)
  ]

int_ops :: [((Int -> Int -> Int), String, [(Int,Int)])]
int_ops = [ 
  ((+),  "(+)",  both_small),
  ((-),  "(-)",  both_small),
  ((^),  "(^)",  small_non_neg_r),
  (div,  "div",  non_min_l_or_zero_r),
  (mod,  "mod",  non_min_l_or_zero_r),
  (quot, "quot", non_min_l_or_zero_r),
  (rem,  "rem",  non_min_l_or_zero_r),
  (gcd,  "gcd",  non_min_either_non_zero),
  (lcm,  "lcm",  non_max_r_either_non_zero)
  ]

-- NOTE: when abs(minInt) is undefined (it is in GHC, because
-- abs(minInt) would be greater than maxInt), then gcd on Ints is also
-- undefined when either operand is minInt.

both_small, non_zero_r, non_min_either_non_zero, non_min_l_or_zero_r,
 non_max_r_either_non_zero, small_non_neg_r
  :: Integral a => [(a,a)]

both_small      = [ (l,r) | l <- operands, r <- operands ]
both_large	= [ (l,r) | l <- large_operands, r <- large_operands ]
large_non_zero_r = [ (l,r) | l <- operands, r <- large_operands, r /= 0 ]
non_zero_r      = [ (l,r) | l <- operands, r <- operands, r /= 0 ]
non_min_either_non_zero = [ (l,r) | l <- non_min_operands, r <- non_min_operands, l /= 0 || r /= 0 ]
large_either_non_zero = [ (l,r) | l <- operands, r <- operands, l /= 0 || r /= 0 ]
small_non_neg_r = [ (l,r) | l <- operands, r <- small_operands, r >= 0 ]
non_min_l_or_zero_r = [ (l,r) | l <- non_min_operands, r <- operands, r /= 0 ]
non_max_r_either_non_zero = [ (l,r) | l <- operands, r <- non_max_operands, l /= 0 || r /= 0 ]

minInt = minBound :: Int
maxInt = maxBound :: Int
