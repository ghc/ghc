-- !!! tests that minBound::Int is correctly handled (for Int & Integer

-- (not necessarily Haskell 98: relies on Int being a 32-bit type.)

main = do
  print (-2147483648 :: Int)			-- -2147483648
  print ((-2147483647)-1 :: Int)		-- -2147483648
  print (-2147483648 :: Integer)		-- -2147483648
  print ((-2147483648 :: Int) >= 0)		-- False
  print ((-2147483648 :: Integer) >= 0)		-- False
  print (-(-2147483648) :: Int)			-- <undefined>
  print (abs (-2147483648) :: Int)		-- <undefined>
  print (abs ((-2147483647)-1) :: Int)		-- <undefined>
  print (abs (-2147483648) :: Integer)		-- 2147483648
  print (abs ((-2147483647)-1) :: Integer) 	-- 2147483648 (wrong in 4.04)
  print (fromInteger (-2147483648 :: Integer) :: Int)	   -- -2147483648
  print (fromInteger ((-2147483647)-1 :: Integer) :: Int)  -- -2147483648
