--!!! Testing error catching

--module TestCatch where

test1, test2 :: String

test1 = show $ primCatchError (error "foo"::Int)
test2 = show $ primCatchError 1


test3, test4, test5 :: String

test3 = show $ catch (1+error "foo") 2
test4 = show $ catch 1 (error "bar")
test5 = show $ catch (error "foo") (error "bar" :: Int)


test6, test7, test8, test9 :: IO ()

test6 = printString "abcdefg"
test7 = printString (error "a" : "bcdefg")
test8 = printString ("abc" ++ error "defg")
test9 = printString (error "a" : "bc" ++ error "defg")

-- if an error occurs, replace it with a default (hopefully error-free) value
catch :: a -> a -> a
catch x deflt = case primCatchError x of
                Just x' -> x'
		Nothing -> deflt

-- lazily print a string - catching any errors as necessary
printString :: String -> IO ()
printString str =
  case primCatchError str of
  Nothing     -> putStr "<error>"
  Just []     -> return ()
  Just (c:cs) -> case primCatchError c of
		 Nothing -> putStr "<error>" >> printString cs
	 	 Just c' -> putChar c' >> printString cs

