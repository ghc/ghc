--!!! Testing error catching

test1, test2 :: Either HugsObject Int

test1 = primCatchError (error "foo")
test2 = primCatchError 1


test3, test4, test5 :: Int

test3 = myCatch (1+error "foo") 2
test4 = myCatch 1 (error "bar")
test5 = myCatch (error "foo") (error "bar")


test6, test7, test8, test9 :: IO ()

test6 = printString "abcdefg"
test7 = printString (error "a" : "bcdefg")
test8 = printString ("abc" ++ error "defg")
test9 = printString (error "a" : "bc" ++ error "defg")

-- if an error occurs, replace it with a default (hopefully error-free) value
myCatch :: a -> a -> a
myCatch x deflt = case primCatchError x of
                Right x' -> x'
		Left _   -> deflt

-- lazily print a string - catching any errors as necessary
printString :: String -> IO ()
printString str =
  case primCatchError str of
  Left _       -> putStr "<error>"
  Right []     -> return ()
  Right (c:cs) -> case primCatchError c of
		  Left _   -> putStr "<error>" >> printString cs
	 	  Right c' -> putChar c' >> printString cs

