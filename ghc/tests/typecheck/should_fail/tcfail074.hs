{- 	SHOULD FAIL

	GHC 2.02 failed to realise that this bogus
	program didn't have the right type for main
-}

main		=  2
main		=  putStrLn "hello world"
