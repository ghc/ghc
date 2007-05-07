-- test case from #1091
main =
 	  case True of {- | -}
 	    True  -> putStrLn "Hello World\n"
 	    False {- | -} -> putStrLn "Goodbye Cruel World\n"
