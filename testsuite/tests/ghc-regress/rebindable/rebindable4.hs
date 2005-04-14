{-# OPTIONS -fglasgow-exts -fno-implicit-prelude #-}

module Main where
	{
--	import Prelude;
	import qualified Prelude;
	import Prelude(String,undefined,Maybe(..),IO,putStrLn,
		Integer,(++),Rational, (==), (>=) );

	debugFunc :: String -> IO a -> IO a;
	debugFunc s ioa = (putStrLn ("++ " ++ s)) Prelude.>>
		(ioa Prelude.>>= (\a -> 
			(putStrLn ("-- " ++ s)) Prelude.>> (Prelude.return a)
		));

	infixl 1 >>=;
	infixl 1 >>;

	class MyMonad m where
		{
		return :: a -> m a;
		(>>=) :: m a -> (a -> m b) -> m b;
		(>>) :: m a -> m b -> m b;
		fail :: String -> m a;
		};

	instance MyMonad IO where
		{
		return a = debugFunc "return" (Prelude.return a);
	
		(>>=) ma amb = debugFunc ">>=" ((Prelude.>>=) ma amb);
		
		(>>) ma mb = debugFunc ">>" ((Prelude.>>) ma mb);
	
		fail s = debugFunc "fail" (Prelude.return undefined);
	--	fail s = debugFunc "fail" (Prelude.fail s);
		};

	fromInteger :: Integer -> Integer;
	fromInteger a = a Prelude.+ a Prelude.+ a Prelude.+ a Prelude.+ a; -- five times

	fromRational :: Rational -> Rational;
	fromRational a = a Prelude.+ a Prelude.+ a; -- three times

	negate :: a -> a;
	negate a = a; -- don't actually negate

	(-) :: a -> a -> a;
	(-) x y = y; -- changed function


	test_do f g = do
		{
		f;				-- >>
		Just a <- g;	-- >>= (and fail if g returns Nothing)
		return a;		-- return
		};
	
	test_fromInteger = 27;
	
	test_fromRational = 31.5;

	test_negate a = - a;

	test_fromInteger_pattern a@1 = "1=" ++ (Prelude.show a);
	test_fromInteger_pattern a@(-2) = "(-2)=" ++ (Prelude.show a);
	test_fromInteger_pattern (a + 7) = "(a + 7)=" ++ Prelude.show a;

	test_fromRational_pattern a@0.5 = "0.5=" ++ (Prelude.show a);
	test_fromRational_pattern a@(-0.7) = "(-0.7)=" ++ (Prelude.show a);
	test_fromRational_pattern a = "_=" ++ (Prelude.show a);


	doTest :: String -> IO a -> IO ();
	doTest s ioa = 
		(putStrLn ("start test " ++ s))
			Prelude.>>
		ioa
			Prelude.>>
		(putStrLn ("end test " ++ s));

	main :: IO ();
	main = 
		(doTest "test_do failure"
			(test_do (Prelude.return ()) (Prelude.return Nothing))
		)
			Prelude.>>
		(doTest "test_do success"
			(test_do (Prelude.return ()) (Prelude.return (Just ())))
		)
			Prelude.>>
		(doTest "test_fromInteger"
			(putStrLn (Prelude.show test_fromInteger))
		)
			Prelude.>>
		(doTest "test_fromRational"
			(putStrLn (Prelude.show test_fromRational))
		)
			Prelude.>>
		(doTest "test_negate"
			(putStrLn (Prelude.show (test_negate 3)))
		)
			Prelude.>>
		(doTest "test_fromInteger_pattern 1"
			(putStrLn (test_fromInteger_pattern 1))
		)
			Prelude.>>
		(doTest "test_fromInteger_pattern (-2)"
			(putStrLn (test_fromInteger_pattern (-2)))
		)
			Prelude.>>
		(doTest "test_fromInteger_pattern 9"
			(putStrLn (test_fromInteger_pattern 9))
		)
			Prelude.>>
		(doTest "test_fromRational_pattern 0.5"
			(putStrLn (test_fromRational_pattern 0.5))
		)
			Prelude.>>
		(doTest "test_fromRational_pattern (-0.7)"
			(putStrLn (test_fromRational_pattern (-0.7)))
		)
			Prelude.>>
		(doTest "test_fromRational_pattern 1.7"
			(putStrLn (test_fromRational_pattern 1.7))
		);
	}
