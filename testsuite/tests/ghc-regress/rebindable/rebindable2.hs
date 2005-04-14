{-# OPTIONS -fglasgow-exts -fno-implicit-prelude #-}

module Main where
	{
--	import Prelude;
	import qualified Prelude;
	import Prelude(String,undefined,Maybe(..),IO,putStrLn,
		Integer,(++),Rational, (==), (>=) );
	
	import Prelude(Monad(..));

	debugFunc :: String -> IO a -> IO a;
	debugFunc s ioa = (putStrLn ("++ " ++ s)) Prelude.>>
		(ioa Prelude.>>= (\a -> 
			(putStrLn ("-- " ++ s)) Prelude.>> (Prelude.return a)
		));

	newtype TM a = MkTM {unTM :: IO a};

	instance (Monad TM) where
		{
		return a = MkTM (debugFunc "return" (Prelude.return a));

		(>>=) ma amb = MkTM (debugFunc ">>=" ((Prelude.>>=) (unTM ma) (\a -> unTM (amb a))));
	
		(>>) ma mb = MkTM (debugFunc ">>" ((Prelude.>>) (unTM ma) (unTM mb)));

		fail s = MkTM (debugFunc "fail" (Prelude.return undefined));
		};

	preturn a = MkTM (Prelude.return a);

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

	tmPutStrLn s = MkTM (putStrLn s);

	doTest :: String -> TM a -> IO ();
	doTest s ioa =
		(putStrLn ("start test " ++ s))
			Prelude.>>
		(unTM ioa)
			Prelude.>>
		(putStrLn ("end test " ++ s));

	main :: IO ();
	main = 
		(doTest "test_do failure"
			(test_do (preturn ()) (preturn Nothing))
		)
			Prelude.>>
		(doTest "test_do success"
			(test_do (preturn ()) (preturn (Just ())))
		)
			Prelude.>>
		(doTest "test_fromInteger"
			(tmPutStrLn (Prelude.show test_fromInteger)) -- 27 * 5 = 135
		)
			Prelude.>>
		(doTest "test_fromRational"
			(tmPutStrLn (Prelude.show test_fromRational)) -- 31.5 * 3 = 189%2
		)
			Prelude.>>
		(doTest "test_negate"
			(tmPutStrLn (Prelude.show (test_negate 3))) -- 3 * 5 = 15, non-negate
		)
			Prelude.>>
		(doTest "test_fromInteger_pattern 1"
			(tmPutStrLn (test_fromInteger_pattern 1)) -- 1 * 5 = 5, matches "1"
		)
			Prelude.>>
		(doTest "test_fromInteger_pattern (-2)"
			(tmPutStrLn (test_fromInteger_pattern (-2))) -- "-2" = 2 * 5 = 10
		)
			Prelude.>>
		(doTest "test_fromInteger_pattern 9"
			(tmPutStrLn (test_fromInteger_pattern 9)) -- "9" = 45, 45 "-" "7" = "7" = 35
		)
			Prelude.>>
		(doTest "test_fromRational_pattern 0.5"
			(tmPutStrLn (test_fromRational_pattern 0.5)) -- "0.5" = 3%2
		)
			Prelude.>>
		(doTest "test_fromRational_pattern (-0.7)"
			(tmPutStrLn (test_fromRational_pattern (-0.7))) -- "-0.7" = "0.7" = 21%10
		)
			Prelude.>>
		(doTest "test_fromRational_pattern 1.7"
			(tmPutStrLn (test_fromRational_pattern 1.7)) -- "1.7" = 51%10
		);
	}
