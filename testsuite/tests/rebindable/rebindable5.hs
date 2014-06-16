{-# LANGUAGE RebindableSyntax, NPlusKPatterns, FlexibleInstances,
             MultiParamTypeClasses, FunctionalDependencies #-}

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

	returnIO :: a -> IO a;
        returnIO = Prelude.return;
	
	class HasReturn m where
		{
		return :: a -> m a;
		};

	class HasBind m n mn | m n -> mn, m mn -> n where
		{
		(>>=) :: m a -> (a -> n b) -> mn b;
		};

	class HasSeq m n mn | m n -> mn, m mn -> n where
		{
		(>>) :: m a -> n b -> mn b;
		};

	class HasFail m where
		{
		fail :: String -> m a;
		};

	instance HasReturn IO where
		{
		return a = debugFunc "return" (returnIO a);
		};

	instance HasBind IO IO IO where
		{
		(>>=) ma amb = debugFunc ">>=" ((Prelude.>>=) ma amb);
		};

	instance HasSeq IO IO IO where
		{
		(>>) ma mb = debugFunc ">>" ((Prelude.>>) ma mb);
		};

	instance HasFail IO where
		{
		fail s = debugFunc "fail" (returnIO undefined);
	--	fail s = debugFunc "fail" (Prelude.fail s);
		};

	class HasFromInteger a where
		{
		fromInteger :: a -> a;
		};

	instance HasFromInteger Integer where
		{
		fromInteger a = a Prelude.+ a Prelude.+ a Prelude.+ a Prelude.+ a; -- five times
		};

	class HasFromRational a where
		{
		fromRational :: a -> a;
		};

	instance HasFromRational Rational where
		{
		fromRational a = a Prelude.+ a Prelude.+ a; -- three times
		};

	class HasNegate a where
		{
		negate :: a -> a;
		};

	instance HasNegate Integer where
		{
		negate a = a; -- don't actually negate
		};

	instance HasNegate Rational where
		{
		negate a = a; -- don't actually negate
		};

	class HasMinus a where
		{
		(-) :: a -> a -> a;
		};

	instance HasMinus Rational where
		{
		(-) x y = y; -- changed function
		};

	instance HasMinus Integer where
		{
		(-) x y = y; -- changed function
		};


	test_do f g = do
		{
		f;				-- >>
		Just a <- g;	-- >>= (and fail if g returns Nothing)
		return a;		-- return
		};
	
	test_fromInteger :: Integer;
	test_fromInteger = 27;
	
	test_fromRational :: Rational;
	test_fromRational = 31.5;

	test_negate :: Integer -> Integer;
	test_negate a = - a;

	test_fromInteger_pattern :: Integer -> String;
	test_fromInteger_pattern a@1 = "1=" ++ (Prelude.show a);
	test_fromInteger_pattern a@(-2) = "(-2)=" ++ (Prelude.show a);
	test_fromInteger_pattern (a + 7) = "(a + 7)=" ++ Prelude.show a;

	test_fromRational_pattern :: Rational -> String;
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
			(test_do (returnIO ()) (returnIO Nothing))
		)
			Prelude.>>
		(doTest "test_do success"
			(test_do (returnIO ()) (returnIO (Just ())))
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
			(putStrLn (test_fromInteger_pattern (-(2::Integer)::Integer)))
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
			(putStrLn (test_fromRational_pattern (-(0.7::Rational)::Rational)))
		)
			Prelude.>>
		(doTest "test_fromRational_pattern 1.7"
			(putStrLn (test_fromRational_pattern 1.7))
		)
	   ;
	}
