{-# OPTIONS -fglasgow-exts -fno-implicit-prelude #-}

module RebindableCase1 where
	{
--	import Prelude;
	import Prelude(String,undefined,Maybe(..), (==), (>=) );

	return :: a;
	return = undefined;

	infixl 1 >>=;
	(>>=) :: a;
	(>>=) = undefined;
	
	infixl 1 >>;
	(>>) :: a;
	(>>) = undefined;

	fail :: a;
	fail = undefined;

	fromInteger :: a;
	fromInteger = undefined;

	fromRational :: a;
	fromRational = undefined;

	negate :: a;
	negate = undefined;

	(-) :: a;
	(-) = undefined;


	test_do f g = do
		{
		f;
		Just a <- g;
		return a;
		};
	
	test_fromInteger = 1;
	
	test_fromRational = 0.5;

	test_negate a = - a;

	test_fromInteger_pattern 1 = undefined;
	test_fromInteger_pattern (-1) = undefined;
	test_fromInteger_pattern (a + 7) = a;

	test_fromRational_pattern 0.5 = undefined;
	test_fromRational_pattern (-0.5) = undefined;
	test_fromRational_pattern _ = undefined;
	}
