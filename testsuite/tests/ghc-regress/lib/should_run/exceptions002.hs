module Main where
	{
	import qualified Control.Exception;
	import Data.IORef;
	import Prelude;

	safeCatch :: IO () -> IO ();
	safeCatch f = Control.Exception.catch f (\_ -> return ());

	type Thrower = IO Bool;

	type Catcher = IO Bool -> IO () -> IO ();

	checkCatch :: Catcher -> Thrower -> IO Bool;
	checkCatch catcher thrower = do
		{
		ref <- newIORef False;
		safeCatch (catcher thrower (writeIORef ref True));
		readIORef ref;
		};

	data Named a = MkNamed String a;

	checkNamedCatch :: Named Catcher -> Named Thrower -> IO ();
	checkNamedCatch (MkNamed cname catcher) (MkNamed tname thrower) = do
		{
		didCatch <- checkCatch catcher thrower;
		putStrLn (cname ++ (if didCatch then " CAUGHT " else " MISSED ") ++ tname);
		};

	checkNamedCatches :: [Named Catcher] -> [Named Thrower] -> IO ();
	checkNamedCatches [] _ = return ();
	checkNamedCatches _ [] = return ();
	checkNamedCatches [c] (t:tr) = do
		{
		checkNamedCatch c t;
		checkNamedCatches [c] tr;
		};
	checkNamedCatches (c:cr) ts = do
		{
		checkNamedCatches [c] ts;
		checkNamedCatches cr ts
		};


	-- throwers

	returnThrower :: Named Thrower;
	returnThrower = MkNamed "return" (return True);

	returnUndefinedThrower :: Named Thrower;
	returnUndefinedThrower = MkNamed "return undefined" (return undefined);

	returnErrorThrower :: Named Thrower;
	returnErrorThrower = MkNamed "return error" (return (error "some error"));

	undefinedThrower :: Named Thrower;
	undefinedThrower = MkNamed "undefined" undefined;

	failThrower :: Named Thrower;
	failThrower = MkNamed "fail" (fail "some failure");

	errorThrower :: Named Thrower;
	errorThrower = MkNamed "error" (error "some error");

	throwThrower :: Named Thrower;
	throwThrower = MkNamed "Control.Exception.throw"
	 (Control.Exception.throw (Control.Exception.ErrorCall "throw error"));

	ioErrorErrorCallThrower :: Named Thrower;
	ioErrorErrorCallThrower = MkNamed "ioError ErrorCall"
	 (Control.Exception.throwIO (Control.Exception.ErrorCall "throw error"));

	ioErrorIOExceptionThrower :: Named Thrower;
	ioErrorIOExceptionThrower = MkNamed "ioError IOException"
	 (Control.Exception.throwIO (Control.Exception.IOException undefined));

	returnThrowThrower :: Named Thrower;
	returnThrowThrower = MkNamed "return Control.Exception.throw"
	 (return (Control.Exception.throw (Control.Exception.ErrorCall "throw error")));


	-- catchers

	bindCatcher :: Named Catcher;
	bindCatcher = MkNamed ">>" (>>);

	preludeCatchCatcher :: Named Catcher;
	preludeCatchCatcher = MkNamed "Prelude.catch"
	 (\f cc -> Prelude.catch (f >> (return ())) (const cc));

	ceCatchCatcher :: Named Catcher;
	ceCatchCatcher = MkNamed "Control.Exception.catch"
	 (\f cc -> Control.Exception.catch (f >> (return ())) (const cc));

	finallyCatcher :: Named Catcher;
	finallyCatcher = MkNamed "Control.Exception.finally"
	 (\f cc -> Control.Exception.finally (f >> (return ())) cc);

	main = checkNamedCatches
		[bindCatcher,preludeCatchCatcher,ceCatchCatcher,finallyCatcher]
		[returnThrower,returnUndefinedThrower,returnThrowThrower,returnErrorThrower,failThrower,
		errorThrower,throwThrower,ioErrorErrorCallThrower,ioErrorIOExceptionThrower,undefinedThrower];

	}
