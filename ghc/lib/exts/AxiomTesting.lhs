% -----------------------------------------------------------------------------
% $Id: AxiomTesting.lhs,v 1.1 1999/10/25 05:19:22 andy Exp $
%
% (c) The Hugs/GHC Team 1999
%

This is a testing framework for using axiomatic like specifications
of equalities.

\begin{code}
module AxiomTesting (
	TestM,		-- abstract
	(&&&),
	(|||),
	funVar,
	displayVars,
	testRules,
	var,
	vars,
	ALPHA, BETA, GAMMA,
	EqALPHA, OrdALPHA,
	testAssoc,
	-- advanced user functions below
	Example(..),
	testComplete,
	testFail,
	bottom,
	bottomExample,
	) where

import Monad
import IO
import List
import IOExts
import Exception (tryAll)
import IOExts    (unsafePtrEq)

infix  4 <==>
infixl 3 &&&
infixl 2 |||

------------------------------------------------------------------------------

newtype TestM a = TestM { runTestM :: TestMState -> IO (TestMResult a) }

data TestMState = TestMState {
	uniqIds        :: IORef Int,
	bindingPairs :: [(String,String)]
	}	

initTestMState ref = TestMState {
	uniqIds = ref,
	bindingPairs = []
	}

data TestMResult a
	= TestMComplete !Int
	| TestMFail TestMState
	| TestMOk [(a,TestMState)]

runTestsM :: (a -> TestM b) -> [(a,TestMState)] 
		-> [(b,TestMState)] -> Int -> IO (TestMResult b)
runTestsM f [] [] n = return (TestMComplete n)
runTestsM f [] xs n = return (TestMOk xs)
runTestsM f ((a,s):as) ys n =
    do r <- runTestM (f a) s
       case r of
	  (TestMFail _)     -> return r
	  (TestMComplete m) -> runTestsM f as ys (n+m)
	  (TestMOk xs)      -> runTestsM f as (xs++ys) n

instance Monad TestM where
   return v  = TestM (\ b -> return (TestMOk [(v,b)]))
   p  >>= f  = TestM (\ b ->
                  do res <- runTestM p b
		     case res of
			  (TestMComplete m) -> return (TestMComplete m)
			  (TestMFail f) -> return (TestMFail f)
		 	  -- The following pattern is an optimization
			  TestMOk [(x,s)] -> runTestM (f x) s
			  TestMOk xs -> runTestsM f xs [] 0)

runIOTestM :: IO a -> TestM a
runIOTestM m = TestM (\ b -> do { r <- m ; return (TestMOk [(r,b)]) })

testComplete = TestM (\ b -> return (TestMComplete 1))
testFail     = TestM (\ b -> return (TestMFail b))


oneTest :: TestM () -> TestM ()
oneTest p =
  TestM (\ b -> do r <- runTestM p b
		   case r of
		      (TestMComplete n) -> return (TestMComplete 1)
		      other             -> return other)

{-
 - This also has the nice effect of stoping the bindings
 - of vars escaping for each side of the test.
 - This is why (>>=) does not have this semantics.
 -
 -}

(&&&) :: TestM () -> TestM () -> TestM ()
(&&&) p q =
  TestM (\ b -> do r <- runTestM p b
		   case r of
		      (TestMComplete n) -> 
			do r2 <- runTestM q b
			   case r2 of
			     (TestMComplete m) -> return (TestMComplete (n+m))
			     other -> return other	
		      (TestMFail _) -> return r
		      _ -> return (error "&&& failed"))


(|||) :: TestM () -> TestM () -> TestM ()
(|||) p q =
  TestM (\ b -> do r <- runTestM p b
		   case r of
		      (TestMComplete n) -> return r
		      (TestMFail _) -> runTestM q b
		      _ -> return (error "||| failed"))

pairUp :: String -> [(a,String)] -> TestM a
pairUp name pairs =
   TestM (\ b -> 
	do return (TestMOk [
			(a,b { bindingPairs = (name,r) : bindingPairs b })
		       		| (a,r) <- pairs ]))

funVar :: String -> String -> TestM ()
funVar name r = pairUp name [((),r)]

uniqId :: TestM Int
uniqId = TestM (\ s ->
 	do v <- readIORef (uniqIds s)
	   let v' = v + 1
	   writeIORef (uniqIds s) v'
	   return (TestMOk [(v',s)]))
{-
 - For debugging, you can make the monad display each binding
 - it is using.
 -}
displayVars  :: TestM ()
displayVars = TestM (\ s ->
 	do putStr "\n"
	   sequence_ [putStr ("    **    " ++ k ++ " = " ++ v ++ "\n")
			| (k,v) <- reverse (bindingPairs s) ]
	   return (TestMOk [((),s)]))

------------------------------------------------------------------------------
{-
 - This function lets you test a list of rules
 - about a specific function.
 -} 

testRules :: String -> [TestM ()] -> IO ()
testRules name actions =
  do putStr (rjustify 25 name ++ " : ")
     f <- tr 1 actions [] 0
     mapM fa f
     return ()
  where
	rjustify n s = replicate (max 0 (n - length s)) ' ' ++ s

	tr n [] [] c = do { 
		putStr (rjustify (45 - n) (" (" ++ show c ++ ")\n")) ;
		return [] }
	tr n [] xs c = do { putStr ("\n")  ; return xs }
	tr n (action:actions) others c = 
	   do ref <- newIORef 0
	      r <- runTestM action (initTestMState ref)
	      case r of
		(TestMComplete m)
			    -> do { putStr "." ;
				    tr (n+1) actions others (c+m) }
		TestMFail f -> do { putStr "#" ;
				   tr (n+1) actions ((n,f):others) c}
		_           -> do { putStr "?" ; tr (n+1) actions others  c}


	fa (n,f) = 
	  do putStr "\n"
	     putStr ("    ** test " 
			++ show n 
			++ " of "
			++ name
			++ " failed with the binding(s)\n")
	     sequence_ [putStr ("    **    " ++ k ++ " = " ++ v ++ "\n")
			| (k,v) <- reverse (bindingPairs f) ]
  	     putStr "\n"

var :: (Example a) => String -> TestM a
var name = 
       do pairs <- getVars
	  pairUp name pairs

vars :: (Example a,Show a) => String -> [a] -> TestM a
vars name as = 
       do pairUp name [(a,show a) | a <- as ]

------------------------------------------------------------------------------

class Example a where
	-- A list of examples of values at this type.
	getVars :: TestM [(a,String)]

	-- A version of equality, where _|_ == _|_ ==> True, not _|_

	(<==>) :: a -> a -> TestM ()
	(<==>) a b =
	      do r <- runIOTestM (magicTest a b)
		 case r of
	 	   Same        -> testComplete
	 	   PerhapsSame -> oneTest (a `equ` b)
	 	   Different   -> testFail

	isFinite :: a -> TestM ()
	isFinite a = 
	       do r <- runIOTestM (magicTest a bottom)
		  case r of
			-- If this is _|_, then this check
			-- is over, because this guard is not met.
			-- but we return success, because the
			-- overall problem was ok.
			-- returning "return ()" whould
			-- continue the test.
			-- (A bit like F => ? ==> T)
		    Same -> testComplete
		    _    -> isFiniteSpine a

	-- protected, use only for defintions of things.
	equ :: a -> a -> TestM ()
	equ _ _ = testFail

	isFiniteSpine :: a -> TestM ()
	isFiniteSpine _ = return ()

data BotCmp = Same | PerhapsSame | Different

------------------------------------------------------------------------------
-- All the compile specific parts are captured inside
-- the function magicTest.

#if __HUGS__

-- Old, Classic Hugs version
primitive catchError :: a -> Maybe a

magicTest :: a -> a -> IO BotCmp
magicTest a b = 
   if unsafePtrEq a b then return Same
   else case (catchError a,catchError b) of
		(Nothing,Nothing) -> return Same
		(Just a,Just b)   -> return PerhapsSame
		_                 -> return Different


#else

magicTest :: a -> a -> IO BotCmp
magicTest a b = 
   if unsafePtrEq a b then return Same
   else do a' <- tryAll a
	   b' <- tryAll b
           case (a',b') of
		(Left _,Left _)   -> return Same
		(Right _,Right _) -> return PerhapsSame
		_                 -> return Different

#endif
------------------------------------------------------------------------------

bottom = error "bottom"
bottomExample = [(bottom,"_|_")]

cmp a b = if (a == b) then testComplete else testFail

render :: (Show a) => [a] -> [(a,String)]
render as = [ (a,show a) | a <- as ]

instance Example Char    where
	getVars = return (render ['a','z'] ++ bottomExample)
	equ a b = cmp a b

instance Example Float   where
	getVars = return (render [0.0,1.0,999.0] ++ bottomExample)
	equ a b = cmp a b

instance Example Double  where
	getVars = return (render [0.0,1.0,999.0] ++ bottomExample)
	equ a b = cmp a b

instance Example Integer where
	getVars = return (render [-1,1,1] ++ bottomExample)
	equ a b = cmp a b

instance Example ()      where
	getVars = return (render [()] ++ bottomExample)
	equ a b = cmp a b

instance Example Int     where
	getVars = return (render [0,1,2,minBound,maxBound] ++ bottomExample)
	equ a b = cmp a b

instance Example Bool    where
	getVars = return (render [True,False] ++ bottomExample)
	equ a b = cmp a b

instance Example a => Example [a] where
	getVars = 
	    do e1 <- getVars
	       e2 <- getVars
	       return (
		  concat [ [ ([e],"[" ++ t ++ "]"),
			     (e:bottom,t ++ ":_|_") ]
				  	 | (e,t) <- e1 ]
		++ [ ([e1,e2],
		      "[" ++ t1 ++ "," ++ t2 ++ "]")
			 | (e1,t1) <- e1, (e2,t2) <- e2 ]
		++ [ ([e1,e2,e1],
		      "[" ++ t1 ++ "," ++ t2 ++ "," ++ t1 ++ "]")
			 | (e1,t1) <- e1, (e2,t2) <- e2 ]
		++ [ ([],"[]")]
		++ bottomExample)

	equ []     []     = testComplete
	equ (a:as) (b:bs) = (a <==> b) &&& (as <==> bs)
	equ _      _      = testFail

	isFiniteSpine []     = return ()
	isFiniteSpine (_:xs) = isFinite xs

instance Example a => Example (Maybe a) where
	getVars = 
	    do e1 <- getVars
	       return (
		  [ (Just e,"Just " ++ t) 
				| (e,t) <- e1 ]
		++ [ (Nothing,"Nothing")]
		++ bottomExample)

	equ Nothing  Nothing     = testComplete
	equ (Just a) (Just b) = a <==> b
	equ _      _      = testFail

	isFiniteSpine Nothing  = return ()
	isFiniteSpine (Just _) = return ()

------------------------------------------------------------------------------

{- We pick something isomorphic to ints because of the
 - shape of the domain.
 -
 -       ... -1  0  1 ...
 - 	       \ | /
 -              \ /
 - 		_|_
 -}

class PolyExample a where
	mkPoly   :: Int -> a
	unPoly   :: a -> Int
	namePoly :: a -> String

	equPoly :: a -> a -> TestM ()
	equPoly a b = (unPoly a) <==> (unPoly b)

	getPolyVars :: TestM [(a,String)]
	getPolyVars =
          do n <- uniqId 
	     return ([mkPair (mkPoly 0) 0,
		     mkPair (mkPoly n) n] ++ bottomExample)
	  where
	    mkPair a n = (a,namePoly a ++ "_" ++ show n)

------------------------------------------------------------------------------

newtype ALPHA = ALPHA { unALPHA :: Int }

instance PolyExample ALPHA where
	mkPoly = ALPHA
	unPoly = unALPHA
	namePoly = const "a"

instance Example ALPHA where { equ = equPoly ; getVars = getPolyVars }

newtype BETA = BETA { unBETA :: Int }

instance PolyExample BETA where
	mkPoly = BETA
	unPoly = unBETA
	namePoly = const "b"

instance Example BETA where { equ = equPoly ; getVars = getPolyVars }

newtype GAMMA = GAMMA { unGAMMA :: Int }

instance PolyExample GAMMA where
	mkPoly = GAMMA
	unPoly = unGAMMA
	namePoly = const "c"

instance Example GAMMA where { equ = equPoly ; getVars = getPolyVars }

newtype EqALPHA = EqALPHA { unEqALPHA :: Int }
	deriving (Eq)

instance PolyExample EqALPHA where
	mkPoly = EqALPHA
	unPoly = unEqALPHA
	namePoly = const "a"

instance Example EqALPHA where { equ = equPoly ; getVars = getPolyVars }

newtype OrdALPHA = OrdALPHA { unOrdALPHA :: Int } 
	deriving (Eq,Ord)

instance PolyExample OrdALPHA where
	mkPoly = OrdALPHA
	unPoly = unOrdALPHA
	namePoly = const "b"

instance Example OrdALPHA where { equ = equPoly ; getVars = getPolyVars }

------------------------------------------------------------------------------
-- More Utilities

testAssoc :: (Example a) => (a -> a -> a) -> TestM ()
testAssoc fn =
   do x	<- var "x"
      y <- var "y"
      z <- var "z"
      ((x `fn` (y `fn` z)) <==> ((x `fn` y) `fn` z))

------------------------------------------------------------------------------
\end{code}

Example specifications. They all have type IO ().

test_concat = testRules "concat" [
	do (xss :: [[ALPHA]]) <- var "xss"
	   concat xss <==> foldr (++) [] xss
	]

test_head = testRules "head" [
	let def_head (x:_) = x
	    def_head []    = error ""
	in do (xs  :: [ALPHA]) <- var "xs"
	      head xs <==> def_head xs
	]

test_sort = testRules "sort" [
	do (xs :: [OrdALPHA]) <- var "xs"
	   sort xs <==> sortBy compare xs,
	do (xs :: [OrdALPHA]) <- var "xs"
	   head (sort xs) <==> minimum xs,
	do (xs :: [OrdALPHA]) <- var "xs"
	   last (sort xs) <==> maximum xs,
	do (xs :: [OrdALPHA]) <- var "xs"
	   (ys :: [OrdALPHA]) <- var "ys"
	   (null xs <==> True)
	     ||| (null ys <==> True)
	     ||| (head (sort (xs ++ ys)) <==> min (minimum xs) (minimum ys)),
	do (xs :: [OrdALPHA]) <- var "xs"
	   (ys :: [OrdALPHA]) <- var "ys"
	   (null xs <==> True)
	     ||| (null ys <==> True)
	     ||| (last (sort (xs ++ ys)) <==> max (maximum xs) (maximum ys))
	]

test_map = testRules "map" [
	let def_map f [] = []
	    def_map f (x:xs) = f x : def_map f xs
	    test f fn =
	        do funVar "f" fn
		   xs   <- var "xs"
		   map f xs <==> def_map f xs
	in
                test (id :: ALPHA -> ALPHA)
		    "id"
	    &&& test ((\ a -> a + 1) :: Int -> Int)
		      "\\ a -> a + 1" 
	    &&& test ((\ a -> bottom) :: Int -> Int)
		      "\\ a -> _|_",
	do (xs :: [ALPHA]) <- var "xs"
	   xs <==> map id xs
	]

test_int_plus = testRules "(+)::Int->Int->Int" [
	testAssoc ((+) :: Int -> Int -> Int)
	]
