{- 
	Date: Wed, 3 Sep 1997 10:25:35 +0200 (MET DST)
	From: Jon Mountjoy <jon@wins.uva.nl>

Hello Bug Hunters,

The following program (rather condensed as it comes from something
much larger), crashes the compiler. I am using ghc-2.05 on a Solaris
box, with one or two patches(including the WwLib one which Simon gave
me, but this error occured before applying this patch).

It seems to be some rather subtle dependency problem.  Compiling with
-O solves the problem, compiling with ghc -c Foo.lhs should highlight
it.  The error message is appended below the file.  What are the new
"discarding polymorphic case" warnings about?

If you change any line in reallyFlatten, the program compiles fine.  
Likewise if you change the type of LinearCode to Int!

-}

> -- SNIP START

> module Foo where

> import Array
> import Monad

> reallyFlatten ::  Int -> Int -> Interpreter ()
> reallyFlatten x t
>  = do
>     let f  = goo x
>     out $ show x
>     setIStatus (IFlattened f f)
>     return ()

> goo :: Int -> LinearCode
> goo = \x -> listArray (0,1) []

%---------------------------------------------------------------------

> type LinearCode = Array Int Int

> data InterpStatus = IFlattened !LinearCode !LinearCode 
> data SM = SM { interpStatus :: InterpStatus }
> initialState = SM { interpStatus = IFlattened undefined undefined }

> newtype Interpreter a = RepInterp ((SM,Int) -> IO (Error ((SM,Int),a)))
> getRepInterp (RepInterp a ) = a

> instance Functor Interpreter where
>  map f (RepInterp intp ) 
>   = RepInterp (\s -> case intp s of
>			 g -> g >>= \q -> 
>		          case q of
>			   Error mes  -> return $ Error mes
>			   Ok (s',a') -> return $ Ok (s',f a'))

> instance Monad Interpreter where
>  return x = RepInterp (\s -> return (Ok (s,x)))
>  (RepInterp intp) >>= g 
>    = RepInterp(\s -> case intp s of
>			p -> p >>= \q ->
>		         case q of
>			  Error mes  -> return $ Error mes
>			  Ok (s',x') -> getRepInterp (g x') s' )

> instance OutputMonad Interpreter where
>  out s = RepInterp (\st -> putStr s >> 
> 			     return (Ok (st,())))

> updateSM :: (SM -> SM) -> Interpreter SM
> updateSM f = RepInterp (\s@(sm,ty) -> return $ Ok ((f sm,ty), sm))

> setIStatus :: InterpStatus -> Interpreter InterpStatus
> setIStatus is = updateSM (\sm -> sm {interpStatus = is}) >>= 
>                 return.interpStatus

> data Error a = Ok a | Error String 
>     
> class Monad m => OutputMonad m  where   
>   out      :: String -> m ()
> instance OutputMonad IO where
>  out s = catch (putStr s) (\_ ->  fail $userError "Oh MY")
