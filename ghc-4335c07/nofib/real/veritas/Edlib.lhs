> module Edlib where

> import Type_defs

> import Core_datatype

> infixr 9 <:

> infixl 8 /// , />/ , /:>/ , /./ , /.>/, /.:>/, |||, |>|, |@|, |.|

> infixl 8 ..., ./., \\\

> infixl 1 `handle`, `ihandle`

>-- infixl 8 >>, >>>, *>>, *>>>, >>+, >>>+, *>>+, *>>>+, >>^, >>>^, *>>^, *>>>^





>-- type Xin = ( String , [Response] , [Int]) 
> type Xin = ( String , [Int] , [Int]) 

> type Xout = String
>-- type Xout = [Request]

> type Xio = ( Xin , Xout )

> type Xio_fn = Xin -> Xio

> type Xst a = ( Xin , a , Xout )

> type Xst_fn a = ( Xin , a ) -> ( Xin , a , Xout )


>--partain: id x = x




{- composition function to allow output fns to be written 'sequentially' -}

> (\\\) :: Xio -> Xio_fn -> Xio

> ( xin , xout ) \\\ g 
>	= ( xin2 , xout ++ xout2 )
>         where
>	  ( xin2, xout2  ) = g xin
	

> (...) :: Xio_fn -> Xio_fn -> Xin -> Xio

> (...) f g xin
>	= f xin \\\ g


composition f - output fn, g - input fn with state (st)

> (./.) :: Xio_fn -> ( Xin -> Xst ( MayBe a b )) -> Xin -> Xst ( MayBe a b )

> (./.) f g xin
>	= ( xin2 , st, xout' ++ xout2 )
>         where
>         ( xin', xout' )  = f xin
>	  ( xin2, st, xout2  ) = g xin'


utility to apply a list of functions to a state 

> app :: [ Xin -> Xio ] -> Xin -> Xio

> app ( fn : fns ) = fn ... app fns

> app [] =  \ xin -> ( xin , [] )




> --partain: (|||) :: MayBe a b -> ( a -> MayBe c d ) -> MayBe c d
> (|||) :: MayBe a b -> ( a -> MayBe c b ) -> MayBe c b

> ( Ok s ) ||| f
> 	= f s

> ( Bad mesg ) ||| f = Bad mesg






vanilla Maybe composed with IO fn

> s |.| f 
> 	= case s of
>		Ok t    -> f t
>		Bad err -> return_err err 






> ( Ok ( s , u )) |@| f
> 	= f s u

> ( Bad mesg ) |@| f = Bad mesg




> ( Ok ( s , u )) |>| f
>	= case f u of
>		Ok t    -> Ok (( s , t ) , u )
>		Bad err -> Bad err







> (///) :: Xst (MayBe a b) -> ( a -> Xin -> Xst (MayBe c b)) -> Xst (MayBe c b)

> x@( xin , st , xout ) /// f
>	= sendout x x'
>	  where
>	  x' = case st of
>	 	   Ok val   -> f val xin 
>		   Bad mesg -> ( xin , Bad mesg , [] )




as above except second argument not fully evaluated

> (/./) :: (Xin -> Xst (MayBe a b)) -> ( a -> Xin -> Xst (MayBe c b)) 
>					           -> Xin -> Xst (MayBe c b)

> (/./) f g xin
>	= f xin /// g 




> (/>/) :: Xst (MayBe a b) -> ( Xin -> Xst (MayBe c b)) -> Xst (MayBe (a,c) b)

> x1@( xin , st , xout ) />/ f
>	= sendout x1 x1'
>	  where
>	  x1' = case st of
>		  Ok val -> ( xin2, st2', xout2 )
>			    where
>			    st2' = case st2 of
>				    Ok val2  -> Ok ( val, val2 )
>				    Bad mesg -> Bad mesg
>		  Bad mesg -> ( xin, Bad mesg, [] )
>	  ( xin2, st2, xout2 ) = f xin





> (/.>/) :: (Xin -> Xst (MayBe a b)) -> ( Xin -> Xst (MayBe c b)) 
>					       -> Xin -> Xst (MayBe (a,c) b)

> (/.>/) f g xin
>	= f xin />/ g 



as above except form list as state output rather than nested two tuples
(note, first read, first in list)


> x1@( xin , st , xout ) /:>/ f
>	= sendout x1 x1'
>	  where
>	  x1' = case st of
>		  Ok val -> ( xin2, st2', xout2 )
>			    where
>			    st2' = case st2 of
>				    Ok val2  -> Ok ( val : val2 )
>				    Bad mesg -> Bad mesg
>		  Bad mesg -> ( xin, Bad mesg, [] )
>	  ( xin2, st2, xout2 ) = f xin



> (/.:>/) :: (Xin -> Xst (MayBe a b)) -> ( Xin -> Xst (MayBe [a] b)) 
>					       -> Xin -> Xst (MayBe [a] b)

> (/.:>/) f g xin
>	= f xin /:>/ g 


error handler

> handle f handler xin
>	= f xin `ihandle` handler 

> ihandle x@( xin , st , _ ) handler 
>	= sendout x x'
>	  where
>	  x' = case st of
>		  Ok val   -> ( xin, Ok val, [] )
>		  Bad mesg -> handler mesg xin 






push non-MayBe Xio fn into valid MayBe result with unused non-xio
result

> mk_ok x_fn arg 
>	= Ok ( x_fn arg )



split a list on a given element

> split :: ( Eq a ) => a -> [a] -> [[a]]

> split c ( a : x ) 
>	| a == c = [] : split c x
>	| a /= c = case split c x of
>			b : y -> ( a : b ) : y
>			[]    -> [[a]]

> split _ [] = []



append a character to the end of a list

> (<:) :: [a] -> a -> [a]

> l <: c = l ++ [c]






> sendout ( _, _, xout ) x
>	= ( xin, st, xout ++ xout2 )
>	  where
>	  ( xin, st, xout2 ) = x



> return_val st xin = ( xin , st , [] )

> return_err st xin = ( xin, Bad st , [] )

> reTurn st xin = ( xin, Ok st, [] )



> genuid ( ins , rsps , ( gno : gnoL ) )
>	= ( ( ins , rsps , gnoL ) , Ok ( show gno ) , [] )











parsing functions



>{-

> ( f >> g ) 
> 	= next_tk f /./ ( next_tk . g ) 

> ( f >>> g)
>	= next_tk f /:>/ next_tk g


> ( f *>> g ) 
>	= f >> ( g . discard_tk )

> ( f *>>> g)
>	= f >>> ( g . discard_tk )




> ( f >>+ g )
>	= ( \ tk -> f tk /./ pst_extend ) >> g 

> ( f >>>+ g )
>	= ( \ tk -> f tk /./ pst_extend ) >>> g

> ( f *>>+ g )
>	= ( \ tk -> f tk /./ pst_extend ) *>> g 

> ( f *>>>+ g )
>	= ( \ tk -> f tk /./ pst_extend ) *>>> g





> ( f >>^ g )
>	= f >> ( g . pst_retract )

> ( f >>>^ g )
>	= f >>> ( g . pst_retract )

> ( f *>>^ g )
>	= f *>> ( g . pst_retract )

> ( f *>>>^ g )
>	= f *>>> ( g . pst_retract )





> next_tk f ( tk : tkL, pst, xin ) 
>	= f tk ( tkL, pst, xin )

> next_tk _ st@( [] , _ , _ )
>	= return_err "Unexpected end of input" st






> discard_tk ( _ : tkL , pst , xin )
>	= ( tkL , pst , xin )

> discard_tk st@( [] , _ , _ ) = st





> pst_retract ( tkL , (tgL, Extend _ isg _ ), xin )
>	= ( tkL, (tgL, isg), xin )

> pst_retract _ = error "pst_retract on empty sg -- impossible"






> pst_extend resL st
>	= pst_extend' resL st ./.
>	  return resL 




> pst_extend' ( Opnd ( Idec idc ) : _ ) ( tkL, ( tgL, isg ), xin )
>	= ( tkL, ( tgL, Extend idc isg [] ), xin )

> pst_extend' ( _ : resL ) 
>	= pst_extend' resL 

> pst_extend' [] 
>	= return_err "No dc with which to extend sg" 




>-}
