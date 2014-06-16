-- The translation of this program should assign only one dictionary to
-- the function search (an Ord dictionary). Instead, it assigns two.
-- The output produced currently displays this.

-- 10/12/92:  This program is actually erroneous.  The pattern-binding for
-- search falls under the monomorphism restriction, and there is no
-- call to search which might fix its type.  So there should be a complaint.
-- But the actual error message is horrible:
-- 
-- "bug001.hs", line 26: Ambiguous overloading:
--     class "Ord_", type "a" (at a use of an overloaded identifier: gt)
--     class "Eq_", type "a" (at a use of an overloaded identifier: eq)


module TcFail where

class Eq_ a where
 eq :: a -> a -> Bool

instance Eq_ Int where
 eq = eqIntEq

instance (Eq_ a) => Eq_ [a] where
 eq = \ xs ys -> 
     if (null xs) 
        then (null ys)
        else if (null ys) 
                then False
                else (&&) (eq (hd xs) (hd ys)) (eq (tl xs) (tl ys))

class (Eq_ a) => Ord_ a where
 gt :: a -> a -> Bool

instance Ord_ Int where
 gt = ordIntGt

search 
 = \ a bs -> if gt (hd bs) a
                then False 
                else if eq a (hd bs) then True else search a (tl bs)


hd :: [a] -> a
hd (a:as) = a

tl :: [a] -> [a]
tl (a:as) = as

ordIntGt :: Int -> Int -> Bool
ordIntGt 2 3 = True

eqIntEq :: Int -> Int -> Bool
eqIntEq  2 3 = True




{-

===============================================
Main.Eq__INST_PreludeBuiltin.Int =
    let
      AbsBinds [] [] [(eq, eq)]
	  {- nonrec -}
	  {-# LINE 2 "test3.hs" -}

	  eq :: PreludeBuiltin.Int -> PreludeBuiltin.Int -> PreludeCore.Bool
	  eq = Main.eqIntEq
    in ({-dict-} [] [eq])

Main.Eq__INST_PreludeBuiltin.List =
    /\ t135 ->
	\{-dict-} _dict138 ->
	    let
	      {- nonrec -}
	      _dict136 = {-singleDict-} _dict138
	      {- nonrec -}
	      _dict129 = {-singleDict-} _dict136
	      AbsBinds [] [] [(eq, eq)]
		  {- nonrec -}

		  _dict133 =
		      Main.Eq__INST_PreludeBuiltin.List
			  [t135] [{-singleDict-} _dict136]
		  {- nonrec -}
		  {-# LINE 5 "test3.hs" -}

		  eq :: [t135] -> [t135] -> PreludeCore.Bool
		  eq = \ xs ys -> 

if (Main.null t135) xs then
				      (Main.null t135) ys
				  else

				      if (Main.null t135) ys then
					  PreludeCore.False
				      else

					  Main.and


					      ((Main.Eq_.eq t135 _dict129)


						   ((Main.hd t135) xs)
						   ((Main.hd t135) ys))
					      





(Main.Eq_.eq [t135] _dict133)



						   ((Main.tl t135) xs)
						   ((Main.tl t135) ys))
	    in ({-dict-} [] [eq])
Main.Ord__INST_PreludeBuiltin.Int =
    let
      {- nonrec -}
      _dict142 = Main.Eq__INST_PreludeBuiltin.Int [] []
      AbsBinds [] [] [(gt, gt)]
	  {- nonrec -}
	  {-# LINE 16 "test3.hs" -}

	  gt :: PreludeBuiltin.Int -> PreludeBuiltin.Int -> PreludeCore.Bool
	  gt = Main.ordIntGt
    in ({-dict-} [_dict142] [gt])

Main.Eq_.eq = /\ a -> \{-classdict-} [] [eq] -> eq

Main.Ord_.gt = /\ a -> \{-classdict-} [_dict56] [gt] -> gt

Main.Ord__TO_Main.Eq_ = /\ a -> \{-classdict-} [_dict58] [gt] -> ???_dict58???

AbsBinds [t60] [] [(hd, Main.hd)]
    {- nonrec -}



    hd :: [t60] -> t60
    hd (a PreludeBuiltin.: as)
	       = a

AbsBinds [t68] [] [(tl, Main.tl)]
    {- nonrec -}




    tl :: [t68] -> [t68]
    tl (a PreludeBuiltin.: as)
	       = as


AbsBinds [t91] [_dict85, _dict88] [(search, Main.search)]
    {- rec -}
    {-# LINE 19 "test3.hs" -}


    search :: t91 -> [t91] -> PreludeCore.Bool
    search
	= \ a bs -> 


if (Main.Ord_.gt t91 _dict85) ((Main.hd t91) bs) a then
			PreludeCore.False
		    else

			if (Main.Eq_.eq t91 _dict88) a ((Main.hd t91) bs) then
			    PreludeCore.True
			else

			    search a ((Main.tl t91) bs)
AbsBinds [] [] [(and, Main.and)]
    {- nonrec -}
    and :: PreludeCore.Bool -> PreludeCore.Bool -> PreludeCore.Bool
    and PreludeCore.True PreludeCore.True
		= PreludeCore.True
AbsBinds [] [] [(ordIntGt, Main.ordIntGt)]
    {- nonrec -}
    _dict97 = PreludeCore.Num_INST_PreludeBuiltin.Int [] []
    {- nonrec -}
    _dict98 = PreludeCore.Eq_INST_PreludeBuiltin.Int [] []
    {- nonrec -}
    _dict100 = PreludeCore.Num_INST_PreludeBuiltin.Int [] []
    {- nonrec -}
    _dict101 = PreludeCore.Eq_INST_PreludeBuiltin.Int [] []
    {- nonrec -}



    ordIntGt :: PreludeBuiltin.Int -> PreludeBuiltin.Int -> PreludeCore.Bool
    ordIntGt
	2 3 = PreludeCore.True
AbsBinds [] [] [(eqIntEq, Main.eqIntEq)]
    {- nonrec -}
    _dict105 = PreludeCore.Num_INST_PreludeBuiltin.Int [] []
    {- nonrec -}
    _dict106 = PreludeCore.Eq_INST_PreludeBuiltin.Int [] []
    {- nonrec -}
    _dict108 = PreludeCore.Num_INST_PreludeBuiltin.Int [] []
    {- nonrec -}
    _dict109 = PreludeCore.Eq_INST_PreludeBuiltin.Int [] []
    {- nonrec -}

    eqIntEq :: PreludeBuiltin.Int -> PreludeBuiltin.Int -> PreludeCore.Bool
    eqIntEq
	2 3 = PreludeCore.True


AbsBinds [t112] [] [(null, Main.null)]
    {- nonrec -}

    null :: [t112] -> PreludeCore.Bool
    null [] = PreludeCore.True
-}
