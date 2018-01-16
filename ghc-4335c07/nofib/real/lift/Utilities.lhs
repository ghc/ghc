Utilities module for the fully lazy lambda lifter
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> module Utilities( 
>	Assn, assLookup,
>	NameSupply, initialNameSupply, newName,
>	Set, setFromList, setEmpty, setSingleton, setToList,
>		setUnion, setIntersect, setDifference, setUnionList,
>	Bag, bagFromList, bagEmpty, bagSingleton, bagToList,
>		bagInsert, bagUnion,
>	mapAccuml
> ) where
>

\section{Association lists}

Association lists are represented using a transparent type, because
it is so convenient to be able to use the usual list functions on them.

> type Assn key value = [(key, value)]
>
> assLookup :: Eq key => Assn key value -> key -> value
> assLookup alist key = head [value | (key',value) <- alist, key == key']

\section{Name supply}

> data NameSupply = MkNS Int
>
> initialNameSupply :: NameSupply
> initialNameSupply = MkNS 0

> newName :: NameSupply -> String -> (NameSupply, String)
> newName (MkNS n) prefix = (MkNS (n+1), prefix ++ show n)

\section{Sets}

We represent sets as {\em ordered} lists, where later elements
are greater than earlier elements.

> data Set a = MkSet [a]

Note: I could have made this |data Ord a => Set a = MkSet [a]|, but
then |setEmpty| would be an overloaded constant, and hence not
exportable.  So I left out the |Ord =>|.

Getting into and out of the representation is pretty easy.

> setFromList xs = MkSet (sortNoDups xs)
> setEmpty = MkSet []	
> setSingleton x = MkSet [x]
> setToList (MkSet xs) = xs

Now the basic combining functions.

> setUnion (MkSet xs) (MkSet ys) =
>   MkSet (merge xs ys) where
>	merge xs [] = xs
>	merge [] ys = ys
>	merge xs@(x:xs') ys@(y:ys') | x<y   = x : merge xs' ys
>			    	    | x==y  = x : merge xs' ys'
>			    	    | x>y   = y : merge xs ys'

> setIntersect (MkSet xs) (MkSet ys) =
>   MkSet (intersect xs ys) where
>	intersect [] ys = []
>	intersect xs [] = []
>	intersect xs@(x:xs') ys@(y:ys') | x<y   = intersect xs' ys
>					| x==y  = x : intersect xs' ys'
>					| x>y	= intersect xs ys'

> setDifference (MkSet xs) (MkSet ys) = 
>   MkSet (difference xs ys) where
>	difference [] ys = []
>	difference xs [] = xs
>	difference xs@(x:xs') ys@(y:ys') | x<y   = x : difference xs' ys
>					 | x==y  = difference xs' ys'
>					 | x>y	 = difference xs ys'

> setUnionList ss = foldr setUnion setEmpty ss

\section{Bags}

MkBags are represented by unordered lists

> data Bag a = MkBag [a]
> bagEmpty = MkBag []
> bagSingleton x = MkBag [x]
> bagFromList xs = MkBag xs
> bagToList (MkBag xs) = xs
> bagInsert x (MkBag xs) = MkBag (x:xs)
> bagUnion (MkBag xs) (MkBag ys) = MkBag (xs ++ ys)


\section{Generally useful functions}

> mapAccuml :: (b -> a -> (b, c)) 	-- Function of elt of input list
>					-- and accumulator, returning new
>					-- accumulator and elt of result list
>	    -> b 		-- Initial accumulator
>	    -> [a] 		-- Input list
>	    -> (b, [c])		-- Final accumulator and result list
>
> mapAccuml f b []     = (b, [])
> mapAccuml f b (x:xs) = (b'', x':xs') where 
>					  (b', x') = f b x 
>					  (b'', xs') = mapAccuml f b' xs

|sortNoDups| sorts a list and removes duplicates from it.

> sortNoDups :: Ord a => [a] -> [a]
> sortNoDups [] = []
> sortNoDups (x:xs) = sortNoDups [y | y <- xs, y < x] 
>		      ++ [x] ++
>		      sortNoDups [y | y <- xs, y > x]


