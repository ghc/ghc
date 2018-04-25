-----------------------------------------------------------------------------
Some General Utilities, including sorts, etc.
This is realy just an extended prelude.
All the code below is understood to be in the public domain.
-----------------------------------------------------------------------------

> module GenUtils (

>       partition', tack, 
>       assocMaybeErr,
>       arrElem,
>       memoise,
>	returnMaybe,handleMaybe, findJust,
>       MaybeErr(..),
>       mapMaybe,
>       maybeMap,
>       joinMaybe,
>       mkClosure,
>       foldb,
>	listArray',
>       cjustify,
>       ljustify,
>       rjustify,
>       space,
>       copy,
>	combinePairs,
>	--trace,		-- re-export it 
>	fst3,
>	snd3,
>	thd3,
>	mapDollarDollar,
>	str, char, nl, brack, brack',
>	interleave, interleave',
>	strspace, maybestr
>        ) where

> import Data.Char  (isAlphaNum)
> import Data.List
> import Data.Ix    ( Ix(..) )
> import Data.Array ( Array, listArray, array, (!) )

%------------------------------------------------------------------------------

Here are two defs that everyone seems to define ... 
HBC has it in one of its builtin modules

> mapMaybe :: (a -> Maybe b) -> [a] -> [b]
> mapMaybe f [] = []
> mapMaybe f (a:r) = case f a of
>                       Nothing -> mapMaybe f r
>                       Just b  -> b : mapMaybe f r

> maybeMap :: (a -> b) -> Maybe a -> Maybe b
> maybeMap f (Just a) = Just (f a)
> maybeMap f Nothing  = Nothing

> joinMaybe :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a 
> joinMaybe _ Nothing  Nothing  = Nothing
> joinMaybe _ (Just g) Nothing  = Just g
> joinMaybe _ Nothing  (Just g) = Just g
> joinMaybe f (Just g) (Just h) = Just (f g h)

> data MaybeErr a err = Succeeded a | Failed err deriving (Eq,Show)

@mkClosure@ makes a closure, when given a comparison and iteration loop. 
Be careful, because if the functional always makes the object different, 
This will never terminate.

> mkClosure :: (a -> a -> Bool) -> (a -> a) -> a -> a
> mkClosure eq f = match . iterate f
>   where
>       match (a:b:c) | a `eq` b = a
>       match (_:c)              = match c

> foldb :: (a -> a -> a) -> [a] -> a
> foldb f [] = error "can't reduce an empty list using foldb"
> foldb f [x] = x
> foldb f l  = foldb f (foldb' l)
>    where 
>       foldb' (x:y:x':y':xs) = f (f x y) (f x' y') : foldb' xs
>       foldb' (x:y:xs) = f x y : foldb' xs
>       foldb' xs = xs

> returnMaybe :: a -> Maybe a
> returnMaybe = Just

> handleMaybe :: Maybe a -> Maybe a -> Maybe a
> handleMaybe m k = case m of
>                Nothing -> k
>                _ -> m
 
> findJust :: (a -> Maybe b) -> [a] -> Maybe b
> findJust f = foldr handleMaybe Nothing . map f


Gofer-like stuff:

> fst3 (a,_,_) = a
> snd3 (_,a,_) = a
> thd3 (_,_,a) = a

> cjustify, ljustify, rjustify :: Int -> String -> String
> cjustify n s = space halfm ++ s ++ space (m - halfm)
>                where m     = n - length s
>                      halfm = m `div` 2
> ljustify n s = s ++ space (max 0 (n - length s))
> rjustify n s = space (n - length s) ++ s

> space       :: Int -> String
> space n      = copy n ' '

> copy  :: Int -> a -> [a]      -- make list of n copies of x
> copy n x = take n xs where xs = x:xs

> partition' :: (Eq b) => (a -> b) -> [a] -> [[a]]
> partition' f [] = []
> partition' f [x] = [[x]]
> partition' f (x:x':xs) | f x == f x' 
>    = tack x (partition' f (x':xs))
>                       | otherwise 
>    = [x] : partition' f (x':xs)

> tack x xss = (x : head xss) : tail xss

> combinePairs :: (Ord a) => [(a,b)] -> [(a,[b])]
> combinePairs xs = 
>	combine [ (a,[b]) | (a,b) <- sortBy (\ (a,_) (b,_) -> compare a b) xs]
>  where
>	combine [] = []
>	combine ((a,b):(c,d):r) | a == c = combine ((a,b++d) : r)
>	combine (a:r) = a : combine r
> 

> assocMaybeErr :: (Eq a) => [(a,b)] -> a -> MaybeErr b String
> assocMaybeErr env k = case [ val | (key,val) <- env, k == key] of
>                        [] -> Failed "assoc: "
>                        (val:vs) -> Succeeded val
> 

Now some utilties involving arrays.  Here is a version of @elem@ that
uses partial application to optimise lookup.

> arrElem :: (Ix a, Ord a) => [a] -> a -> Bool
> arrElem obj = \x -> inRange size x && arr ! x 
>   where
>       obj' = sort obj
>       size = (head obj',last obj')
>       arr = listArray size [ i `elem` obj | i <- range size ]


You can use this function to simulate memoisation. For example:

      > fib = memoise (0,100) fib'
      >   where
      >       fib' 0 = 0
      >       fib' 1 = 0
      >       fib' n = fib (n-1) + fib (n-2)

will give a very efficent variation of the fib function.


> memoise :: (Ix a) => (a,a) -> (a -> b) -> a -> b
> memoise bds f = (!) arr
>   where arr = array bds [ (t, f t) | t <- range bds ]

> listArray' :: (Int,Int) -> [a] -> Array Int a
> listArray' (low,up) elems = 
>	if length elems /= up-low+1 then error "wibble" else
>	listArray (low,up) elems



Replace $$ with an arbitrary string, being careful to avoid ".." and '.'.

> mapDollarDollar :: String -> Maybe (String -> String)
> mapDollarDollar code = go code ""
>   where go code acc =
>           case code of
>		[] -> Nothing
>	
>		'"'  :r    -> case reads code :: [(String,String)] of
>				 []      -> go r ('"':acc)
>				 (s,r):_ -> go r (reverse (show s) ++ acc)
>		a:'\'' :r | isAlphaNum a -> go r ('\'':a:acc)
>		'\'' :r    -> case reads code :: [(Char,String)] of
>				 []      -> go r ('\'':acc)
>				 (c,r):_ -> go r (reverse (show c) ++ acc)
>		'\\':'$':r -> go r ('$':acc)
>		'$':'$':r  -> Just (\repl -> reverse acc ++ repl ++ r)
>		c:r  -> go r (c:acc)


%-------------------------------------------------------------------------------
Fast string-building functions. 

> str = showString
> char c = (c :)
> interleave s = foldr (\a b -> a . str s . b) id
> interleave' s = foldr1 (\a b -> a . str s . b) 

> strspace = char ' '
> nl = char '\n'

> maybestr (Just s)	= str s
> maybestr _		= id

> brack s = str ('(' : s) . char ')'
> brack' s = char '(' . s . char ')'


