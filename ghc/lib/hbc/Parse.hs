module Parse(
	Parser(..), (+.+), (..+), (+..), (|||), (>>>), (||!), (|!!), (.>),
	into, lit, litp, many, many1, succeed, sepBy, count, sepBy1, testp, token, recover,
	ParseResult, parse, sParse, simpleParse,
#if __HASKELL1__ < 3
	(>>), fail
#else
	act, failP
#endif
	) where

--import Trace
#if __HASKELL1__ < 3
import {-flummox mkdependHS-}
	Maybe
import
	Either renaming (Left to Wrong)
#else
#define Wrong Left
#endif
#if defined(__HBC__)
import UnsafeDirty(seq)
#endif

infixr 8 +.+ , ..+ , +..
#if __HASKELL1__ < 3
infix  6 >> , `act` , >>>, `into` , .>
#else
infix  6 `act` , >>>, `into` , .>
#endif
infixr 4 ||| , ||! , |!!

#if !defined(__HBC__)
seq x y = y --partain: a substitute
#endif

type ErrMsg = String

data FailAt a
	= FailAt Int{-#STRICT#-} [ErrMsg] a		 	-- token pos, list of acceptable tokens, rest of tokens
	deriving (Text)
data ParseResult a b
	= Many [(b, Int, a)] (FailAt a)				-- parse succeeded with many (>1) parses)
	| One b Int{-#STRICT#-} a (FailAt a){-#STRICT#-}	-- parse succeeded with one parse
	| None Bool{-#STRICT#-} (FailAt a){-#STRICT#-}		-- parse failed. The Bool indicates hard fail
	deriving (Text)

type Parser a b = a -> Int -> ParseResult a b

noFail = FailAt (-1) [] (error "noFail")		-- indicates no failure yet

updFail f (None w f')     = None w (bestFailAt f f') 
updFail f (One c n as f') = One c n as (bestFailAt f f')
updFail f (Many cas f')   = let r = bestFailAt f f' in seq r (Many cas r)

bestFailAt f@(FailAt i a t) f'@(FailAt j a' _) =
	if i > j then 
	    f 
	else if j > i then 
	    f' 
	else if i == -1 then 
	    noFail --FailAt (-1) [] [] 
	else 
	    FailAt i (a ++ a') t

-- Alternative
(|||) :: Parser a b -> Parser a b -> Parser a b
p ||| q = \as n ->
    case (p as n, q as n) of
        (pr@(None True  _), _                ) -> pr
        (pr@(None _     f), qr               ) -> updFail f qr
	(    One b k as f , qr               ) -> Many ((b,k,as) : l') (bestFailAt f f') where (l',f') = lf qr
	(    Many  l f    , qr               ) -> Many (        l++l') (bestFailAt f f') where (l',f') = lf qr
    where lf (Many l f)     = (l,          f)
	  lf (One b k as f) = ([(b,k,as)], f)
	  lf (None _   f)   = ([],         f)

-- Alternative, but with committed choice
(||!) :: Parser a b -> Parser a b -> Parser a b 
p ||! q = \as n -> 
    case (p as n, q as n) of
        (pr@(None True  _), _                ) -> pr
        (    None _     f , qr               ) -> updFail f qr
	(pr               , _                ) -> pr

process f [] [] = seq f (None False f)
process f [(b,k,as)]  [] = seq f (One b k as f)
process f rs [] = seq f (Many rs f)
process f rs (w@(None True _):_) = seq f w
process f rs (None False f':rws) = process (bestFailAt f f') rs rws
process f rs (One b k as f':rws) = process (bestFailAt f f') (rs++[(b,k,as)]) rws
process f rs (Many rs' f'  :rws) = process (bestFailAt f f') (rs++rs') rws

doMany g cas f = Many [ (g c, n, as) | (c,n,as) <- cas] f

-- Sequence
(+.+) :: Parser a b -> Parser a c -> Parser a (b,c)
p +.+ q = 
    \as n-> 
    case p as n of
	None w f -> None w f
	One b n' as' f ->
	    case q as' n' of
		None w f'         -> None w (bestFailAt f f') 
		One c n'' as'' f' -> One (b,c) n'' as'' (bestFailAt f f')
		Many cas f'       -> doMany (\x->(b,x)) cas (bestFailAt f f')
	Many bas f ->
	    let rss = [ case q as' n' of { None w f -> None w f;
					   One c n'' as'' f' -> One (b,c) n'' as'' f';
					   Many cas f' -> doMany (\x->(b,x)) cas f'  }
                        | (b,n',as') <- bas ]
	    in  process f [] rss

-- Sequence, throw away first part
(..+) :: Parser a b -> Parser a c -> Parser a c
p ..+ q = -- p +.+ q `act` snd
    \as n-> 
    case p as n of
	None w f       -> None w f
	One _ n' as' f -> updFail f (q as' n')
	Many bas f     -> process f [] [ q as' n' | (_,n',as') <- bas ]

-- Sequence, throw away second part
(+..) :: Parser a b -> Parser a c -> Parser a b
p +.. q = -- p +.+ q `act` fst
    \as n-> 
    case p as n of
	None w f -> None w f
	One b n' as' f ->
	    case q as' n' of
		None w f'         -> None w (bestFailAt f f')
		One _ n'' as'' f' -> One b n'' as'' (bestFailAt f f')
		Many cas f'       -> doMany (const b) cas (bestFailAt f f')
        Many bas f ->
	    let rss = [ case q as' n' of { None w f -> None w f; 
					   One _ n'' as'' f' -> One b n'' as'' f';
					   Many cas f' -> doMany (const b) cas f' }
                        | (b,n',as') <- bas ]
	    in  process f [] rss

-- Return a fixed value
(.>) :: Parser a b -> c -> Parser a c
p .> v =
    \as n-> 
    case p as n of
      None w f        -> None w f
      One _ n' as' f' -> One v n' as' f'
      Many bas f      -> doMany (const v) bas f

-- Action
#if __HASKELL1__ < 3
act = (>>)
(>>) :: Parser a b -> (b->c) -> Parser a c
p >> f = \as n-> 
    case p as n of
	None w f       -> None w f
	One b n as' ff -> One (f b) n as' ff
	Many bas ff    -> doMany f bas ff
#else
act :: Parser a b -> (b->c) -> Parser a c
p `act` f = \as n-> 
    case p as n of
	None w f       -> None w f
	One b n as' ff -> One (f b) n as' ff
	Many bas ff    -> doMany f bas ff
#endif

-- Action on two items
(>>>) :: Parser a (b,c) -> (b->c->d) -> Parser a d
p >>> f = \as n-> 
    case p as n of
	None w ff          -> None w ff
	One (b,c) n as' ff -> One (f b c) n as' ff
	Many bas ff        -> doMany (\ (x,y)->f x y) bas ff

-- Use value
into :: Parser a b -> (b -> Parser a c) -> Parser a c
p `into` fq = \as n -> 
    case p as n of
	None w f       -> None w f
	One b n' as' f -> updFail f (fq b as' n')
	Many bas f     -> process f [] [ fq b as' n' | (b,n',as') <- bas ]

-- Succeeds with a value
succeed :: b -> Parser a b
succeed v = \as n -> One v n as noFail

-- Always fails.
#if __HASKELL1__ < 3
fail :: ErrMsg -> Parser a b
fail s = \as n -> None False (FailAt n [s] as)
#else
failP :: ErrMsg -> Parser a b
failP s = \as n -> None False (FailAt n [s] as)
#endif

-- Fail completely if parsing proceeds a bit and then fails
mustAll :: Parser a b -> Parser a b
mustAll p = \as n->
	case p as n of
	None False f@(FailAt x _ _) | x/=n -> None True f
	r -> r 

-- If first alternative gives partial parse it's a failure
p |!! q = mustAll p ||! q

-- Kleene star
many :: Parser a b -> Parser a [b]
many p = p `into` (\v-> many p `act` (v:))
     ||! succeed []

many1 :: Parser a b -> Parser a [b]
many1 p = p `into` (\v-> many p `act` (v:))

-- Parse an exact number of items
count :: Parser a b -> Int -> Parser a [b]
count p 0 = succeed []
count p k = p +.+ count p (k-1) >>> (:)

-- Non-empty sequence of items separated by something
sepBy1 :: Parser a b -> Parser a c -> Parser a [b]
p `sepBy1` q = p `into` (\v-> many (q ..+ p) `act` (v:))	-- p +.+ many (q ..+ p) >>> (:)    is slower

-- Sequence of items separated by something
sepBy :: Parser a b -> Parser a c -> Parser a [b]
p `sepBy` q = p `sepBy1` q
          ||! succeed []

-- Recognize a literal token
lit :: (Eq a, Text a) => a -> Parser [a] a
lit x = \as n ->
	case as of
	a:as' | a==x -> One a (n+1) as' noFail
	_ -> None False (FailAt n [show x] as)

-- Recognize a token with a predicate
litp :: ErrMsg -> (a->Bool) -> Parser [a] a
litp s p = \as n->
	case as of
	a:as' | p a -> One a (n+1) as' noFail
	_ -> None False (FailAt n [s] as)

-- Generic token recognizer
token :: (a -> Either ErrMsg (b,a)) -> Parser a b
token f = \as n->
	case f as of
	    Wrong s -> None False (FailAt n [s] as)
	    Right (b, as') -> One b (n+1) as' noFail

-- Test a semantic value
testp :: String -> (b->Bool) -> Parser a b -> Parser a b
testp s tst p = \ as n ->
    case p as n of
      None w f -> None w f
      o@(One b _ _ _) -> if tst b then o else None False (FailAt n [s] as)
      Many bas f ->
	case [ r | r@(b, _, _) <- bas, tst b] of
	    [] -> None False (FailAt n [s] as)
	    [(x,y,z)] -> One x y z f
	    rs -> Many rs f

-- Try error recovery.
recover :: Parser a b -> ([ErrMsg] -> a -> Maybe (a, b)) -> Parser a b
recover p f = \ as n ->
	case p as n of
	    r@(None _ fa@(FailAt n ss ts)) ->
		case f ss ts of
		    Nothing -> r
		    Just (a, b) -> One b (n+1) a fa
	    r -> r

-- Parse, and check if it was ok.
parse :: Parser a b -> a -> Either ([ErrMsg],a) [(b, a)]
parse p as =
	case p as 0 of
	    None w (FailAt _ ss ts) -> Wrong (ss,ts)
	    One b _ ts _            -> Right [(b,ts)]
	    Many bas _              -> Right [(b,ts) | (b,_,ts) <- bas ]

sParse :: (Text a) => Parser [a] b -> [a] -> Either String b
sParse p as =
	case parse p as of
	    Wrong (ss,ts)     -> Wrong ("Parse failed at token "++pshow ts++", expected "++unwords ss++"\n")
				  where pshow [] = "<EOF>"
				        pshow (t:_) = show t
	    Right ((b,[]):_)  -> Right b
	    Right ((_,t:_):_) -> Wrong ("Parse failed at token "++show t++", expected <EOF>\n")

simpleParse :: (Text a) => Parser [a] b -> [a] -> b
simpleParse p as =
	case sParse p as of
	Wrong msg -> error msg
	Right x -> x
