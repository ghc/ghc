> module Parse where

The Parser monad in "Comprehending Monads"

> infixr 9 `thenP`
> infixr 9 `thenP_`
> infixr 9 `plusP`

> type P t a = [t] -> [(a,[t])]

> unitP :: a -> P t a
> unitP a = \i -> [(a,i)]

> thenP :: P t a -> (a -> P t b) -> P t b
> m `thenP` k = \i0 -> [(b,i2) | (a,i1) <- m i0, (b,i2) <- k a i1]

> thenP_ :: P t a -> P t b -> P t b
> m `thenP_` k = \i0 -> [(b,i2) | (a,i1) <- m i0, (b,i2) <- k i1]

zeroP is the parser that always fails to parse its input

> zeroP :: P t a
> zeroP = \i -> []

plusP combines two parsers in parallel
(called "alt" in "Comprehending Monads")

> plusP :: P t a -> P t a -> P t a
> a1 `plusP` a2 = \i -> (a1 i) ++ (a2 i)

itemP is the parser that parses a single token
(called "next" in "Comprehending Monads")

> itemP :: P t t
> itemP = \i -> [(head i, tail i) | not (null i)]

force successful parse

> cutP :: P t a -> P t a
> cutP p = \u -> let l = p u in if null l then [] else [head l]

find all complete parses of a given string

> useP :: P t a -> [t] -> [a]
> useP m =  \x -> [ a | (a,[]) <- m x ]

find first complete parse

> theP :: P t a -> [t] -> a
> theP m = head . (useP m)


Some standard parser definitions

mapP applies f to all current parse trees

> mapP :: (a -> b) -> P t a -> P t b
> f `mapP` m =  m `thenP` (\a -> unitP (f a))

filter is the parser that parses a single token if it satisfies a
predicate and fails otherwise.

> filterP :: (a -> Bool) -> P t a -> P t a
> p `filterP` m = m `thenP` (\a -> (if p a then unitP a else zeroP))

lit recognises literals

> litP :: Eq t => t -> P t ()
> litP t = ((==t) `filterP` itemP) `thenP` (\c -> unitP () )

> showP :: (Text a) => P t a -> [t] -> String
> showP m xs = show (theP m xs)


Simon Peyton Jones adds some useful operations:

> zeroOrMoreP :: P t a -> P t [a]
> zeroOrMoreP p = oneOrMoreP p `plusP` unitP []

> oneOrMoreP :: P t a -> P t [a]
> oneOrMoreP p = seq p
>  where seq p = p		`thenP` (\a ->
>		(seq p		`thenP` (\as -> unitP (a:as)))
>		`plusP`
>		unitP [a] )

> oneOrMoreWithSepP :: P t a -> P t b -> P t [a]
> oneOrMoreWithSepP p1 p2 = seq1 p1 p2
>   where seq1 p1 p2 =	p1 `thenP` (\a -> seq2 p1 p2 a `plusP`  unitP [a])
>         seq2 p1 p2 a =	p2		`thenP` (\_ ->
>				seq1 p1 p2	`thenP` (\as -> unitP (a:as) ))

