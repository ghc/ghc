> module Main where

> import Parsers
> import System.Environment
> import System.IO

> infixr 8 +.+ , +.. , ..+
> infixl 7 <<< , <<*
> infixr 6 |||
> 
> (+.+) = thn
> (..+) = xthn
> (+..) = thnx
> (|||) = alt
> (<<<) = using
> (<<*) = using2
> lit   :: Eq a => a -> Parser a a
> lit   = literal
> star  = rpt
> anyC  = satisfy (const True)
> butC cs = satisfy (not.(`elem` cs))
> noC   "" = [("","")]
> noC   _  = []

----------------------------------------------------------------------

> main = getArgs >>= \ args -> parse_args args

> parse_args :: [String] -> IO ()
> parse_args (regexp: files) =
> 	let acc = acceptor (fst(head(nnRegexp regexp)))
> 	    acc' = unlines . filter acc . lines
> 	in
> 	    getContents >>= \ inp ->
> 	    putStr (acc' inp)
> parse_args _ =
> 	getProgName >>= \progName ->
>	hPutStr stderr ("Usage: " ++ progName ++ " regexp\n")

{-
  Atom		= character | "\\" character | "." | "\\(" Regexp "\\) .
  ExtAtom	= Atom ["*" | "+" | "?"] .
  Factor	= ExtAtom + .
  Regexp	= Factor / "\\|" ["$"].
-}

> data NFANode 
>      	= NFAChar Char NFANode
> 	| NFAAny  NFANode
>	| NFAEps  [NFANode]
> 	| NFAEnd  NFANode
> 	| NFAFinal
>	| NFATable [(Char, NFANode)] [NFANode] [NFANode] Bool

NFAChar c next	- a state with arc on character c to next state
NFAAny next	- a state with arc on any character
NFAEps nexts	- a state with a set of epsilon transitions
NFAEnd		- a state with an arc if end of string is reached
NFAFinal	- a final state
NFATable charTrans anyTrans endTrans final
		- a state with character arcs according to charTrans,
		  any character arcs according to anyTrans, end arcs
		  according to endTrans, and a boolean flag indicating
		  a final state

> nfaChar = NFAChar
> nfaAny  = NFAAny
> -- nfaEps  = NFAEps
> nfaEps  = mkTable [] [] [] False . epsClosure
> nfaEnd  = NFAEnd
> nfaFinal= NFAFinal

just wrappers for the NFANode constructors, 
modified such that epsilon transitions are compressed into tables

> mkTable pairs anys ends final []      = NFATable pairs anys ends final 
> mkTable pairs anys ends final (NFAChar c n:ns) = mkTable ((c,n):pairs) anys ends final ns
> mkTable pairs anys ends final (NFAAny n:ns) = mkTable pairs (n:anys) ends final ns
> mkTable pairs anys ends final (NFATable pairs' anys' ends' final':ns) = mkTable (pairs'++pairs) (anys'++anys) (ends'++ends) (final' || final) ns
> mkTable pairs anys ends final (NFAEnd n:ns) = mkTable pairs anys (n:ends) final ns
> mkTable pairs anys ends final (NFAFinal:ns) = mkTable pairs anys ends True ns
> mkTable _ _ _ _ _ = error "illegal argument to mkTable"
> 
> type NFAproducer = NFANode -> NFANode

An NFAproducer takes a final state and produces the initial state of a
non-deterministic automaton.

> nnAtom :: Parser Char NFAproducer
> nnAtom =
>      lit '\\' ..+ lit '(' ..+ nnRegexp +.. lit '\\' +.. lit ')'
>  ||| lit '\\' ..+ butC "|()"	 <<< nfaChar
>  ||| lit '.'			 <<< const NFAAny
>  ||| butC "\\.$"		 <<< nfaChar
>  ||| lit '$' `followedBy` anyC <<< nfaChar

> nnExtAtom :: Parser Char NFAproducer
> nnExtAtom =
>      nnAtom +.+ opt (lit '*' <<< const (\ at final ->
> 					 let at_init = at (nfaEps [final, at_init])
> 					 in  nfaEps [at_init, final])
> 		|||  lit '+' <<< const (\ at final ->
> 					 let at_init = at (nfaEps [final, at_init])
> 					 in  nfaEps [at_init])
> 		|||  lit '?' <<< const (\ at final ->
> 					 let at_init = at (nfaEps [final])
> 					 in  nfaEps [final, at_init]))
> 	<<< helper
>      where
>        helper (ea, []) = ea
>        helper (ea, [f]) = f ea
> 
> nnFactor :: Parser Char NFAproducer
> nnFactor =
>      plus nnExtAtom	<<< foldr (.) id

> nnRegexp :: Parser Char NFAproducer
> nnRegexp =
>      nnFactor +.+ star (lit '\\' ..+ lit '|' ..+ nnFactor) +.+ opt (lit '$')
> 	<<< helper
>      where
>        helper (ef, (efs, [])) = foldl combine ef efs
>        helper (ef, (efs, _ )) = foldl combine ef efs . nfaEnd
>	 combine f1 f2 final = nfaEps [f1 final, f2 final]

Step function for the NFA interpreter.
Note if epsilon compression is removed above, all {- epsClosure -} must 
be uncommented!

> nfaStep states c = {- epsClosure -} (concat (map step states))
>   where
>     step (NFAChar c' n') | c == c' = [n']
>     step (NFAAny n') = [n']
>     step (NFATable pairs anys ends finals) = [ n' | (c',n') <- pairs, c == c' ] ++ anys
>     step _ = []

precondition: there are no epsilon cycles!

> epsClosure [] = []
> epsClosure (NFAEps ns:ns') = epsClosure (ns++ns')
> epsClosure (n:ns) = n:epsClosure ns

> acceptor :: NFAproducer -> String -> Bool
> acceptor nfa str = nfaRun ( {- epsClosure -} [nfa nfaFinal]) str

The NFA interpreter

> nfaRun :: [NFANode] -> String -> Bool
> nfaRun ns (c:cs) = nfaRun (nfaStep ns c) cs
> nfaRun ns [] = not (null ( {- epsClosure -} (concat (map step ns))))
>   where
>     step (NFAEnd n') = [n']
>     step (NFAFinal)  = [NFAFinal]
>     step (NFATable pairs anys ends True) = [NFAFinal]
>     step (NFATable pairs anys ends finals) = ends
>     step _           = []

