-- CLAUSIFY: Reducing Propositions to Clausal Form
-- Colin Runciman, University of York, 10/90
--
-- An excellent benchmark is: (a = a = a) = (a = a = a) = (a = a = a)
--
-- Optimised version: based on Runciman & Wakeling [1993]
-- Patrick Sansom, University of Glasgow, 2/94
--
-- Char# specialisation test
-- Patrick Sansom, University of Glasgow, 12/94

module Main ( main ) where

-- the main program: reads stdin and writes stdout
main = _scc_ "CAF:main" 
    do
	input <- getContents
	putStr (clausify input)

-- convert lines of propostions input to clausal forms
clausify input = _scc_ "clausify"
	         concat
		 (interleave (repeat "prop> ")
		             (map clausifyline (lines input)))

clausifyline = _scc_ "CAF:clausifyline"
	       concat . map disp . clauses . parse

-- the main pipeline from propositional formulae to printed clauses
clauses = _scc_ "CAF:clauses" unicl . split . disin . negin . elim

-- clauses = (_scc_ "unicl" unicl) . (_scc_ "split" split) .
--           (_scc_ "disin" disin) . (_scc_ "negin" negin) .
--           (_scc_ "elim"  elim)

-- clauses = (\x -> _scc_ "unicl" unicl x) .
--           (\x -> _scc_ "split" split x) .
--           (\x -> _scc_ "disin" disin x) . 
--           (\x -> _scc_ "negin" negin x) .
--           (\x -> _scc_ "elim"  elim x)

data StackFrame = Ast Formula | Lex Char

data Formula =
  Sym Char# |									-- ***
  Not Formula |
  Dis Formula Formula |
  Con Formula Formula |
  Imp Formula Formula |
  Eqv Formula Formula 

-- separate positive and negative literals, eliminating duplicates
clause p = _scc_ "clause"
           let 
           clause' (Dis p q)       x   = clause' p (clause' q x)
           clause' (Sym s)       (c,a) = (insert s c , a)
           clause' (Not (Sym s)) (c,a) = (c , insert s a)
	   in
	   clause' p ([] , [])

conjunct p = _scc_ "conjunct"
	     case p of 
	     (Con p q) -> True
	     p         -> False

-- shift disjunction within conjunction
disin p = _scc_ "disin"
	  case p of 
	  (Con p q) -> Con (disin p) (disin q)
	  (Dis p q) -> disin' (disin p) (disin q)
	  p         -> p

-- auxilary definition encoding (disin . Dis)
disin' p r = _scc_ "disin'"
	     case p of
	     (Con p q) -> Con (disin' p r) (disin' q r)
	     p         -> case r of
	                  (Con q r) -> Con (disin' p q) (disin' p r)
			  q         -> Dis p q

-- format pair of lists of propositional symbols as clausal axiom
disp p = _scc_ "disp"
	 case p of
	 (l,r) -> interleave (foldr ((:) . C#) [] l) spaces ++ "<="		-- ***
	          ++ interleave spaces (foldr ((:) . C#) [] r) ++ "\n" 		-- ***

-- eliminate connectives other than not, disjunction and conjunction
elim f = _scc_ "elim"
	 case f of
	 (Sym s)    -> Sym s
	 (Not p)    -> Not (elim p)
         (Dis p q)  -> Dis (elim p) (elim q)
         (Con p q)  -> Con (elim p) (elim q)
         (Imp p q)  -> Dis (Not (elim p)) (elim q)
         (Eqv f f') -> Con (elim (Imp f f')) (elim (Imp f' f))

-- remove duplicates and any elements satisfying p
filterset p s = _scc_ "filterset"
	        filterset' [] p s

filterset' res p l = _scc_ "filterset'"
		     case l of
		     []     -> []
                     (x:xs) -> if (notElem x res) && (p x) then
				   x : filterset' (x:res) p xs
      			       else
				   filterset' res p xs

-- insertion of an item into an ordered list
insert x l = _scc_ "insert"
	     case l of
             []     -> [x]
             (y:ys) -> if x < y then x:(y:ys)
                       else if x > y then y : insert x ys
                       else y:ys

interleave xs ys = _scc_ "interleave"
	 	   case xs of
		   (x:xs) -> x : interleave ys xs
  		   []     -> []

-- shift negation to innermost positions
negin f = _scc_ "negin"
	  case f of
          (Not (Not p))   -> negin p
          (Not (Con p q)) -> Dis (negin (Not p)) (negin (Not q))
	  (Not (Dis p q)) -> Con (negin (Not p)) (negin (Not q))
	  (Dis p q)       -> Dis (negin p) (negin q)
	  (Con p q)       -> Con (negin p) (negin q)
	  p               -> p

-- the priorities of symbols during parsing
opri c = _scc_ "opri"
	 case c of
         '(' -> 0
         '=' -> 1
         '>' -> 2
         '|' -> 3
         '&' -> 4
         '~' -> 5

-- parsing a propositional formula
parse t = _scc_ "parse" 
	  let [Ast f] = parse' t []
	  in f

parse' cs s = _scc_ "parse'"
	      case cs of
	      []      -> redstar s
	      (' ':t) -> parse' t s
	      ('(':t) -> parse' t (Lex '(' : s)
              (')':t) -> let (x : Lex '(' : s') = redstar s
	                 in  parse' t (x:s')
              (c:t)   -> if inRange ('a','z') c then 
			    parse' t (Ast (Sym (case c of C# c# -> c#)) : s)	-- ***
                         else if spri s > opri c then parse' (c:t) (red s)
                         else parse' t (Lex c : s)

-- reduction of the parse stack
red l = _scc_ "red" 
	case l of
	(Ast p : Lex '=' : Ast q : s) -> Ast (Eqv q p) : s
	(Ast p : Lex '>' : Ast q : s) -> Ast (Imp q p) : s
	(Ast p : Lex '|' : Ast q : s) -> Ast (Dis q p) : s
	(Ast p : Lex '&' : Ast q : s) -> Ast (Con q p) : s
	(Ast p : Lex '~' : s)         -> Ast (Not p) : s

-- iterative reduction of the parse stack
redstar = _scc_ "CAF:redstar" 
	  while ((/=) 0 . spri) red

spaces = _scc_ "CAF:spaces" 
	 repeat ' '

-- split conjunctive proposition into a list of conjuncts
split p = _scc_ "split" 
	  let
          split' (Con p q) a = split' p (split' q a)
          split' p a = p : a
	  in
	  split' p []

-- priority of the parse stack
spri s = _scc_ "spri"
	 case s of
	 (Ast x : Lex c : s) -> opri c
         s -> 0

-- does any symbol appear in both consequent and antecedant of clause
tautclause p = _scc_ "tautclause"
	       case p of
	       (c,a) -> -- [x | x <- c, x `elem` a] /= []
			any (\x -> x `elem` a) c

-- form unique clausal axioms excluding tautologies
unicl = _scc_ "CAF:unicl"
	filterset (not . tautclause) . map clause

-- functional while loop
while p f x = _scc_ "while"
	      if p x then while p f (f x) else x
