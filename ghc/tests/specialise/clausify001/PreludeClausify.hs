-- CLAUSIFY: Reducing Propositions to Clausal Form
-- Colin Runciman, University of York, 10/90
--
-- An excellent benchmark is: (a = a = a) = (a = a = a) = (a = a = a)
--
-- Optimised version: based on Runciman & Wakeling [1993]
-- Patrick Sansom, University of Glasgow, 2/94
--
-- Char# specialisation test with prelude stuff explicit
-- Patrick Sansom, University of Glasgow, 12/94

module PreludeClausify( clausify, AList(..) ) where

-- convert lines of propostions input to clausal forms
clausify input = scc "clausify"
	         concat
		 (interleave (repeat "prop> ")
		             (map clausifyline (lines input)))

clausifyline = scc "CAF:clausifyline"
	       concat . map disp . clauses . parse

-- the main pipeline from propositional formulae to printed clauses
clauses = scc "CAF:clauses" unicl . split . disin . negin . elim

-- clauses = (scc "unicl" unicl) . (scc "split" split) .
--           (scc "disin" disin) . (scc "negin" negin) .
--           (scc "elim"  elim)

-- clauses = (\x -> scc "unicl" unicl x) .
--           (\x -> scc "split" split x) .
--           (\x -> scc "disin" disin x) . 
--           (\x -> scc "negin" negin x) .
--           (\x -> scc "elim"  elim x)

data StackFrame = Ast Formula | Lex Char

data Formula =
  Sym Char# |			-- ***
  Not Formula |
  Dis Formula Formula |
  Con Formula Formula |
  Imp Formula Formula |
  Eqv Formula Formula 

-- separate positive and negative literals, eliminating duplicates
clause p = scc "clause"
           let 
           clause' (Dis p q)       x   = clause' p (clause' q x)
           clause' (Sym s)       (c,a) = (insert s c , a)
           clause' (Not (Sym s)) (c,a) = (c , insert s a)
	   in
	   clause' p (ANil , ANil)

conjunct p = scc "conjunct"
	     case p of 
	     (Con p q) -> True
	     p         -> False

-- shift disjunction within conjunction
disin p = scc "disin"
	  case p of 
	  (Con p q) -> Con (disin p) (disin q)
	  (Dis p q) -> disin' (disin p) (disin q)
	  p         -> p

-- auxilary definition encoding (disin . Dis)
disin' p r = scc "disin'"
	     case p of
	     (Con p q) -> Con (disin' p r) (disin' q r)
	     p         -> case r of
	                  (Con q r) -> Con (disin' p q) (disin' p r)
			  q         -> Dis p q

-- format pair of lists of propositional symbols as clausal axiom
disp p = scc "disp"
	 case p of
	 (l,r) -> interleave (foldrA ((:) `o` C#) [] l) spaces ++ "<="
	          ++ interleave spaces (foldrA ((:) `o` C#) [] r) ++ "\n"

-- eliminate connectives other than not, disjunction and conjunction
elim f = scc "elim"
	 case f of
	 (Sym s)    -> Sym s
	 (Not p)    -> Not (elim p)
         (Dis p q)  -> Dis (elim p) (elim q)
         (Con p q)  -> Con (elim p) (elim q)
         (Imp p q)  -> Dis (Not (elim p)) (elim q)
         (Eqv f f') -> Con (elim (Imp f f')) (elim (Imp f' f))

-- remove duplicates and any elements satisfying p
filterset p s = scc "filterset"
	        filterset' [] p s

filterset' res p l = scc "filterset'"
		     case l of
		     []     -> []
                     (x:xs) -> if (notElem x res) && (p x) then
				   x : filterset' (x:res) p xs
      			       else
				   filterset' res p xs

-- insertion of an item into an ordered list
insert x l = scc "insert"
	     case l of
             ANil    -> x :! ANil
             (y:!ys) -> if x < y then x:!(y:!ys)
                       else if x > y then y :! insert x ys
                       else y:!ys

interleave xs ys = scc "interleave"
	 	   case xs of
		   (x:xs) -> x : interleave ys xs
  		   []     -> []

-- shift negation to innermost positions
negin f = scc "negin"
	  case f of
          (Not (Not p))   -> negin p
          (Not (Con p q)) -> Dis (negin (Not p)) (negin (Not q))
	  (Not (Dis p q)) -> Con (negin (Not p)) (negin (Not q))
	  (Dis p q)       -> Dis (negin p) (negin q)
	  (Con p q)       -> Con (negin p) (negin q)
	  p               -> p

-- the priorities of symbols during parsing
opri c = scc "opri"
	 case c of
         '(' -> 0
         '=' -> 1
         '>' -> 2
         '|' -> 3
         '&' -> 4
         '~' -> 5

-- parsing a propositional formula
parse t = scc "parse" 
	  let [Ast f] = parse' t []
	  in f

parse' cs s = scc "parse'"
	      case cs of
	      []      -> redstar s
	      (' ':t) -> parse' t s
	      ('(':t) -> parse' t (Lex '(' : s)
              (')':t) -> let (x : Lex '(' : s') = redstar s
	                 in  parse' t (x:s')
              (c:t)   -> if inRange ('a','z') c then 
			    parse' t (Ast (Sym (case c of C# c# -> c#)) : s) -- ***
                         else if spri s > opri c then parse' (c:t) (red s)
                         else parse' t (Lex c : s)

-- reduction of the parse stack
red l = scc "red" 
	case l of
	(Ast p : Lex '=' : Ast q : s) -> Ast (Eqv q p) : s
	(Ast p : Lex '>' : Ast q : s) -> Ast (Imp q p) : s
	(Ast p : Lex '|' : Ast q : s) -> Ast (Dis q p) : s
	(Ast p : Lex '&' : Ast q : s) -> Ast (Con q p) : s
	(Ast p : Lex '~' : s)         -> Ast (Not p) : s

-- iterative reduction of the parse stack
redstar = scc "CAF:redstar" 
	  while ((/=) 0 . spri) red

spaces = scc "CAF:spaces" 
	 repeat ' '

-- split conjunctive proposition into a list of conjuncts
split p = scc "split" 
	  let
          split' (Con p q) a = split' p (split' q a)
          split' p a = p : a
	  in
	  split' p []

-- priority of the parse stack
spri s = scc "spri"
	 case s of
	 (Ast x : Lex c : s) -> opri c
         s -> 0

-- does any symbol appear in both consequent and antecedant of clause
tautclause p = scc "tautclause"
	       case p of
	       (c,a) -> -- [x | x <- c, x `elem` a] /= []
			anyA (\x -> x `elemA` a) c

-- form unique clausal axioms excluding tautologies
unicl = scc "CAF:unicl"
	filterset (not . tautclause) . map clause

-- functional while loop
while p f x = scc "while"
	      if p x then while p f (f x) else x

{- STUFF FROM PRELUDE -}

data AList a = ANil | a :! (AList a)
     deriving (Eq)

elemA x ANil	= False
elemA x (y:!ys)	= x==y || elemA x ys

anyA p ANil	= False
anyA p (x:!xs)	= p x || anyA p xs

foldrA f z ANil	  =  z
foldrA f z (x:!xs)=  f x (foldrA f z xs)

o f g x = f (g x)


instance Eq Char# where
    x == y = eqChar# x y
    x /= y = neChar# x y

instance Ord Char# where
    (<=) x y = leChar# x y
    (<)	 x y = ltChar# x y
    (>=) x y = geChar# x y
    (>)  x y = gtChar# x y

    max a b = case _tagCmp a b of { _LT -> b; _EQ -> a;  _GT -> a }
    min a b = case _tagCmp a b of { _LT -> a; _EQ -> a;  _GT -> b }

    _tagCmp a b
      = if      (eqChar# a b) then _EQ
	else if (ltChar# a b) then _LT else _GT

