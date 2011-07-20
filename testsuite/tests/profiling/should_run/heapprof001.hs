{-
From: dw@minster.york.ac.uk
To: partain
Subject:    a compiler test
Date:        3 Mar 1992 12:31:00 GMT

Will,
   One of the decisions taken at the FLARE meeting yesterday was that we 
(FLARE people) should send you (GRASP people) interesting Haskell programs 
to test your new compiler. So allow me to present the following program, 
written by Colin Runciman in various functional languages over the years,
which puts propositions into clausal form. The original program was 
interactive, but I've made it batch so that you can run it over night.
Here is an example run with the prototype compiler. Note the result is 
"a <=".

	hc clausify.hs
	Haskell-0.41 (EXPERIMENTAL)
	Glasgow University Haskell Compiler, version 0.41
	G-Code version
	-71$ a.out
	a <= 
	-71$

Cheers,

David
-}

------------------------------------------------------------------------------
-- reducing propositions to clausal form
-- Colin Runciman, University of York, 18/10/90

-- an excellent benchmark is: (a = a = a) = (a = a = a) = (a = a = a)
-- batch mode version David Wakeling, February 1992

module Main(main) where

import Data.Ix
import System.Environment

main = do
  (n:_) <- getArgs
  putStr (res (read n))

res n = concat (map clauses xs)
 where xs = take n (repeat "(a = a = a) = (a = a = a) = (a = a = a)")
       {-# NOINLINE xs #-}

data StackFrame = Ast Formula | Lex Char 

data Formula =
  Sym Char |
  Not Formula |
  Dis Formula Formula |
  Con Formula Formula |
  Imp Formula Formula |
  Eqv Formula Formula 

-- separate positive and negative literals, eliminating duplicates
clause p = clause' p ([] , [])
           where
           clause' (Dis p q)       x   = clause' p (clause' q x)
           clause' (Sym s)       (c,a) = (insert s c , a)
           clause' (Not (Sym s)) (c,a) = (c , insert s a)

-- the main pipeline from propositional formulae to printed clauses
clauses = concat . map disp . unicl . split . disin . negin . elim . parse

conjunct (Con p q) = True
conjunct p = False

-- shift disjunction within conjunction
disin (Dis p (Con q r)) = Con (disin (Dis p q)) (disin (Dis p r))
disin (Dis (Con p q) r) = Con (disin (Dis p r)) (disin (Dis q r))
disin (Dis p q) =
  if conjunct dp || conjunct dq then disin (Dis dp dq)
  else (Dis dp dq)
  where
  dp = disin p
  dq = disin q
disin (Con p q) = Con (disin p) (disin q)
disin p = p

-- format pair of lists of propositional symbols as clausal axiom
disp (l,r) = interleave l spaces ++ "<=" ++ interleave spaces r ++ "\n"

-- eliminate connectives other than not, disjunction and conjunction
elim (Sym s) = Sym s
elim (Not p) = Not (elim p)
elim (Dis p q) = Dis (elim p) (elim q)
elim (Con p q) = Con (elim p) (elim q)
elim (Imp p q) = Dis (Not (elim p)) (elim q)
elim (Eqv f f') = Con (elim (Imp f f')) (elim (Imp f' f))

-- the priorities of propositional expressions
{- UNUSED:
fpri   (Sym c) = 6
fpri   (Not p) = 5
fpri (Con p q) = 4
fpri (Dis p q) = 3
fpri (Imp p q) = 2
fpri (Eqv p q) = 1
-}

-- insertion of an item into an ordered list
-- Note: this is a corrected version from Colin (94/05/03 WDP)
insert x [] = [x]
insert x p@(y:ys) =
  if x < y then x : p
  else if x > y then y : insert x ys
  else p


interleave (x:xs) ys = x : interleave ys xs
interleave []     _  = []

-- shift negation to innermost positions
negin (Not (Not p)) = negin p
negin (Not (Con p q)) = Dis (negin (Not p)) (negin (Not q))
negin (Not (Dis p q)) = Con (negin (Not p)) (negin (Not q))
negin (Dis p q) = Dis (negin p) (negin q)
negin (Con p q) = Con (negin p) (negin q)
negin p = p

-- the priorities of symbols during parsing
opri '(' = 0
opri '=' = 1
opri '>' = 2
opri '|' = 3
opri '&' = 4
opri '~' = 5

-- parsing a propositional formula
parse t = f where [Ast f] = parse' t []

parse' [] s = redstar s
parse' (' ':t) s = parse' t s
parse' ('(':t) s = parse' t (Lex '(' : s)
parse' (')':t) s = parse' t (x:s')
                   where
                   (x : Lex '(' : s') = redstar s
parse' (c:t) s = if inRange ('a','z') c then parse' t (Ast (Sym c) : s)
                 else if spri s > opri c then parse' (c:t) (red s)
                 else parse' t (Lex c : s)

-- reduction of the parse stack
red (Ast p : Lex '=' : Ast q : s) = Ast (Eqv q p) : s
red (Ast p : Lex '>' : Ast q : s) = Ast (Imp q p) : s
red (Ast p : Lex '|' : Ast q : s) = Ast (Dis q p) : s
red (Ast p : Lex '&' : Ast q : s) = Ast (Con q p) : s
red (Ast p : Lex '~' : s) = Ast (Not p) : s

-- iterative reduction of the parse stack
redstar = while ((/=) 0 . spri) red

-- old: partain:
--redstar = while ((/=) (0::Int) . spri) red

spaces = repeat ' '

-- split conjunctive proposition into a list of conjuncts
split p = split' p []
          where
          split' (Con p q) a = split' p (split' q a)
          split' p a = p : a

-- priority of the parse stack
spri (Ast x : Lex c : s) = opri c
spri s = 0

-- does any symbol appear in both consequent and antecedant of clause
tautclause (c,a) = [x | x <- c, x `elem` a] /= []

-- form unique clausal axioms excluding tautologies
unicl a = foldr unicl' [] a
          where
          unicl' p x = if tautclause cp then x else insert cp x
                       where
                       cp = clause p

while p f x = if p x then while p f (f x) else x
