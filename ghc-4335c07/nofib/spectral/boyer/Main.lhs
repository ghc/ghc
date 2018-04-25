\documentstyle[a4]{article}
%;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%File:         boyer.lhs
%Description:  The Boyer benchmark
%Author:       Bob Boyer
%Created:      05-Apr-1985 Bob Boyer?
%Modified:     10-Apr-1985 Bob Shaw
%              22-Jul-1987 Will Clinger
%              21-Jul-1992 Converted from Scheme to Haskell
%Language:     Haskell
%Status:       Public Domain
%Scheme version FTP'ed from anonymous@nexus.cs.yorku.ca:pub/scheme
%;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

\title{BOYER -- Logic programming benchmark}
\author{Bob Boyer \\
Converted from Scheme to Haskell by \\
Denis Howe {\tt <dbh\@doc.ic.ac.uk>} \\
        Imperial College}
\begin{document}
\maketitle
\section{Introduction}
This is a Haskell version of the ``Boyer'' benchmark from the Gabriel
suite.  It rewrites a given input term according to a given set of
lemmas in an attempt to produce the value @True@ meaning that the
original term was a valid theorem.

Terms are either variables or applications of functions to (all) their
arguments.  Constants are represented as functions with no arguments.
Each function has an associated set of lemmas which specify how an
application of that function may be rewritten.
\begin{code}

module Main (main) where

import System.Environment

data Term               = Var Id |
                          Fun Id [Term] [Lemma]

instance Eq Term where
        Var i1 == Var i2                = i1 == i2
        Fun f1 ts1 _ == Fun f2 ts2 _    = f1 == f2 && ts1 == ts2
        _ == _                          = False

\end{code}
A lemma is a pair, @(LHS,RHS)@ representing two terms which are assumed
to be equal.  It is used to rewrite a term matching the LHS into one
obtained by substituting for the variables in the RHS.  It could in
fact be used either way round.
\begin{code}

type Lemma              = (Term, Term)

\end{code}
In attempting to match the current term against the left hand side of
a lemma, a substitution is found which binds the lemma's variables to
parts of the term.  This substitution is used as the binding
environment for variables in the right hand side.
\begin{code}

type Substitution       = [(Id, Term)]

\end{code}
All variables and functions have an identifier of type @Id@.
\begin{code}

data Id       = A | B | C | D | X | Y | Z | U | W |
                ADD1 | AND | APPEND | CONS | CONSP | DIFFERENCE |
                DIVIDES | EQUAL | EVEN | EXP | F | FALSE |
                FOUR | GCD | GREATEREQP | GREATERP | IF | IFF |
                IMPLIES | LENGTH | LESSEQP | LESSP | LISTP | MEMBER |
                NIL | NILP | NLISTP | NOT | ODD | ONE | OR | PLUS |
                QUOTIENT | REMAINDER | REVERSE | SUB1 | TIMES | TRUE |
                TWO | ZERO | ZEROP
                deriving Eq


\end{code}
\section{Unification}
Function @one_way_unify@ is used to match (part of) the term being
rewritten (@term1@) against (part of) the LHS of a lemma (@term2@).
It takes these two terms and returns @(True,subst)@ if they are
unifiable with substitution @subst@ or @(False,error)@ if they are
not.

If @term2@ is a variable and it is bound by the current substitution
then compare its bound value against @term1@.  If it is an unbound
variable then bind it to @term1@ in the returned substitution.  If
both terms are function applications then compare their functions and
attempt to unify their arguments.

The unification is ``one-way'' because only @term2@ (from the lemma)
can contain free variables, @term1@ (from the term being re-written)
cannot.
\begin{code}

one_way_unify :: Term -> Term -> (Bool, Substitution)
one_way_unify term1 term2 = one_way_unify1 term1 term2 []

one_way_unify1 :: Term -> Term -> Substitution -> (Bool, Substitution)
one_way_unify1 term1 term2@(Var vid2) subst
        = if found
          then (term1 == v2, subst)
          else (True, (vid2,term1):subst)
          where (found, v2) = find vid2 subst
{-
        = case find vid2 subst of { (found, v2) -> 
          if found
          then (term1 == v2, subst)
          else (True, (vid2,term1):subst)
          }
-}
one_way_unify1 (Fun f1 as1 _) (Fun f2 as2 _) subst
        | f1 == f2
        = one_way_unify1_lst as1 as2 subst
one_way_unify1 _ _ _ = (False, error "unify")

one_way_unify1_lst [] [] subst = (True, subst)
one_way_unify1_lst (t1:ts1) (t2:ts2) subst
        = (hd_ok && tl_ok, subst'')
          where (hd_ok, subst')  = one_way_unify1 t1 t2 subst
                (tl_ok, subst'') = one_way_unify1_lst ts1 ts2 subst'
one_way_unify1_lst _ _ _ = (False, error "unify_lst")


find :: Id -> Substitution -> (Bool, Term)
find vid []                   = (False, error "find")
find vid1 ((vid2,val2):bs)    = if vid1 == vid2
                                  then (True, val2)
                                  else find vid1 bs
\end{code}
\section{Variable substitution}
Once a substitution has been found which makes the LHS of a lemma
match a term then we apply it to the lemma's RHS to get a new term.
If the term is a variable then look up its @Id@ in the substitution.
If found, return the associated value else return the original term.
If the term is an application, apply the substitution to its
arguments.
\begin{code}

apply_subst :: Substitution -> Term -> Term
apply_subst subst term@(Var vid)
        = if found then value else term
          where (found, value) = find vid subst
apply_subst subst (Fun f args ls)
        = Fun f (map (apply_subst subst) args) ls

\end{code}
\section{Term rewriting}
Lemmas concerning a particular function are stored as part of the
@Fun@ node representing the function application.  Thus, to rewrite a
term, the lemmas associated with its top-level function are searched
until one is found whose LHS will unify with the term.  The term is
then rewritten using that lemma's RHS, after first applying any
substitutions required by the unification.  Rewriting recurses on the
new term until there are no matching lemmas.
\begin{code}

rewrite :: Term -> Term
rewrite term@(Var _)
        = term
rewrite term@(Fun f args lemmas)
        = rewrite_with_lemmas (Fun f (map rewrite args) lemmas) lemmas

rewrite_with_lemmas :: Term -> [Lemma] -> Term
rewrite_with_lemmas term []
        = term
rewrite_with_lemmas term ((lhs, rhs):ls)
        = if unified
          then rewrite (apply_subst subst rhs)
          else rewrite_with_lemmas term ls
          where (unified, subst) = one_way_unify term lhs

\end{code}
\section{Theorem proving}
This term rewriting aparatus can be used to prove theorems by
attempting to rewrite them to the constant @True@.  Terms whose
top-level function is an @IF@ are treated specially.  An @IF@ term is
true if:
\begin{itemize}
\item its condition is true and its {\em then} part is true or
\item its condition is false and its {\em else} part is true or
\item its {\em then} is true if we assume the condition is true and
      its {\em else} is true if we assume the condition is false.
\end{itemize}
The current assumptions are passed to recursive calls as @true_lst@
and @false_lst@ which are initially empty.
\begin{code}

tautp :: Term -> Bool
tautp x = tautologyp (rewrite x) [] []

tautologyp :: Term -> [Term] -> [Term] -> Bool
tautologyp x true_lst false_lst
        | truep  x true_lst     = True  -- trivially or assumed true
        | falsep x false_lst    = False -- trivially or assumed false
tautologyp (Fun IF [cond, t, e] ls) true_lst false_lst
        | truep cond true_lst   = tautologyp t true_lst false_lst
        | falsep cond false_lst = tautologyp e true_lst false_lst
        | otherwise             =  tautologyp t (cond:true_lst) false_lst
                                   && tautologyp e true_lst (cond:false_lst)
tautologyp _ _ _                = False

\end{code}
A term is true if it is the constant @TRUE@ or if it has been assumed
to be true.  Similarly a term is false if it is the constant FALSE or
if it has been assumed to be false.
\begin{code}

truep :: Term -> [Term] -> Bool
truep (Fun TRUE _ _)   _ = True
truep x l                = x `elem` l

falsep :: Term -> [Term] -> Bool
falsep (Fun FALSE _ _) _ = True
falsep x l               = x `elem` l

\end{code}
\section{Main function and top level theorem}
The top level theorem to prove and the environment binding its
variables are defined in one big mutually recursive definition along
with all the (Haskell) functions used as shorthand for @Var@ and
@Fun@ terms.

This theorem (translated directly from the Scheme version) seems
rather strange in that none of the values bound in @subst0@ are
actually relevant to the truth of the theorem.  In fact none of them
can be rewritten in any interesting way.
\begin{code}

main = do 
  (n:_) <- getArgs
  print (test (read n))

test :: Int -> Bool
test n = all test0 xs
 where xs = take n (repeat (Var X))
       {-# NOINLINE xs #-}

test0 xxxx = tautp (apply_subst subst0 theorem)
 where
        subst0 = [
                  (X,   f (plus (plus a b) (plus c zero))),
                  (Y,   f (times (times a b) (plus c d))),
                  (Z,   f (reverse_ (append (append a b) nil))),
                  (U,   equal (plus a b) (difference x y)),
                  (W,   lessp (remainder a b) (member a (length_ b)))
                 ]
        theorem = implies (and_ (implies xxxx y)
                                (and_ (implies y z)
                                      (and_ (implies z u) (implies u w))))
                          (implies x w)
-- Variables

        a       = Var A
        b       = Var B
        c       = Var C
        d       = Var D
        u       = Var U
        w       = Var W
        x       = Var X
        y       = Var Y
        z       = Var Z

-- Constants

        false   = Fun FALSE [] []
        nil     = Fun NIL   [] []
        true    = Fun TRUE  [] []
        zero    = Fun ZERO  [] [] 

-- Functions with their associated lemmas

        add1 a  = Fun ADD1 [a] []
        and_ a b
                = Fun AND [a,b] [
                  (and_ x y,            if_ x (if_ y true false) false)]
        append a b
                = Fun APPEND [a,b] [
                  (append (append x y) z,       append x (append y z))]
        cons a b
                = Fun CONS [a,b] []
        consp a = Fun CONSP [a] [
                  (consp (cons x y),            true)]
        difference a b                  -- natural numbers
                = Fun DIFFERENCE [a,b] [
                  (difference x x,                      zero),
                  (difference (plus x y) x,             y),
                  (difference (plus y x) x,             y),
                  (difference (plus x y) (plus x z),    difference y z),
                  (difference (plus y (plus x z)) x,    plus y z),
                  (difference (add1 (plus y z)) z,      add1 y),
                  (difference (add1 (add1 x)) two,      x)]
        divides a b
                = Fun DIVIDES [a,b] [
                  (divides x y,                 zerop (remainder y x))]
        equal a b
                = Fun EQUAL [a,b] [
                  (equal (plus x y) zero,       and_ (zerop x) (zerop y)),
                  (equal (plus x y) (plus x z), equal y z),
                  (equal zero (difference x y), not_ (lessp y x)),
                  (equal x (difference x y),    or_ (equal x zero)
                                                    (zerop y)),
                  (equal (times x y) zero,      or_ (zerop x) (zerop y)),
                  (equal (append x y) (append x z), equal y z),
                  (equal y (times x y),         or_ (equal y zero)
                                                    (equal x one)),
                  (equal x (times x y),         or_ (equal x zero)
                                                    (equal y one)),
                  (equal (times x y) one,       and_ (equal x one)
                                                     (equal y one)),
                  (equal (difference x y)
                         (difference z y),      if_ (lessp x y)
                                                    (not_ (lessp y z))
                                                    (if_ (lessp z y)
                                                         (not_ (lessp y x))
                                                         (equal x z))),
                  (equal (lessp x y) z,         if_ (lessp x y)
                                                    (equal true z)
                                                    (equal false z))]
        even_ a = Fun EVEN [a] [
                  (even_ x,                     if_ (zerop x)
                                                    true
                                                    (odd_ (sub1 x)))]
        exp_ a b
                = Fun EXP [a,b] [
                  (exp_ x (plus y z),           times (exp_ x y) (exp_ x z)),
                  (exp_ x (times y z),          exp_ (exp_ x y) z)]
        f a     = Fun F [a] []
        four    = Fun FOUR [] [
                  (four,                        add1 (add1 two))]
        gcd_ a b
                = Fun GCD [a,b] [
                  (gcd_ x y,                     gcd_ y x),
                  (gcd_ (times x z) (times y z), times z (gcd_ x y))]
        greatereqp a b
                = Fun GREATEREQP [a,b] [
                  (greatereqp x y,              not_ (lessp x y))]
        greaterp a b
                = Fun GREATERP [a,b] [
                  (greaterp x y,                lessp y x)]
        if_ a b c
                = Fun IF [a,b,c] [
                  (if_ (if_ x y z) u w,         if_ x (if_ y u w) (if_ z u w))]
        iff a b = Fun IFF [a,b] [
                  (iff x y,                     and_ (implies x y)
                                                     (implies y x))]
        implies a b
                = Fun IMPLIES [a,b] [
                  (implies x y,                 if_ x (if_ y true false) true)]
        length_ a
                = Fun LENGTH [a] [
                  (length_ (reverse_ x),        length_ x),
                  (length_ (cons x (cons y (cons z (cons u w)))),
                                                plus four (length_
                                                w))]
        lesseqp a b
                = Fun LESSEQP [a,b] [
                  (lesseqp x y,                 not_ (lessp y x))]
        lessp a b
                = Fun LESSP [a,b] [
                  (lessp (remainder x y) y,     not_ (zerop y)),
                  (lessp (quotient x y) x,      and_ (not_ (zerop x))
                                                     (lessp one y)),
                  (lessp (plus x y) (plus x z), lessp y z),
                  (lessp (times x z) (times y z),
                                                and_ (not_ (zerop z))
                                                     (lessp x y)),
                  (lessp y (plus x y),          not_ (zerop x))]
        listp a = Fun LISTP [a] [
                  (listp x,                     or_ (nilp x) (consp x))]
        member a b
                = Fun MEMBER [a,b] [
                  (member x (append y z),       or_ (member x y) (member x z)),
                  (member x (reverse_ y),       member x y)]
        nilp a  = Fun NILP [a] [
                  (nilp x,                      equal x nil)]
        nlistp a
                = Fun NLISTP [a] [
                  (nlistp x,                    not_ (listp x))]
        not_ a  = Fun NOT [a] [
                  (not_ x,                      if_ x false true)]
        odd_ a  = Fun ODD [a] [
                  (odd_ x,                      even_ (sub1 x))]
        one     = Fun ONE [] [
                  (one,                         add1 zero)]
        or_ a b = Fun OR [a,b] [
                  (or_ x y,                     if_ x true (if_ y true false))]
        plus a b
                = Fun PLUS [a,b] [
                  (plus (plus x y) z,           plus x (plus y z)),
                  (plus (remainder x y)
                      (times y (quotient x y)), x),
                  (plus x (add1 y),             add1 (plus x y))]
        quotient a b
                = Fun QUOTIENT [a,b] [
                  (quotient (plus x (plus x y))
                            two,                plus x (quotient y two)),
                  (quotient (times y x) y,      if_ (zerop y) zero x)]
        remainder a b
                = Fun REMAINDER [a,b] [
                  (remainder x one,             zero),
                  (remainder x x,               zero),
                  (remainder (times x y) x,     zero),
                  (remainder (times x y) y,     zero)]

        reverse_ a
                = Fun REVERSE [a] [
                  (reverse_ (append x y),       append (reverse_ y)
                                                       (reverse_ x))]
        sub1 a  = Fun SUB1 [a] [
                  (sub1 (add1 x),               x)]
        times a b
                = Fun TIMES [a,b] [
                  (times x (plus y z),          plus (times x y) (times x z)),
                  (times (times x y) z,         times x (times y z)),
                  (times x (difference y z),    difference (times y x)
                                                           (times z x)),
                  (times x (add1 y),            plus x (times x y))]
        two     = Fun TWO [] [
                  (two,                         add1 one)]
        zerop a = Fun ZEROP [a] [
                  (zerop x,                     equal x zero)]

\end{code}
\section{Original Scheme version}
The source of the Scheme version is included here for comparison.
Many of the lemmas included here have not been translated to Haskell
because they are not used by the top-level expression.

The Scheme version uses property lists to associate lemmas with their
head Lisp symbol.  The procedure @setup@ takes all the lemmas and adds
them to the appropriate symbols.  This work is not included in the
time for the benchmark.

The main advantage of Lisp over Haskell evident from this translation
is the ability of Lisp to manipulate Lisp programs which means that it
is not necessary to invent a new syntax in which to write the terms we
are manipulating.

The main disadvantage of Lisp shown here is the incomprehensibility of
code which takes apart structured values using @car@, @cdr@, @cadr@,
@caddr@ etc..  This is made much clearer by Haskell's use of pattern
matching and user data types.  It is sometimes unclear whether the
Lisp program is dealing with a list of terms, a single term, a number
or a truth value.  Haskell's strong typing is a great bonus here.

\begin{verbatim}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; File:         boyer.sch
; Description:  The Boyer benchmark
; Author:       Bob Boyer
; Created:      5-Apr-85
; Modified:     10-Apr-85 14:52:20 (Bob Shaw)
;               22-Jul-87 (Will Clinger)
; Language:     Scheme (but see note)
; Status:       Public Domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Note:  This benchmark uses property lists.  The procedures that must
; be supplied are get and put, where (put x y z) is equivalent to Common
; Lisp's (setf (get x y) z).
; Note:  The Common Lisp version of this benchmark returns the wrong
; answer because it uses the Common Lisp equivalent of memv instead of
; member in the falsep and truep procedures.  (The error arose because
; memv is called member in Common Lisp.  Don't ask what member is called,
; unless you want to learn about keyword arguments.)  This Scheme version
; may run a few percent slower than it would if it were equivalent to
; the Common Lisp version, but it works.

;;; BOYER -- Logic programming benchmark, originally written by Bob Boyer.
;;; Fairly CONS intensive.

(define unify-subst 0)
(define temp-temp 0)

(define (add-lemma term)
  (cond ((and (pair? term)
              (eq? (car term)
                   (quote equal))
              (pair? (cadr term)))
         (put (car (cadr term))
              (quote lemmas)
              (cons term (get (car (cadr term)) (quote lemmas)))))
        (else (error "ADD-LEMMA did not like term:  " term))))

(define (add-lemma-lst lst)
  (cond ((null? lst)
         #t)
        (else (add-lemma (car lst))
              (add-lemma-lst (cdr lst)))))

(define (apply-subst alist term)
  (cond ((not (pair? term))
         (cond ((begin (set! temp-temp (assq term alist))
                       temp-temp)
                (cdr temp-temp))
               (else term)))
        (else (cons (car term)
                    (apply-subst-lst alist (cdr term))))))

(define (apply-subst-lst alist lst)
  (cond ((null? lst)
         #f)
        (else (cons (apply-subst alist (car lst))
                    (apply-subst-lst alist (cdr lst))))))

(define (falsep x lst)
  (or (equal? x (quote (f)))
      (member x lst)))

(define (one-way-unify term1 term2)
  (begin (set! unify-subst #f)
         (one-way-unify1 term1 term2)))

(define (one-way-unify1 term1 term2)
  (cond ((not (pair? term2))
         (cond ((begin (set! temp-temp (assq term2 unify-subst))
                       temp-temp)
                (equal? term1 (cdr temp-temp)))
               (else (set! unify-subst (cons (cons term2 term1)
                                             unify-subst))
                     #t)))
        ((not (pair? term1))
         #f)
        ((eq? (car term1)
              (car term2))
         (one-way-unify1-lst (cdr term1)
                             (cdr term2)))
        (else #f)))

(define (one-way-unify1-lst lst1 lst2)
  (cond ((null? lst1)
         #t)
        ((one-way-unify1 (car lst1)
                         (car lst2))
         (one-way-unify1-lst (cdr lst1)
                             (cdr lst2)))
        (else #f)))

(define (rewrite term)
  (cond ((not (pair? term))
         term)
        (else (rewrite-with-lemmas (cons (car term)
                                         (rewrite-args (cdr term)))
                                   (get (car term)
                                        (quote lemmas))))))

(define (rewrite-args lst)
  (cond ((null? lst)
         #f)
        (else (cons (rewrite (car lst))
                    (rewrite-args (cdr lst))))))

(define (rewrite-with-lemmas term lst)
  (cond ((null? lst)
         term)
        ((one-way-unify term (cadr (car lst)))
         (rewrite (apply-subst unify-subst (caddr (car lst)))))
        (else (rewrite-with-lemmas term (cdr lst)))))

(define (setup)
  (add-lemma-lst
   (quote ((equal (compile form)
                  (reverse (codegen (optimize form)
                                    (nil))))
           (equal (eqp x y)
                  (equal (fix x)
                         (fix y)))
           (equal (greaterp x y)
                  (lessp y x))
           (equal (lesseqp x y)
                  (not (lessp y x)))
           (equal (greatereqp x y)
                  (not (lessp x y)))
           (equal (boolean x)
                  (or (equal x (t))
                      (equal x (f))))
           (equal (iff x y)
                  (and (implies x y)
                       (implies y x)))
           (equal (even1 x)
                  (if (zerop x)
                      (t)
                      (odd (1- x))))
           (equal (countps- l pred)
                  (countps-loop l pred (zero)))
           (equal (fact- i)
                  (fact-loop i 1))
           (equal (reverse- x)
                  (reverse-loop x (nil)))
           (equal (divides x y)
                  (zerop (remainder y x)))
           (equal (assume-true var alist)
                  (cons (cons var (t))
                        alist))
           (equal (assume-false var alist)
                  (cons (cons var (f))
                        alist))
           (equal (tautology-checker x)
                  (tautologyp (normalize x)
                              (nil)))
           (equal (falsify x)
                  (falsify1 (normalize x)
                            (nil)))
           (equal (prime x)
                  (and (not (zerop x))
                       (not (equal x (add1 (zero))))
                       (prime1 x (1- x))))
           (equal (and p q)
                  (if p (if q (t)
                              (f))
                        (f)))
           (equal (or p q)
                  (if p (t)
                        (if q (t)
                              (f))
                        (f)))
           (equal (not p)
                  (if p (f)
                        (t)))
           (equal (implies p q)
                  (if p (if q (t)
                              (f))
                        (t)))
           (equal (fix x)
                  (if (numberp x)
                      x
                      (zero)))
           (equal (if (if a b c)
                      d e)
                  (if a (if b d e)
                        (if c d e)))
           (equal (zerop x)
                  (or (equal x (zero))
                      (not (numberp x))))
           (equal (plus (plus x y)
                        z)
                  (plus x (plus y z)))
           (equal (equal (plus a b)
                         (zero))
                  (and (zerop a)
                       (zerop b)))
           (equal (difference x x)
                  (zero))
           (equal (equal (plus a b)
                         (plus a c))
                  (equal (fix b)
                         (fix c)))
           (equal (equal (zero)
                         (difference x y))
                  (not (lessp y x)))
           (equal (equal x (difference x y))
                  (and (numberp x)
                       (or (equal x (zero))
                           (zerop y))))
           (equal (meaning (plus-tree (append x y))
                           a)
                  (plus (meaning (plus-tree x)
                                 a)
                        (meaning (plus-tree y)
                                 a)))
           (equal (meaning (plus-tree (plus-fringe x))
                           a)
                  (fix (meaning x a)))
           (equal (append (append x y)
                          z)
                  (append x (append y z)))
           (equal (reverse (append a b))
                  (append (reverse b)
                          (reverse a)))
           (equal (times x (plus y z))
                  (plus (times x y)
                        (times x z)))
           (equal (times (times x y)
                         z)
                  (times x (times y z)))
           (equal (equal (times x y)
                         (zero))
                  (or (zerop x)
                      (zerop y)))
           (equal (exec (append x y)
                        pds envrn)
                  (exec y (exec x pds envrn)
                          envrn))
           (equal (mc-flatten x y)
                  (append (flatten x)
                          y))
           (equal (member x (append a b))
                  (or (member x a)
                      (member x b)))
           (equal (member x (reverse y))
                  (member x y))
           (equal (length (reverse x))
                  (length x))
           (equal (member a (intersect b c))
                  (and (member a b)
                       (member a c)))
           (equal (nth (zero)
                       i)
                  (zero))
           (equal (exp i (plus j k))
                  (times (exp i j)
                         (exp i k)))
           (equal (exp i (times j k))
                  (exp (exp i j)
                       k))
           (equal (reverse-loop x y)
                  (append (reverse x)
                          y))
           (equal (reverse-loop x (nil))
                  (reverse x))
           (equal (count-list z (sort-lp x y))
                  (plus (count-list z x)
                        (count-list z y)))
           (equal (equal (append a b)
                         (append a c))
                  (equal b c))
           (equal (plus (remainder x y)
                        (times y (quotient x y)))
                  (fix x))
           (equal (power-eval (big-plus1 l i base)
                              base)
                  (plus (power-eval l base)
                        i))
           (equal (power-eval (big-plus x y i base)
                              base)
                  (plus i (plus (power-eval x base)
                                (power-eval y base))))
           (equal (remainder y 1)
                  (zero))
           (equal (lessp (remainder x y)
                         y)
                  (not (zerop y)))
           (equal (remainder x x)
                  (zero))
           (equal (lessp (quotient i j)
                         i)
                  (and (not (zerop i))
                       (or (zerop j)
                           (not (equal j 1)))))
           (equal (lessp (remainder x y)
                         x)
                  (and (not (zerop y))
                       (not (zerop x))
                       (not (lessp x y))))
           (equal (power-eval (power-rep i base)
                              base)
                  (fix i))
           (equal (power-eval (big-plus (power-rep i base)
                                        (power-rep j base)
                                        (zero)
                                        base)
                              base)
                  (plus i j))
           (equal (gcd x y)
                  (gcd y x))
           (equal (nth (append a b)
                       i)
                  (append (nth a i)
                          (nth b (difference i (length a)))))
           (equal (difference (plus x y)
                              x)
                  (fix y))
           (equal (difference (plus y x)
                              x)
                  (fix y))
           (equal (difference (plus x y)
                              (plus x z))
                  (difference y z))
           (equal (times x (difference c w))
                  (difference (times c x)
                              (times w x)))
           (equal (remainder (times x z)
                             z)
                  (zero))
           (equal (difference (plus b (plus a c))
                              a)
                  (plus b c))
           (equal (difference (add1 (plus y z))
                              z)
                  (add1 y))
           (equal (lessp (plus x y)
                         (plus x z))
                  (lessp y z))
           (equal (lessp (times x z)
                         (times y z))
                  (and (not (zerop z))
                       (lessp x y)))
           (equal (lessp y (plus x y))
                  (not (zerop x)))
           (equal (gcd (times x z)
                       (times y z))
                  (times z (gcd x y)))
           (equal (value (normalize x)
                         a)
                  (value x a))
           (equal (equal (flatten x)
                         (cons y (nil)))
                  (and (nlistp x)
                       (equal x y)))
           (equal (listp (gopher x))
                  (listp x))
           (equal (samefringe x y)
                  (equal (flatten x)
                         (flatten y)))
           (equal (equal (greatest-factor x y)
                         (zero))
                  (and (or (zerop y)
                           (equal y 1))
                       (equal x (zero))))
           (equal (equal (greatest-factor x y)
                         1)
                  (equal x 1))
           (equal (numberp (greatest-factor x y))
                  (not (and (or (zerop y)
                                (equal y 1))
                            (not (numberp x)))))
           (equal (times-list (append x y))
                  (times (times-list x)
                         (times-list y)))
           (equal (prime-list (append x y))
                  (and (prime-list x)
                       (prime-list y)))
           (equal (equal z (times w z))
                  (and (numberp z)
                       (or (equal z (zero))
                           (equal w 1))))
           (equal (greatereqpr x y)
                  (not (lessp x y)))
           (equal (equal x (times x y))
                  (or (equal x (zero))
                      (and (numberp x)
                           (equal y 1))))
           (equal (remainder (times y x)
                             y)
                  (zero))
           (equal (equal (times a b)
                         1)
                  (and (not (equal a (zero)))
                       (not (equal b (zero)))
                       (numberp a)
                       (numberp b)
                       (equal (1- a)
                              (zero))
                       (equal (1- b)
                              (zero))))
           (equal (lessp (length (delete x l))
                         (length l))
                  (member x l))
           (equal (sort2 (delete x l))
                  (delete x (sort2 l)))
           (equal (dsort x)
                  (sort2 x))
           (equal (length (cons x1
                                (cons x2
                                      (cons x3 (cons x4
                                                     (cons x5
                                                           (cons x6 x7)))))))
                  (plus 6 (length x7)))
           (equal (difference (add1 (add1 x))
                              2)
                  (fix x))
           (equal (quotient (plus x (plus x y))
                            2)
                  (plus x (quotient y 2)))
           (equal (sigma (zero)
                         i)
                  (quotient (times i (add1 i))
                            2))
           (equal (plus x (add1 y))
                  (if (numberp y)
                      (add1 (plus x y))
                      (add1 x)))
           (equal (equal (difference x y)
                         (difference z y))
                  (if (lessp x y)
                      (not (lessp y z))
                      (if (lessp z y)
                          (not (lessp y x))
                          (equal (fix x)
                                 (fix z)))))
           (equal (meaning (plus-tree (delete x y))
                           a)
                  (if (member x y)
                      (difference (meaning (plus-tree y)
                                           a)
                                  (meaning x a))
                      (meaning (plus-tree y)
                               a)))
           (equal (times x (add1 y))
                  (if (numberp y)
                      (plus x (times x y))
                      (fix x)))
           (equal (nth (nil)
                       i)
                  (if (zerop i)
                      (nil)
                      (zero)))
           (equal (last (append a b))
                  (if (listp b)
                      (last b)
                      (if (listp a)
                          (cons (car (last a))
                                b)
                          b)))
           (equal (equal (lessp x y)
                         z)
                  (if (lessp x y)
                      (equal t z)
                      (equal f z)))
           (equal (assignment x (append a b))
                  (if (assignedp x a)
                      (assignment x a)
                      (assignment x b)))
           (equal (car (gopher x))
                  (if (listp x)
                      (car (flatten x))
                      (zero)))
           (equal (flatten (cdr (gopher x)))
                  (if (listp x)
                      (cdr (flatten x))
                      (cons (zero)
                            (nil))))
           (equal (quotient (times y x)
                            y)
                  (if (zerop y)
                      (zero)
                      (fix x)))
           (equal (get j (set i val mem))
                  (if (eqp j i)
                      val
                      (get j mem)))))))

(define (tautologyp x true-lst false-lst)
  (cond ((truep x true-lst)
         #t)
        ((falsep x false-lst)
         #f)
        ((not (pair? x))
         #f)
        ((eq? (car x)
              (quote if))
         (cond ((truep (cadr x)
                       true-lst)
                (tautologyp (caddr x)
                            true-lst false-lst))
               ((falsep (cadr x)
                        false-lst)
                (tautologyp (cadddr x)
                            true-lst false-lst))
               (else (and (tautologyp (caddr x)
                                      (cons (cadr x)
                                            true-lst)
                                      false-lst)
                          (tautologyp (cadddr x)
                                      true-lst
                                      (cons (cadr x)
                                            false-lst))))))
        (else #f)))

(define (tautp x)
  (tautologyp (rewrite x)
              #f #f))

(define (test)
  (define ans #f)
  (define term #f)
  (set! term
        (apply-subst
         (quote ((x f (plus (plus a b)
                            (plus c (zero))))
                 (y f (times (times a b)
                             (plus c d)))
                 (z f (reverse (append (append a b)
                                       (nil))))
                 (u equal (plus a b)
                          (difference x y))
                 (w lessp (remainder a b)
                          (member a (length b)))))
         (quote (implies (and (implies x y)
                              (and (implies y z)
                                   (and (implies z u)
                                        (implies u w))))
                         (implies x w)))))
  (set! ans (tautp term))
  ans)

(define (trans-of-implies n)
  (list (quote implies)
        (trans-of-implies1 n)
        (list (quote implies)
              0 n)))

(define (trans-of-implies1 n)
  (cond ((equal? n 1)
         (list (quote implies)
               0 1))
        (else (list (quote and)
                    (list (quote implies)
                          (- n 1)
                          n)
                    (trans-of-implies1 (- n 1))))))

(define (truep x lst)
  (or (equal? x (quote (t)))
      (member x lst)))

(setup)

;;; make sure you've run (setup) then call:  (test)

(run-benchmark "Boyer" (lambda () (test)))
\end{verbatim}
\end{document}
%Local Variables:
%mode:latex
%compile-command:"ghc -Rmax-heapsize 16M boyer.lhs"
%End:
