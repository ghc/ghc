{-
    Haskell version of ...

! Text of rule base for Boyer benchmark !

! Started by Tony Kitto on 6th April 1988                                  !
! Changes Log                                                              !
! 07-04-88 ADK Corrected errors in rules
! 08-04-88 ADK Modified "or" rule to remove extra final (f) of book definition

Haskell version:

    23-06-93 JSM initial version

-}

module Rulebasetext (rules) where

rules :: [String]

-- Rule base extracted from Gabriel pages 118 to 126

rules = [
             "(equal (compile form)\
                    \(reverse (codegen (optimize form) (nil) ) ) )",
             "(equal (eqp x y)\
                    \(equal (fix x)\
                           \(fix y) ) )",
             "(equal (greaterp x y)\
                    \(lessp y x) )",
             "(equal (lesseqp x y)\
                    \(not (lessp y x) ) )",
             "(equal (greatereqp x y)\
                    \(not (lessp y x) ) )",
             "(equal (boolean x)\
                    \(or (equal x (t) )\
                        \(equal x (f) ) )",
             "(equal (iff x y)\
                    \(and (implies x y)\
                         \(implies y x) ) )",
             "(equal (even1 x)\
                    \(if (zerop x)\
                        \(t)\
                        \(odd (1- x) ) ) )",
             "(equal (countps- l pred)\
                    \(countps-loop l pred (zero) ) )",
             "(equal (fact- i)\
                    \(fact-loop i 1) )",

             "(equal (reverse- x)\
                    \(reverse-loop x (nil) ) )",
             "(equal (divides x y)\
                    \(zerop (remainder y x) ) )",
             "(equal (assume-true var alist)\
                    \(cons (cons var (t) )\
                     \alist) )",
             "(equal (assume-false var alist)\
                    \(cons (cons var (f) )\
                          \alist) )",
             "(equal (tautology-checker x)\
                    \(tautologyp (normalize x)\
                                \(nil) ) )",
             "(equal (falsify x)\
                    \(falsify1 (normalize x)\
                              \(nil) ) )",
             "(equal (prime x)\
                    \(and (not (zerop x))\
                         \(not (equal x (add1 (zero) ) ) )\
                         \(prime1 x (1- x) ) ) )",
             "(equal (and p q)\
                    \(if p (if q (t) (f) ) (f) ) )",
             "(equal (or p q)\
                    \(if p (t) (if q (t) (f) ) ) )", -- book has extra (f)
             "(equal (not p)\
                    \(if p (f) (t) ) )",

             "(equal (implies p q)\
                    \(if p (if q (t) (f) ) (t) ) )",
             "(equal (fix x)\
                    \(if (numberp x) x (zero) ) )",
             "(equal (if (if a b c) d e)\
                    \(if a (if b d e) (if c d e) ) )",
             "(equal (zerop x)\
                    \(or (equal x (zero) )\
                        \(not (numberp x) ) ) )",
             "(equal (plus (plus x y) z )\
                    \(plus x (plus y z) ) )",
             "(equal (equal (plus a b) (zero ) )\
                    \(and (zerop a) (zerop b) ) )",
             "(equal (difference x x)\
                    \(zero) )",
             "(equal (equal (plus a b) (plus a c) )\
                    \(equal (fix b) (fix c) ) )",
             "(equal (equal (zero) (difference x y) )\
                    \(not (lessp y x) ) )",
             "(equal (equal x (difference x y) )\
                    \(and (numberp x)\
                         \(or (equal x (zero) )\
                             \(zerop y) ) ) )",

             "(equal (meaning (plus-tree (append x y) ) a)\
                    \(plus (meaning (plus-tree x) a)\
                          \(meaning (plus-tree y) a) ) )",
             "(equal (meaning (plus-tree (plus-fringe x) ) a)\
                    \(fix (meaning x a) ) )",
             "(equal (append (append x y) z)\
                    \(append x (append y z) ) )",
             "(equal (reverse (append a b) )\
                    \(append (reverse b) (reverse a) ) )",
             "(equal (times x (plus y z) )\
                    \(plus (times x y)\
                          \(times x z) ) )",
             "(equal (times (times x y) z)\
                    \(times x (times y z) ) )",
             "(equal (equal (times x y) (zero) )\
                    \(or (zerop x)\
                        \(zerop y) ) )",
             "(equal (exec (append x y)\
                           \pds envrn)\
                    \(exec y (exec x pds envrn)\
                          \envrn) )",
             "(equal (mc-flatten x y)\
                    \(append (flatten x)\
                           \y) )",
             "(equal (member x (append a b) )\
                    \(or (member x a)\
                        \(member x b) ) )",

             "(equal (member x (reverse y) )\
                    \(member x y) )",
             "(equal (length (reverse x) )\
                    \(length x) )",
             "(equal (member a (intersect b c) )\
                    \(and (member a b)\
                         \(member a c) ) )",
             "(equal (nth (zero)\
                          \i)\
                    \(zero) )",
             "(equal (exp i (plus j k) )\
                    \(times (exp i j)\
                           \(exp i k) ) )",
             "(equal (exp i (times j k) )\
                    \(exp (exp i j)\
                         \k) )",
             "(equal (reverse-loop x y)\
                    \(append (reverse x)\
                            \y) )",
             "(equal (reverse-loop x (nil) )\
                    \(reverse x) )",
             "(equal (count-list z (sort-lp x y) )\
                    \(plus (count-list z x)\
                          \(count-list z y) ) )",
             "(equal (equal (append a b)\
                           \(append a c) )\
                    \(equal b c) )",

             "(equal (plus (remainder x y)\
                          \(times y (quotient x y) ) )\
                    \(fix x) )",
             "(equal (power-eval (big-plus1 l i base)\
                                \base)\
                    \(plus (power-eval l base)\
                          \i) )",
             "(equal (power-eval (big-plus x y i base)\
                                \base)\
                    \(plus i (plus (power-eval x base)\
                                  \(power-eval y base) ) ) )",
             "(equal (remainder y 1)\
                    \(zero) )",
             "(equal (lessp (remainder x y)\
                           \y)\
                    \(not (zerop y) ) )",
             "(equal (remainder x x)\
                    \(zero) )",
             "(equal (lessp (quotient i j)\
                           \i)\
                    \(and (not (zerop i) )\
                         \(or (zerop j)\
                             \(not (equal j 1) ) ) ) )",
             "(equal (lessp (remainder x y)\
                           \x)\
                    \(and (not (zerop y) )\
                         \(not (zerop x) )\
                         \(not (lessp x y) ) ) )",
             "(equal (power-eval (power-rep i base)\
                                \base)\
                    \(fix i) )",
             "(equal (power-eval (big-plus (power-rep i base)\
                                          \(power-rep j base)\
                                          \(zero)\
                                          \base)\
                                 \base)\
                    \(plus i j) )",

             "(equal (gcd x y)\
                    \(gcd y x) )",
             "(equal (nth (append a b)\
                         \i)\
                    \(append (nth a i)\
                            \(nth b (difference i (length a) ) ) ) )",
             "(equal (difference (plus x y)\
                                \x)\
                    \(fix y) )",
             "(equal (difference (plus y x)\
                                \x)\
                    \(fix y) )",
             "(equal (difference (plus x y)\
                                \(plus x z) )\
                    \(difference y z) )",
             "(equal (times x (difference c w) )\
                    \(difference (times c x)\
                                \(times w x) ) )",
             "(equal (remainder (times x z)\
                               \z)\
                    \(zero) )",
             "(equal (difference (plus b (plus a c) )\
                                \a)\
                    \(plus b c) )",
             "(equal (difference (add1 (plus y z)\
                                \z)\
                    \(add1 y) )",
             "(equal (lessp (plus x y)\
                           \(plus x z ) )\
                    \(lessp y z) )",

             "(equal (lessp (times x z)\
                           \(times y z) )\
                    \(and (not (zerop z) )\
                         \(lessp x y) ) )",
             "(equal (lessp y (plus x y) )\
                    \(not (zerop x) ) )",
             "(equal (gcd (times x z)\
                         \(times y z) )\
                    \(times z (gcd x y) ) )",
             "(equal (value (normalize x)\
                           \a)\
                    \(value x a) )",
             "(equal (equal (flatten x)\
                           \(cons y (nil) ) )\
                    \(and (nlistp x)\
                         \(equal x y) ) )",
             "(equal (listp (gopher x) )\
                    \(listp x) )",
             "(equal (samefringe x y)\
                    \(equal (flatten x)\
                           \(flatten y) ) )",
             "(equal (equal (greatest-factor x y)\
                           \(zero) )\
                    \(and (or (zerop y)\
                             \(equal y 1) )\
                         \(equal x (zero) ) ) )",
             "(equal (equal (greatest-factor x y)\
                           \1)\
                    \(equal x 1) )",
             "(equal (numberp (greatest-factor x y) )\
                    \(not (and (or (zerop y)\
                                  \(equal y 1) )\
                              \(not (numberp x) ) ) ) )",

             "(equal (times-list (append x y) )\
                    \(times (times-list x)\
                           \(times-list y) ) )",
             "(equal (prime-list (append x y) )\
                    \(and (prime-list x)\
                         \(prime-list y) ) )",
             "(equal (equal z (times w z) )\
                    \(and (numberp z)\
                         \(or (equal z (zero) )\
                             \(equal w 1) ) ) )",
             "(equal (greatereqpr x y)\
                    \(not (lessp x y) ) )",
             "(equal (equal x (times x y) )\
                    \(or (equal x (zero) )\
                        \(and (numberp x)\
                             \(equal y 1) ) ) )",
             "(equal (remainder (times y x)\
                               \y)\
                    \(zero) )",
             "(equal (equal (times a b)\
                           \1)\
                    \(and (not (equal a (zero) ) )\
                         \(not (equal b (zero) ) )\
                         \(numberp a)\
                         \(numberp b)\
                         \(equal (1- a)\
                                \(zero) )\
                         \(equal (1- b)\
                                \(zero) ) ) )",
             "(equal (lessp (length (delete x l) )\
                           \(length l) )\
                    \(member x l) )",
             "(equal (sort2 (delete x l) )\
                    \(delete x (sort2 l) ) )",
             "(equal (dsort x)\
                    \(sort2 x) )",

             "(equal (length\
                     \(cons \
                      \x1\
                      \(cons \
                       \x2\
                       \(cons \
                        \x3\
                        \(cons \
                         \x4\
                         \(cons \
                          \x5\
                          \(cons x6 x7) ) ) ) ) ) )\
                    \(plus 6 (length x7) ) )",
             "(equal (difference (add1 (add1 x) )\
                                \2)\
                    \(fix x) )",
             "(equal (quotient (plus x (plus x y) )\
                              \2)\
                    \(plus x (quotient y 2) ) )",
             "(equal (sigma (zero)\
                           \i)\
                    \(quotient (times i (add1 i) )\
                              \2) )",
             "(equal (plus x (add1 y) )\
                    \(if (numberp y)\
                        \(add1 (plus x y) )\
                        \(add1 x) ) )",
             "(equal (equal (difference x y)\
                           \(difference z y) )\
                    \(if (lessp x y)\
                        \(not (lessp y z) )\
                        \(if (lessp z y)\
                            \(not (lessp y x) )\
                            \(equal (fix x)\
                                   \(fix z) ) ) ) )",
             "(equal (meaning (plus-tree (delete x y) )\
                             \a)\
                    \(if (member x y)\
                        \(difference (meaning (plus-tree y)\
                                             \a)\
                                    \(meaning x a) )\
                        \(meaning (plus-tree y)\
                                 \a) ) )",
             "(equal (times x (add1 y) )\
                    \(if (numberp y)\
                        \(plus x (times x y) )\
                        \(fix x) ) )",
             "(equal (nth (nil)\
                         \i)\
                    \(if (zerop i)\
                        \(nil)\
                        \(zero) ) )",
             "(equal (last (append a b) )\
                    \(if (listp b)\
                        \(last b)\
                        \(if (listp a)\
                            \(cons (car (last a) )\
                                 \b)\
                            \b) ) )",

             "(equal (equal (lessp x y)\
                           \z)\
                    \(if (lessp x y)\
                        \(equal t z)\
                        \(equal f z) ) )",
             "(equal (assignment x (append a b) )\
                    \(if (assignedp x a)\
                        \(assignment x a)\
                        \(assignment x b) ) )",
             "(equal (car (gopher x) )\
                    \(if (listp x)\
                       \(car (flatten x) )\
                        \(zero) ) )",
             "(equal (flatten (cdr (gopher x) ) )\
                    \(if (listp x)\
                        \(cdr (flatten x) )\
                        \(cons (zero)\
                              \(nil) ) ) )",
             "(equal (quotient (times y x)\
                              \y)\
                    \(if (zerop y)\
                        \(zero)\
                        \(fix x) ) )",
             "(equal (get j (set i val mem) )\
                    \(if (eqp j i)\
                        \val\
                        \(get j mem) ) )"]

