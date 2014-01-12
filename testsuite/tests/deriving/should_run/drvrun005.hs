module Main where

{-
  If a fixity declaration hasn't been supplied for
  an operator, it is defaulted to being "infixl 9".

  OLD:   The derived Read instances for data types containing
	 left-assoc constructors produces code that causes
	 non-termination if you use 'read' to evaluate them
	 ( (head (reads x)) is cool tho.)

	  ==> The inferred assoc for :++ below left & the derived
	  Read instance should fail to terminate (with ghc-4.xx,
	  this is exemplified by having the stack overflow.)

  NEW: the new H98 spec says that we ignore associativity when
       parsing, so it terminates fine
-}
-- infixl 9 :++
data T = T1 |  T :++ T  deriving (Eq,Show, Read)

t :: T
t = read "T1"

main = do
  print ((fst (head (reads "T1"))) :: T)
  print t
