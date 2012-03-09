-- Test instances for tuples up to 15
-- For Read, Show, Eq, Ord, Bounded

module Main where

data T = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O
       deriving( Eq, Ord, Show, Read, Bounded )

t15 = (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O)
t14 = (A,B,C,D,E,F,G,H,I,J,K,L,M,N)
t13 = (A,B,C,D,E,F,G,H,I,J,K,L,M)
t12 = (A,B,C,D,E,F,G,H,I,J,K,L)
t11 = (A,B,C,D,E,F,G,H,I,J,K)
t10 = (A,B,C,D,E,F,G,H,I,J)
t9  = (A,B,C,D,E,F,G,H,I)
t8  = (A,B,C,D,E,F,G,H)
t7  = (A,B,C,D,E,F,G)
t6  = (A,B,C,D,E,F)
t5  = (A,B,C,D,E)
t4  = (A,B,C,D)
t3  = (A,B,C)
t2  = (A,B)
t0  = ()

big = (t0,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15)

main = do print big
	  print (read (show big) `asTypeOf` big)
	  print (big == big)
	  print (big < big)
	  print (big > big)
	  print (minBound `asTypeOf` big)
	  print (maxBound `asTypeOf` big)