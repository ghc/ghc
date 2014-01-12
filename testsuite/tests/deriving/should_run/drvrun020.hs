-- A  nasty deriving test
-- Note the "T2 T1 { f1=3 }" part!

module Main where


infix 4 :%%
data T = Int :%% T
 	 | T1 { f1 :: Int }
	 | T2 T
	deriving( Show, Read )

main = print (read "3 :%% T2 T1 { f1=3 }" :: T)

{- Here's the parser that is produced 

import GHC.Read
import Text.ParserCombinators.ReadPrec
import Text.Read

instance Read T where
  readPrec =
    parens
    ( prec 4 (
        do x           <- step readPrec
           Symbol ":%%" <- lexP
           y           <- step readPrec
           return (x :%% y))
      +++
      prec (appPrec+1) (
	do Ident "T1" <- lexP
	   Punc "{" <- lexP
	   Ident "f1" <- lexP
	   Punc "=" <- lexP
	   x	      <- reset readPrec
	   Punc "}" <- lexP
	   return (T1 { f1 = x }))
      +++
      prec appPrec (
        do Ident "T2" <- lexP
           x          <- step readPrec
           return (T2 x))
    )

appPrec = 10::Int
-}
