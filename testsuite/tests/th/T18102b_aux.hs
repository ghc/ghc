{-# LANGUAGE RebindableSyntax, TemplateHaskell, OverloadedLists #-}
module T18102b_aux where

import Prelude hiding ((>>=), return )
import Language.Haskell.TH.Syntax



ifThenElse :: Bool -> Int -> Int -> Int
ifThenElse _ a b = a+b

intQuote_TTH :: Code Q Int
intQuote_TTH = [|| if True then 10 else 15 ||]

intQuote_TH :: Quote m => m Exp
intQuote_TH = [| if True then 10 else 15 |]

t1 :: Int
t1 = if True then 10 else 15


(>>=) :: a -> ((forall b . b) -> c) -> c
a >>= f = f undefined
return _ = 'b'
fail s = undefined

t2 :: Char
t2 = do { return 'k' }

charQuote_TTH :: Code Q Char
charQuote_TTH = [|| do { return 'k' } ||]

charQuote_TH :: Quote m => m Exp
charQuote_TH = [| do { return 'k' } |]

fromListN :: Int -> [Int] -> [Int]
fromListN _ l = replicate (length l) (length l)

fromList  :: [Int] -> [Int]
fromList x = replicate (length x) (length x)


t3 :: [Int]
t3 = [2..7]

seqQuote_TTH :: Code Q [Int]
seqQuote_TTH = [|| [2..7] ||]

seqQuote_TH :: Quote m => m Exp
seqQuote_TH = [| [2..7] |]


t4 :: [Int]
t4 = [1,2,3]

listQuote_TTH :: Code Q [Int]
listQuote_TTH = [|| [1,2,3] ||]

listQuote_TH :: Quote m => m Exp
listQuote_TH = [| [1,2,3] |]
