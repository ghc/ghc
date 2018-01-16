
-- ==========================================================--
-- === Read the lattice table.               ReadTable.hs ===--
-- ==========================================================--

module ReadTable where
import BaseDefs
import Utils
import MyUtils
import Parser2

import Data.Char(isDigit) -- 1.3

-- ==========================================================--
--
rtReadTable :: String -> [(Domain, Int)]

rtReadTable s
   = case rtTable (rtLex 1 s) of
        PFail [] 
           -> myFail "Unexpected end of lattice table"
        PFail ((n,t):_) 
           -> myFail ("Syntax error in lattice table, line " ++ show n ++ ".")
        POk tab [] 
           -> tab
        POk tab ((n,t):_) 
           -> myFail ("Syntax error in lattice table, line " ++ show n ++ ".")


-- ==========================================================--
--
rtLex :: Int -> String -> [Token]

rtLex n [] = []

rtLex n ('\n':cs) = rtLex (n+1) cs
rtLex n (' ':cs) = rtLex n cs
rtLex n ('\t':cs) = rtLex n cs

rtLex n ('(':cs) = (n, "("):rtLex n cs
rtLex n (')':cs) = (n, ")"):rtLex n cs
rtLex n ('[':cs) = (n, "["):rtLex n cs
rtLex n (']':cs) = (n, "]"):rtLex n cs
rtLex n (',':cs) = (n, ","):rtLex n cs

rtLex n ('T':'w':'o':cs)          = (n, "T"):rtLex n cs
rtLex n ('F':'u':'n':'c':cs)      = (n, "F"):rtLex n cs
rtLex n ('L':'i':'f':'t':'1':cs)  = (n, "L"):rtLex n cs
rtLex n ('L':'i':'f':'t':'2':cs)  = (n, "M"):rtLex n cs

rtLex n (c:cs)
   | isDigit c 
   = (n, c:takeWhile isDigit cs):rtLex n (dropWhile isDigit cs)
   | otherwise 
   = myFail ("Illegal character " ++ show c ++
           " in lattice table, line " ++ show n ++ "." )


-- ==========================================================--
--
rtPWithComma p = paThen2 (\a b -> a) p (paLit ",")

-- ==========================================================--
--
rtListMain p
  = paAlts 
    [ ( (=="]"), 
        paApply (paLit "]") (const []) ),

      ( const True, 
        paThen3 (\a b c -> a ++ [b]) 
               (paZeroOrMore (rtPWithComma p)) p (paLit "]") ) ]

-- ==========================================================--
--
rtList p = paThen2 (\a b -> b) (paLit "[") (rtListMain p)

-- ==========================================================--
--
rtListDomain = rtList rtDomain

-- ==========================================================--
--
rtDomain
  = paAlts
    [
       ( (=="("), paThen3 (\a b c -> b) (paLit "(") rtDomain (paLit ")") ),
       ( (=="T"), paApply (paLit "T") (const Two) ),
       ( (=="L"), paThen2 (\a b -> Lift1 b) (paLit "L") rtListDomain ),
       ( (=="M"), paThen2 (\a b -> Lift2 b) (paLit "M") rtListDomain ),
       ( (=="F"), paThen3 (\a b c -> Func b c)
                          (paLit "F") rtListDomain rtDomain )
    ]

-- ==========================================================--
--
rtPair pa pb
   = paThen4 (\a b c d -> (b,d)) (paLit "(") pa (paLit ",") (
     paThen2 (\a b -> a)         pb (paLit ")") )

-- ==========================================================--
--
rtTable 
  = rtList (rtPair rtDomain paNum)


-- ==========================================================--
-- === end                                   ReadTable.hs ===--
-- ==========================================================--
