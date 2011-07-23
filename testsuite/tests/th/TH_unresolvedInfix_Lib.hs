module TH_unresolvedInfix_Lib where

import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Quote

infixl 6 :+
infixl 7 :*

data Tree = N
  | Tree :+ Tree 
  | Tree :* Tree 

-- custom instance, including redundant parentheses
instance Show Tree where
  show N = "N"
  show (a :+ b) = "(" ++ show a ++ " :+ " ++ show b ++ ")"
  show (a :* b) = "(" ++ show a ++ " :* " ++ show b ++ ")"

-- VarE versions
infixl 6 +:
infixl 7 *:
(+:) = (:+)
(*:) = (:*)

n = conE (mkName "N")
plus = conE (mkName ":+")
times = conE (mkName ":*")

a +? b = uInfixE a plus b
a *? b = uInfixE a times b
a +! b = infixApp a plus b
a *! b = infixApp a times b

plus2 = varE (mkName "+:")
times2 = varE (mkName "*:")
plus3 = conE ('(:+))


--------------------------------------------------------------------------------
--                                  Patterns                                  --
--------------------------------------------------------------------------------
-- The only way to test pattern splices is using QuasiQuotation
mkQQ pat = QuasiQuoter undefined (const pat) undefined undefined
p = conP (mkName "N") []
plus' = mkName ":+"
times' = mkName ":*"

a ^+? b = uInfixP a plus' b
a ^*? b = uInfixP a times' b
a ^+! b = infixP a plus' b
a ^*! b = infixP a times' b

-------------- Completely-unresolved patterns
p1 = mkQQ ( p ^+? (p ^*? p) )
p2 = mkQQ ( (p ^+? p) ^*? p )
p3 = mkQQ ( p ^+? (p ^+? p) )
p4 = mkQQ ( (p ^+? p) ^+? p )
-------------- Completely-resolved patterns
p5 = mkQQ ( p ^+! (p ^*! p) )
p6 = mkQQ ( (p ^+! p) ^*! p )
p7 = mkQQ ( p ^+! (p ^+! p) )
p8 = mkQQ ( (p ^+! p) ^+! p )
-------------- Mixed resolved/unresolved
p9 = mkQQ ( (p ^+! p) ^*? (p ^+? p) )
p10 = mkQQ ( (p ^+? p) ^*? (p ^+! p) )
p11 = mkQQ ( (p ^+? p) ^*! (p ^+! p) )
p12 = mkQQ ( (p ^+? p) ^*! (p ^+? p) )
-------------- Parens
p13 = mkQQ ( ((parensP ((p ^+? p) ^*? p)) ^+? p) ^*? p )
p14 = mkQQ ( (parensP (p ^+? p)) ^*? (parensP (p ^+? p)) )
p15 = mkQQ ( parensP ((p ^+? p) ^*? (p ^+? p)) )
-------------- Dropping constructors
p16 = mkQQ ( p ^*? (tupP [p ^+? p]) )
