{-# LANGUAGE QuasiQuotes #-}

module Main where

import TH_unresolvedInfix_Lib
import Language.Haskell.TH

--------------------------------------------------------------------------------
--                                Expressions                                 --
--------------------------------------------------------------------------------
exprs = [
-------------- Completely-unresolved bindings
  $( n +? (n *? n) ),
  $( (n +? n) *? n ),
  $( n +? (n +? n) ),
  $( (n +? n) +? n ),
  -- VarE version
  $( uInfixE n plus2 (uInfixE n plus2 n) ),
  $( uInfixE (uInfixE n plus2 n) plus2 n ),
  $( uInfixE n plus3 (uInfixE n plus3 n) ),
  $( uInfixE (uInfixE n plus3 n) plus3 n ),

--------------- Completely-resolved bindings
  $( n +! (n *! n) ),
  $( (n +! n) *! n ),
  $( n +! (n +! n) ),
  $( (n +! n) +! n ),

-------------- Mixed resolved/unresolved
  $( (n +! n) *? (n +? n) ),
  $( (n +? n) *? (n +! n) ),
  $( (n +? n) *! (n +! n) ),
  $( (n +? n) *! (n +? n) ),

-------------- Parens
  $( ((parensE ((n +? n) *? n)) +? n) *? n ),
  $( (parensE (n +? n)) *? (parensE (n +? n)) ),
  $( parensE ((n +? n) *? (n +? n)) ),

-------------- Sections
  $( infixE (Just $ n +? n) plus Nothing ) N,
  -- see B.hs for the (non-compiling) other version of the above
  $( infixE Nothing plus (Just $ parensE $ uInfixE n plus n) ) N,

-------------- Dropping constructors
  $( n *? tupE [n +? n] )
  ]

--------------------------------------------------------------------------------
--                                  Patterns                                  --
--------------------------------------------------------------------------------
patterns = [
-------------- Completely-unresolved patterns
  case N :+ (N :* N) of
    [p1|unused|] -> True,
  case N :+ (N :* N) of
    [p2|unused|] -> True,
  case (N :+ N) :+ N of
    [p3|unused|] -> True,
  case (N :+ N) :+ N of
    [p4|unused|] -> True,
-------------- Completely-resolved patterns
  case N :+ (N :* N) of
    [p5|unused|] -> True,
  case (N :+ N) :* N of
    [p6|unused|] -> True,
  case N :+ (N :+ N) of
    [p7|unused|] -> True,
  case (N :+ N) :+ N of
    [p8|unused|] -> True,
-------------- Mixed resolved/unresolved
  case ((N :+ N) :* N) :+ N of
    [p9|unused|] -> True,
  case N :+ (N :* (N :+ N)) of
    [p10|unused|] -> True,
  case (N :+ N) :* (N :+ N) of
    [p11|unused|] -> True,
  case (N :+ N) :* (N :+ N) of
    [p12|unused|] -> True,
-------------- Parens
  case (N :+ (N :* N)) :+ (N :* N) of
    [p13|unused|] -> True,
  case (N :+ N) :* (N :+ N) of
    [p14|unused|] -> True,
  case (N :+ (N :* N)) :+ N of
    [p15|unused|] -> True,
-------------- Dropping constructors
  case (N :* (N :+ N)) of
    [p16|unused|] -> True
 ]

main = do
  mapM_ print exprs
  mapM_ print patterns
  -- check that there are no Parens or UInfixes in the output
  runQ [|N :* N :+ N|] >>= print
  runQ [|(N :* N) :+ N|] >>= print
  runQ [p|N :* N :+ N|] >>= print
  runQ [p|(N :* N) :+ N|] >>= print

  -- pretty-printing of unresolved infix expressions
  let ne = ConE $ mkName "N"
      np = ConP (mkName "N") []
      plusE = ConE (mkName ":+")
      plusP = (mkName ":+")
  putStrLn $ pprint (InfixE (Just ne) plusE (Just $ UInfixE ne plusE (UInfixE ne plusE ne)))
  putStrLn $ pprint (ParensE ne)
  putStrLn $ pprint (InfixP np plusP (UInfixP np plusP (UInfixP np plusP np)))
  putStrLn $ pprint (ParensP np)
