{-
Copyright(C) 1999 Erik Meijer and Arjan van Yzendoorn

Determines wether an express/declaration is "simple".
The pretty-printing strategy is to print a "complex" expression
on a new line.
-}

module Utils where

import Mondrian
 
isSimpleExpr :: Expr -> Bool
isSimpleExpr = \e ->
  case e of
    { Lit l -> True
    ; Var n -> True
    ; Case e as -> and [ isSimpleArms as, isSimpleExpr e ]
    ; Let ds e -> and [ isSimpleDecls ds, isSimpleExpr e ]
    ; Lambda n e -> isSimpleExpr e
    ; New n ds -> all isSimpleDecl ds
    ; App f a -> and [ isSimpleExpr f, isSimpleExpr a]
    ; Chain e oes -> and [ isSimpleExpr e, all isSimpleExpr [ e | (o,e) <- oes ] ]
    }

isSimpleArms = \as ->
  and [ length as == 1, all isSimpleExpr [ e | (p,e) <- as ], all isSimplePattern [ p | (p,e) <- as ] ]

isSimplePattern = \ p->
  case p of
    { Pattern n ds -> isSimpleDecls ds
    ; Default -> True
    }

isSimpleDecls = \ds ->
  and [ all isSimpleDecl ds ]
  
isSimpleDecl = \d ->
  case d of
    { ClassDecl n ns ds -> False
    ; ImportDecl n -> True
    ; VarDecl n e -> isSimpleExpr e
    ; SigDecl n e -> True
    }

groupLambdas :: Expr -> Expr
groupLambdas = \e ->
  case e of
    { Lambda ns (Lambda ms e) -> groupLambdas (Lambda (ns++ms) e)
    ; otherwise -> e
    }

isTopLevel :: [Name] -> Name -> Bool
isTopLevel = \topLevel -> \n ->
  n `elem` topLevel
  
topLevel :: CompilationUnit -> [Name]
topLevel = \p ->
  case p of 
    { Package n ds -> [ n | VarDecl n e <- ds ]
    }
