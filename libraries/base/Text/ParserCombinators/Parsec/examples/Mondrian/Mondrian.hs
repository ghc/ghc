{-
Abstract Syntax for Core Mondrian
(c) 1999 Erik Meijer and Arjan van Yzendoorn
-}

module Mondrian where 

data CompilationUnit
  = Package Name [Decl]
    deriving Show

data Decl
 = ClassDecl Name [Name] [Decl]    
 | ImportDecl Name
 | VarDecl Name Expr
 | SigDecl Name Expr
   deriving Show

data Expr
  = Lit Lit
  | Var Name
  | Case Expr [(Pattern, Expr)]
  | Let [Decl] Expr
  | Lambda [Name] Expr
  | App Expr Expr
  | New Name [Decl]
  | Chain Expr [(Name, Expr)] 
    deriving Show
      
data Pattern 
  = Pattern Name [Decl]
  | Default
    deriving Show
  
data Lit
  = IntLit Integer
  | CharLit Char
  | StringLit String
    deriving Show
    
type Name = [String]
