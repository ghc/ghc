{-
Copyright(C) 1999 Erik Meijer and Arjan van Yzendoorn
-}
module SimpleMondrianPrinter where

import Mondrian
import Pretty
import Utils

mondrianIndent :: Int
mondrianIndent = 2

compilationUnit :: CompilationUnit -> Doc
compilationUnit = \m ->
  case m of 
    { Package n ds -> package m (name n) (decls ds) 
    }

package = \(Package n' ds') -> \n -> \ds -> 
  case null ds' of
    { True -> text "package" <+> n <+> row ds
    ; False -> text "package" <+> n <-> nest (-mondrianIndent) (column ds)
    }

decls = \ds -> [ decl d | d <- ds ]

decl = \d ->
  case d of
    { ImportDecl ns -> importDecl d (name ns)
    ; ClassDecl n xs ds -> classDecl d (name n) (extends xs) (decls ds)
    ; SigDecl n t -> sigDecl (name n) (expr t)
    ; VarDecl v (Lambda ns e) -> varDecl d (name v) (lambdas ns) (expr e)
    ; VarDecl v e -> decl (VarDecl v (Lambda [] e))
    }

extends = \xs ->
  case xs of 
    { [] -> empty
    ; [x] -> text "extends" <+> name x <+> empty
    ; xs -> text "multiple inheritance not supported" <+> row [name x | x <- xs]
    } 
    
classDecl = \(ClassDecl n' xs' ds') -> \n -> \xs -> \ds -> 
  case ds' of
    { [] -> text "class" <+> n <+> xs
    ; otherwise -> text "class" <+> n <+> xs <-> column ds
    }

sigDecl = \n -> \t -> n <+> text "::" <+> t
    
importDecl = \d -> \n -> text "import" <+> n

varDecl = \(VarDecl v' (Lambda ns' e')) -> \v -> \ns -> \e ->
  if isSimpleExpr e'
  then v <+> text "=" <+> ns <|> e
  else v <+> text "=" <+> ns <-> nest mondrianIndent e

names = \ns -> horizontal (text " ") [ name n | n <- ns ]       
                 
name = \ns -> horizontal (text ".") [text n | n <- ns]
  
lambdas = \ns ->
  case ns of 
    { []   -> empty
    ; [n]  -> text "\\" <|> name n <+> text "->" <+> empty
    ; n:ns -> text "\\" <|> name n <+> text "->" <+> lambdas ns
    }

expr = \e ->
  case e of
    { Lit l -> lit l
    ; Var n -> name n
    ; App f a -> application (expr f) (expr a)
    ; Lambda ns b -> lambdaExpr e (lambdas ns) (expr b)
    ; New n ds -> newExpr e (name n) (decls ds)
    ; Case e1 as -> caseExpr e (expr e1) (arms as)
    ; Let ds e1 -> letExpr e (decls ds) (expr e1)                                            
    ; Chain e1 oes -> chain e1 oes
    }
   
application = \f -> \a -> text "(" <|> f <+> a <|> text ")"

newExpr = \(New n' ds') -> \n -> \ds ->
  case ds' of
    { [] -> text "new" <+> n
    ; otherwise -> 
        if isSimpleDecls ds'
        then text "new" <+> n <+> row ds
        else text "new" <+> n <-> column ds
    }
    
lambdaExpr = \(Lambda ns' e') -> \ns -> \e ->
  if isSimpleExpr e'
  then ns <|> e
  else ns <-> nest mondrianIndent e

caseExpr :: Expr -> Doc -> [Doc] -> Doc
caseExpr = \(Case e' as') -> \e -> \as ->
  case (isSimpleExpr e', isSimpleArms as') of
    { (True, True) -> text "case" <+> e <+> text "of" <+> row as
    ; (True, False)-> text "case" <+> e <+> text "of" <-> column as
    ; (False, True) -> text "case" <-> nest mondrianIndent e <-> text "of" <+> row as
    ; (False, False) -> text "case" <-> nest mondrianIndent e <-> text "of" <-> column as
    }
  
letExpr = \(Let ds' e') -> \ds -> \e ->
  case (length ds' == 1 && isSimpleDecls ds', isSimpleExpr e') of
    { (True, True) -> text "let" <+> row ds <+> text "in" <+> e
    ; (True, False) -> text "let" <+> row ds <-> text "in" <-> nest mondrianIndent e
    ; (False, True) -> text "let" <-> column ds <-> text "in" <+> e
    ; (False, False) -> text "let" <-> column ds <-> text "in" <-> nest mondrianIndent e
    }

arms = \as -> [ arm (p,e) (pattern p) (expr e) | (p,e) <- as ]
  
arm = \(p',e') -> \p -> \e ->
  if isSimplePattern p' && isSimpleExpr e'
  then p <+> text "->" <+> e
  else p <+> text "->" <-> nest mondrianIndent e
    
-- This is a dirty hack!

chain = \e -> \oes ->
  case oes of
    { []        -> bracket e
    ; ([""],f):oes -> if (isSimpleExpr f)
                   then (bracket e) <+> chain f oes
                   else (bracket e) <-> nest 2 (chain f oes)
    ; (o,f):oes -> if (isSimpleExpr f)
                   then (bracket e) <+> name o <+> chain f oes
                   else (bracket e) <-> name o <+> chain f oes           
    }

pattern = \p ->
  case p of
    { Pattern n ds -> 
        case ds of
          { [] -> name n
          ; otherwise -> name n <+> row (decls ds)
          }
    ; Default -> text "default"
    }
    
lit = \l ->
  case l of
    { IntLit i    -> text (show i)
    ; CharLit c   -> text (show c)
    ; StringLit s -> text (show s)
    }

bracket = \e ->
  case e of
    { Lit l -> expr e
    ; Var n -> expr e
    ; e     -> par (expr e)
    }

par = \e -> text "(" <|> e <|> text ")"

column = \ds -> nest mondrianIndent (block (text "{ ", text ";" <+> empty, text "}") ds)

row = \ds -> text "{" <|> horizontal (text ";" <+> empty) ds <|> text "}"