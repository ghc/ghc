----------------------------------------------------------------
-- the Henk Abstract Syntax
-- Copyright 2000, Jan-Willem Roorda and Daan Leijen
----------------------------------------------------------------
module HenkAS where

import Pretty

----------------------------------------------------------------
-- Abstract Syntax 
----------------------------------------------------------------
data Program        = Program [TypeDecl] [ValueDecl]
                    
data TypeDecl       = Data Var [Var]
                    
data ValueDecl      = Let Bind
                    | LetRec [Bind]
                    
data Bind           = Bind Var Expr
                    
data Expr           = Var Var
                    | Lit Lit
                    | Box
                    | Star
                    | Unknown
                    
                    | App Expr Expr          
                    | Case Expr [Alt] [Expr]
                    | In ValueDecl Expr
                    | Pi Var Expr
                    | Lam Var Expr
                    
data Alt            = Alt Pat Expr

data Pat            = PatVar Var
                    | PatLit Lit

data Var            = TVar Identifier Expr

data Lit            = LitInt Integer

type Identifier     = String    

anonymous           = "_"
isAnonymous s       = (null s || (head s == head anonymous))


----------------------------------------------------------------
-- pretty print abstract syntax
----------------------------------------------------------------
instance Show Program where
  showsPrec d program   = shows (pprogram program)

vsep ds
    = vcat (map ($$ text "") ds)    


-- program
pprogram (Program tdecls vdecls)
    = vsep ((map ptdecl tdecls) ++ (map pvdecl vdecls))
    
ptdecl (Data v vs)
    = (text "data" <+> pbindvar v)
      $$ indent (text "=" <+> braced (map ptvar vs))
  
    
pvdecl vdecl
    = case vdecl of
        Let bind     -> text "let" <+> pbind bind
        LetRec binds -> text "letrec" $$ indent (braced (map pbind binds))
  
pbind (Bind v e)
    = pbindvar v $$ indent (text "=" <+> pexpr e)
  
-- expressions (are parenthesis correct ?)  
parensExpr e
    = case e of
        In _ _      -> parens (pexpr e)
        Pi _ _      -> parens (pexpr e)
        Lam _ _     -> parens (pexpr e)
        Case _ _ _  -> parens (pexpr e)
        App _ _     -> parens (pexpr e)
        Var (TVar i t) -> case t of
                            Unknown -> pexpr e
                            other   -> parens (pexpr e)
        other       -> pexpr e
  
pexpr e
    = case e of
        Var v       -> pboundvar v
        Lit l       -> plit l
        Box         -> text "[]"
        Star        -> text "*"
        Unknown     -> text "?"
                        
        App e1 e2   -> pexpr e1 <+> parensExpr e2
        Case e as ts-> sep $ [text "case" <+> parensExpr e <+> text "of"
                             ,nest 3 (braced (map palt as))
                             ] ++
                             (if (null as) 
                               then []
                               else [text "at"
                                    ,nest 3 (braced (map pexpr ts))
                                    ])
                       
        In v e      -> sep[ pvdecl v, text "in" <+> pexpr e]        
        Pi v e      -> case v of
                         TVar i t    | isAnonymous i -> parensExpr t <+> text "->" <+> pexpr e
                         TVar i Star -> sep[ text "\\/" <> text i <> text ".", pexpr e]
                         other       -> sep[ text "|~|" <> pbindvar v <> text ".", pexpr e]
        Lam v e     -> case v of
                         TVar i Star -> sep[ text "/\\" <> text i <> text ".", pexpr e]
                         other       -> sep[ text "\\" <> pbindvar v <> text ".", pexpr e]
  
  
-- atomic stuff  
palt (Alt p e)
    = ppat p <+> text "=>" <+> pexpr e
    
ppat p
    = case p of PatVar v -> pboundvar v
                PatLit l -> plit l
            
                  
pboundvar v@(TVar i e)
    = case e of Unknown  -> text i
                other    -> ptvar v
  
pbindvar v@(TVar i e)
    = case e of Star     -> text i
                other    -> ptvar v
                
ptvar (TVar i e)
    = text i <> colon <+> pexpr e
            
             
plit l
    = case l of LitInt i -> integer i
    
braced []
    = empty
    
braced ds
    = let prefix = map text $ ["{"] ++ repeat ";"
      in  cat ((zipWith (<+>) prefix ds) ++ [text "}"])
      
indent
    = nest 4
    
  
    