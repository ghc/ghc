-- TH.Ppr contains a prettyprinter for the
-- Template Haskell datatypes

module Language.Haskell.TH.Ppr where
    -- All of the exports from this module should
    -- be "public" functions.  The main module TH
    -- re-exports them all.

import Text.PrettyPrint.HughesPJ (render)
import Language.Haskell.TH.PprLib
import Language.Haskell.TH.Syntax
import Data.Char ( toLower )

nestDepth :: Int
nestDepth = 4

type Precedence = Int
appPrec, opPrec, noPrec :: Precedence
appPrec = 2    -- Argument of a function application
opPrec  = 1    -- Argument of an infix operator
noPrec  = 0    -- Others

parensIf :: Bool -> Doc -> Doc
parensIf True d = parens d
parensIf False d = d

------------------------------

pprint :: Ppr a => a -> String
pprint x = render $ to_HPJ_Doc $ ppr x

class Ppr a where
    ppr :: a -> Doc
    ppr_list :: [a] -> Doc
    ppr_list = vcat . map ppr

instance Ppr a => Ppr [a] where
    ppr x = ppr_list x

------------------------------
instance Ppr Name where
    ppr v = pprName v -- text (show v)

------------------------------
instance Ppr Info where
    ppr (ClassI d) = ppr d
    ppr (TyConI d) = ppr d
    ppr (PrimTyConI name arity is_unlifted) 
      = text "Primitive"
	<+> (if is_unlifted then text "unlifted" else empty)
	<+> text "type construtor" <+> quotes (ppr name)
	<+> parens (text "arity" <+> int arity)
    ppr (ClassOpI v ty cls fix) 
      = text "Class op from" <+> ppr cls <> colon <+>
        vcat [ppr_sig v ty, pprFixity v fix]
    ppr (DataConI v ty tc fix) 
      = text "Constructor from" <+> ppr tc <> colon <+>
        vcat [ppr_sig v ty, pprFixity v fix]
    ppr (TyVarI v ty)
      = text "Type variable" <+> ppr v <+> equals <+> ppr ty
    ppr (VarI v ty mb_d fix) 
      = vcat [ppr_sig v ty, pprFixity v fix, 
              case mb_d of { Nothing -> empty; Just d -> ppr d }]

ppr_sig v ty = ppr v <+> text "::" <+> ppr ty

pprFixity :: Name -> Fixity -> Doc
pprFixity v f | f == defaultFixity = empty
pprFixity v (Fixity i d) = ppr_fix d <+> int i <+> ppr v
    where ppr_fix InfixR = text "infixr"
          ppr_fix InfixL = text "infixl"
          ppr_fix InfixN = text "infix"


------------------------------
instance Ppr Exp where
    ppr = pprExp noPrec

pprExp :: Precedence -> Exp -> Doc
pprExp _ (VarE v)     = ppr v
pprExp _ (ConE c)     = ppr c
pprExp i (LitE l)     = pprLit i l
pprExp i (AppE e1 e2) = parensIf (i >= appPrec) $ pprExp opPrec e1
                                              <+> pprExp appPrec e2
pprExp i (InfixE (Just e1) op (Just e2))
 = parensIf (i >= opPrec) $ pprExp opPrec e1
                        <+> ppr op
                        <+> pprExp opPrec e2
pprExp _ (InfixE me1 op me2) = parens $ pprMaybeExp noPrec me1
                                    <+> ppr op
                                    <+> pprMaybeExp noPrec me2
pprExp i (LamE ps e) = parensIf (i > noPrec) $ char '\\' <> hsep (map ppr ps)
                                           <+> text "->" <+> ppr e
pprExp _ (TupE es) = parens $ sep $ punctuate comma $ map ppr es
-- Nesting in Cond is to avoid potential problems in do statments
pprExp i (CondE guard true false)
 = parensIf (i > noPrec) $ sep [text "if"   <+> ppr guard,
                       nest 1 $ text "then" <+> ppr true,
                       nest 1 $ text "else" <+> ppr false]
pprExp i (LetE ds e) = parensIf (i > noPrec) $ text "let" <+> ppr ds
                                            $$ text " in" <+> ppr e
pprExp i (CaseE e ms)
 = parensIf (i > noPrec) $ text "case" <+> ppr e <+> text "of"
                        $$ nest nestDepth (ppr ms)
pprExp i (DoE ss) = parensIf (i > noPrec) $ text "do" <+> ppr ss
pprExp _ (CompE []) = error "Can't happen: pprExp (CompExp [])"
-- This will probably break with fixity declarations - would need a ';'
pprExp _ (CompE ss) = text "[" <> ppr s
                  <+> text "|"
                  <+> (sep $ punctuate comma $ map ppr ss')
                   <> text "]"
    where s = last ss
          ss' = init ss
pprExp _ (ArithSeqE d) = ppr d
pprExp _ (ListE es) = brackets $ sep $ punctuate comma $ map ppr es
pprExp i (SigE e t) = parensIf (i > noPrec) $ ppr e <+> text "::" <+> ppr t
pprExp _ (RecConE nm fs) = ppr nm <> braces (pprFields fs)
pprExp _ (RecUpdE e fs) = pprExp appPrec e <> braces (pprFields fs)

pprFields :: [(Name,Exp)] -> Doc
pprFields = sep . punctuate comma . map (\(s,e) -> ppr s <+> equals <+> ppr e)

pprMaybeExp :: Precedence -> Maybe Exp -> Doc
pprMaybeExp _ Nothing = empty
pprMaybeExp i (Just e) = pprExp i e

------------------------------
instance Ppr Stmt where
    ppr (BindS p e) = ppr p <+> text "<-" <+> ppr e
    ppr (LetS ds) = text "let" <+> ppr ds
    ppr (NoBindS e) = ppr e
    ppr (ParS sss) = sep $ punctuate (text "|")
                         $ map (sep . punctuate comma . map ppr) sss

------------------------------
instance Ppr Match where
    ppr (Match p rhs ds) = ppr p <+> pprBody False rhs
                        $$ where_clause ds

------------------------------
pprBody :: Bool -> Body -> Doc
pprBody eq (GuardedB xs) = nest nestDepth $ vcat $ map do_guard xs
  where eqd = if eq then text "=" else text "->"
        do_guard (NormalG g, e) = text "|" <+> ppr g <+> eqd <+> ppr e
        do_guard (PatG ss, e) = text "|" <+> vcat (map ppr ss)
                             $$ nest nestDepth (eqd <+> ppr e)
pprBody eq (NormalB e) = (if eq then text "=" else text "->") <+> ppr e

------------------------------
pprLit :: Precedence -> Lit -> Doc
pprLit i (IntPrimL x)    = parensIf (i > noPrec && x < 0)
                                    (integer x <> char '#')
pprLit i (FloatPrimL x)  = parensIf (i > noPrec && x < 0)
                                    (float (fromRational x) <> char '#')
pprLit i (DoublePrimL x) = parensIf (i > noPrec && x < 0)
                                    (double (fromRational x) <> text "##")
pprLit i (IntegerL x)    = parensIf (i > noPrec && x < 0) (integer x)
pprLit _ (CharL c)       = text (show c)
pprLit _ (StringL s)     = text (show s)
pprLit i (RationalL rat) = parensIf (i > noPrec) $ rational rat

------------------------------
instance Ppr Pat where
    ppr = pprPat noPrec

pprPat :: Precedence -> Pat -> Doc
pprPat i (LitP l)     = pprLit i l
pprPat _ (VarP v)     = ppr v
pprPat _ (TupP ps)    = parens $ sep $ punctuate comma $ map ppr ps
pprPat i (ConP s ps)  = parensIf (i > noPrec) $ ppr s
                                            <+> sep (map (pprPat appPrec) ps)
pprPat i (InfixP p1 n p2)
                      = parensIf (i > noPrec)
                      $ pprPat opPrec p1 <+> ppr n <+> pprPat opPrec p2
pprPat i (TildeP p)   = parensIf (i > noPrec) $ pprPat appPrec p
pprPat i (AsP v p)    = parensIf (i > noPrec) $ ppr v <> text "@"
                                                      <> pprPat appPrec p
pprPat _ WildP        = text "_"
pprPat _ (RecP nm fs)
 = parens $     ppr nm
            <+> braces (sep $ punctuate comma $
                        map (\(s,p) -> ppr s <+> equals <+> ppr p) fs)
pprPat _ (ListP ps) = brackets $ sep $ punctuate comma $ map ppr ps
pprPat i (SigP p t) = parensIf (i > noPrec) $ ppr p <+> text "::" <+> ppr t

------------------------------
instance Ppr Dec where
    ppr (FunD f cs)   = vcat $ map (\c -> ppr f <+> ppr c) cs
    ppr (ValD p r ds) = ppr p <+> pprBody True r
                     $$ where_clause ds
    ppr (TySynD t xs rhs) = text "type" <+> ppr t <+> hsep (map ppr xs) 
                        <+> text "=" <+> ppr rhs
    ppr (DataD ctxt t xs cs decs)
        = text "data"
      <+> pprCxt ctxt
      <+> ppr t <+> hsep (map ppr xs)
      <+> sep (pref $ map ppr cs)
       $$ if null decs
          then empty
          else nest nestDepth
             $ text "deriving"
           <+> parens (hsep $ punctuate comma $ map ppr decs)
        where pref :: [Doc] -> [Doc]
              pref [] = [char '='] -- Can't happen in H98
              pref (d:ds) = (char '=' <+> d):map (char '|' <+>) ds
    ppr (NewtypeD ctxt t xs c decs)
        = text "newtype"
      <+> pprCxt ctxt
      <+> ppr t <+> hsep (map ppr xs)
      <+> char '=' <+> ppr c
       $$ if null decs
          then empty
          else nest nestDepth
             $ text "deriving"
           <+> parens (hsep $ punctuate comma $ map ppr decs)
    ppr (ClassD ctxt c xs fds ds) = text "class" <+> pprCxt ctxt
                                <+> ppr c <+> hsep (map ppr xs) <+> ppr fds
                                 $$ where_clause ds
    ppr (InstanceD ctxt i ds) = text "instance" <+> pprCxt ctxt <+> ppr i
                             $$ where_clause ds
    ppr (SigD f t) = ppr f <+> text "::" <+> ppr t
    ppr (ForeignD f) = ppr f

------------------------------
instance Ppr FunDep where
    ppr (FunDep xs ys) = hsep (map ppr xs) <+> text "->" <+> hsep (map ppr ys)
    ppr_list xs = char '|' <+> sep (punctuate (text ", ") (map ppr xs))

------------------------------
instance Ppr Foreign where
    ppr (ImportF callconv safety impent as typ)
       = text "foreign import"
     <+> showtextl callconv
     <+> showtextl safety
     <+> text (show impent)
     <+> ppr as
     <+> text "::" <+> ppr typ
    ppr (ExportF callconv expent as typ)
        = text "foreign export"
      <+> showtextl callconv
      <+> text (show expent)
      <+> ppr as
      <+> text "::" <+> ppr typ

------------------------------
instance Ppr Clause where
    ppr (Clause ps rhs ds) = hsep (map ppr ps) <+> pprBody True rhs
                          $$ where_clause ds

------------------------------
instance Ppr Con where
    ppr (NormalC c sts) = ppr c <+> hsep (map pprStrictType sts)
    ppr (RecC c vsts)
        = ppr c <+> braces (hsep (punctuate comma $ map pprVarStrictType vsts))
    ppr (InfixC st1 c st2) = pprStrictType st1 <+> ppr c <+> pprStrictType st2
    ppr (ForallC ns ctxt con) = text "forall" <+> hsep (map ppr ns)
                            <+> char '.' <+> pprCxt ctxt <+> ppr con

------------------------------
pprVarStrictType :: (Name, Strict, Type) -> Doc
-- Slight infelicity: with print non-atomic type with parens
pprVarStrictType (v, str, t) = ppr v <+> text "::" <+> pprStrictType (str, t)

------------------------------
pprStrictType :: (Strict, Type) -> Doc
-- Prints with parens if not already atomic
pprStrictType (IsStrict, t) = char '!' <> pprParendType t
pprStrictType (NotStrict, t) = pprParendType t

------------------------------
pprParendType :: Type -> Doc
pprParendType (VarT v)   = ppr v
pprParendType (ConT c)   = ppr c
pprParendType (TupleT 0) = text "()"
pprParendType (TupleT n) = parens (hcat (replicate (n-1) comma))
pprParendType ArrowT     = parens (text "->")
pprParendType ListT      = text "[]"
pprParendType other      = parens (ppr other)

instance Ppr Type where
    ppr (ForallT tvars ctxt ty) = 
        text "forall" <+> hsep (map ppr tvars) <+> text "."
                      <+> pprCxt ctxt <+> ppr ty
    ppr ty = pprTyApp (split ty)

pprTyApp :: (Type, [Type]) -> Doc
pprTyApp (ArrowT, [arg1,arg2]) = sep [ppr arg1 <+> text "->", ppr arg2]
pprTyApp (ListT, [arg]) = brackets (ppr arg)
pprTyApp (TupleT n, args)
 | length args == n = parens (sep (punctuate comma (map ppr args)))
pprTyApp (fun, args) = pprParendType fun <+> sep (map pprParendType args)

split :: Type -> (Type, [Type])    -- Split into function and args
split t = go t []
    where go (AppT t1 t2) args = go t1 (t2:args)
          go ty           args = (ty, args)

------------------------------
pprCxt :: Cxt -> Doc
pprCxt [] = empty
pprCxt [t] = ppr t <+> text "=>"
pprCxt ts = parens (hsep $ punctuate comma $ map ppr ts) <+> text "=>"

------------------------------
instance Ppr Range where
    ppr = brackets . pprRange
        where pprRange :: Range -> Doc
              pprRange (FromR e) = ppr e <> text ".."
              pprRange (FromThenR e1 e2) = ppr e1 <> text ","
                                        <> ppr e2 <> text ".."
              pprRange (FromToR e1 e2) = ppr e1 <> text ".." <> ppr e2
              pprRange (FromThenToR e1 e2 e3) = ppr e1 <> text ","
                                             <> ppr e2 <> text ".."
                                             <> ppr e3

------------------------------
where_clause :: [Dec] -> Doc
where_clause [] = empty
where_clause ds = nest nestDepth $ text "where" <+> vcat (map ppr ds)

showtextl :: Show a => a -> Doc
showtextl = text . map toLower . show

