-- | contains a prettyprinter for the
-- Template Haskell datatypes

module Language.Haskell.TH.Ppr where
    -- All of the exports from this module should
    -- be "public" functions.  The main module TH
    -- re-exports them all.

import Text.PrettyPrint (render)
import Language.Haskell.TH.PprLib
import Language.Haskell.TH.Syntax
import Data.Word ( Word8 )
import Data.Char ( toLower, chr, ord, isSymbol )
import GHC.Show  ( showMultiLineString )
import Data.Ratio ( numerator, denominator )

nestDepth :: Int
nestDepth = 4

type Precedence = Int
appPrec, unopPrec, opPrec, noPrec :: Precedence
appPrec = 3    -- Argument of a function application
opPrec  = 2    -- Argument of an infix operator
unopPrec = 1   -- Argument of an unresolved infix operator
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
    ppr v = pprName v

------------------------------
instance Ppr Info where
    ppr (TyConI d)     = ppr d
    ppr (ClassI d is)  = ppr d $$ vcat (map ppr is)
    ppr (FamilyI d is) = ppr d $$ vcat (map ppr is)
    ppr (PrimTyConI name arity is_unlifted)
      = text "Primitive"
        <+> (if is_unlifted then text "unlifted" else empty)
        <+> text "type constructor" <+> quotes (ppr name)
        <+> parens (text "arity" <+> int arity)
    ppr (ClassOpI v ty cls)
      = text "Class op from" <+> ppr cls <> colon <+> ppr_sig v ty
    ppr (DataConI v ty tc)
      = text "Constructor from" <+> ppr tc <> colon <+> ppr_sig v ty
    ppr (TyVarI v ty)
      = text "Type variable" <+> ppr v <+> equals <+> ppr ty
    ppr (VarI v ty mb_d)
      = vcat [ppr_sig v ty,
              case mb_d of { Nothing -> empty; Just d -> ppr d }]

ppr_sig :: Name -> Type -> Doc
ppr_sig v ty = pprName' Applied v <+> dcolon <+> ppr ty

pprFixity :: Name -> Fixity -> Doc
pprFixity _ f | f == defaultFixity = empty
pprFixity v (Fixity i d) = ppr_fix d <+> int i <+> ppr v
    where ppr_fix InfixR = text "infixr"
          ppr_fix InfixL = text "infixl"
          ppr_fix InfixN = text "infix"


------------------------------
instance Ppr Module where
  ppr (Module pkg m) = text (pkgString pkg) <+> text (modString m)

instance Ppr ModuleInfo where
  ppr (ModuleInfo imps) = text "Module" <+> vcat (map ppr imps)

------------------------------
instance Ppr Exp where
    ppr = pprExp noPrec

pprPrefixOcc :: Name -> Doc
-- Print operators with parens around them
pprPrefixOcc n = parensIf (isSymOcc n) (ppr n)

isSymOcc :: Name -> Bool
isSymOcc n
  = case nameBase n of
      []    -> True  -- Empty name; weird
      (c:_) -> isSymbolASCII c || (ord c > 0x7f && isSymbol c)
                   -- c.f. OccName.startsVarSym in GHC itself

isSymbolASCII :: Char -> Bool
isSymbolASCII c = c `elem` "!#$%&*+./<=>?@\\^|~-"

pprInfixExp :: Exp -> Doc
pprInfixExp (VarE v) = pprName' Infix v
pprInfixExp (ConE v) = pprName' Infix v
pprInfixExp _        = text "<<Non-variable/constructor in infix context>>"

pprExp :: Precedence -> Exp -> Doc
pprExp _ (VarE v)     = pprName' Applied v
pprExp _ (ConE c)     = pprName' Applied c
pprExp i (LitE l)     = pprLit i l
pprExp i (AppE e1 e2) = parensIf (i >= appPrec) $ pprExp opPrec e1
                                              <+> pprExp appPrec e2
pprExp _ (ParensE e)  = parens (pprExp noPrec e)
pprExp i (UInfixE e1 op e2)
 = parensIf (i > unopPrec) $ pprExp unopPrec e1
                         <+> pprInfixExp op
                         <+> pprExp unopPrec e2
pprExp i (InfixE (Just e1) op (Just e2))
 = parensIf (i >= opPrec) $ pprExp opPrec e1
                        <+> pprInfixExp op
                        <+> pprExp opPrec e2
pprExp _ (InfixE me1 op me2) = parens $ pprMaybeExp noPrec me1
                                    <+> pprInfixExp op
                                    <+> pprMaybeExp noPrec me2
pprExp i (LamE ps e) = parensIf (i > noPrec) $ char '\\' <> hsep (map (pprPat appPrec) ps)
                                           <+> text "->" <+> ppr e
pprExp i (LamCaseE ms) = parensIf (i > noPrec)
                       $ text "\\case" $$ nest nestDepth (ppr ms)
pprExp _ (TupE es) = parens (commaSep es)
pprExp _ (UnboxedTupE es) = hashParens (commaSep es)
-- Nesting in Cond is to avoid potential problems in do statments
pprExp i (CondE guard true false)
 = parensIf (i > noPrec) $ sep [text "if"   <+> ppr guard,
                       nest 1 $ text "then" <+> ppr true,
                       nest 1 $ text "else" <+> ppr false]
pprExp i (MultiIfE alts)
  = parensIf (i > noPrec) $ vcat $
      case alts of
        []            -> [text "if {}"]
        (alt : alts') -> text "if" <+> pprGuarded arrow alt
                         : map (nest 3 . pprGuarded arrow) alts'
pprExp i (LetE ds_ e) = parensIf (i > noPrec) $ text "let" <+> pprDecs ds_
                                             $$ text " in" <+> ppr e
  where
    pprDecs []  = empty
    pprDecs [d] = ppr d
    pprDecs ds  = braces (semiSep ds)

pprExp i (CaseE e ms)
 = parensIf (i > noPrec) $ text "case" <+> ppr e <+> text "of"
                        $$ nest nestDepth (ppr ms)
pprExp i (DoE ss_) = parensIf (i > noPrec) $ text "do" <+> pprStms ss_
  where
    pprStms []  = empty
    pprStms [s] = ppr s
    pprStms ss  = braces (semiSep ss)

pprExp _ (CompE []) = text "<<Empty CompExp>>"
-- This will probably break with fixity declarations - would need a ';'
pprExp _ (CompE ss) = text "[" <> ppr s
                  <+> text "|"
                  <+> commaSep ss'
                   <> text "]"
    where s = last ss
          ss' = init ss
pprExp _ (ArithSeqE d) = ppr d
pprExp _ (ListE es) = brackets (commaSep es)
pprExp i (SigE e t) = parensIf (i > noPrec) $ ppr e <+> dcolon <+> ppr t
pprExp _ (RecConE nm fs) = ppr nm <> braces (pprFields fs)
pprExp _ (RecUpdE e fs) = pprExp appPrec e <> braces (pprFields fs)
pprExp i (StaticE e) = parensIf (i >= appPrec) $
                         text "static"<+> pprExp appPrec e
pprExp _ (UnboundVarE v) = pprName' Applied v

pprFields :: [(Name,Exp)] -> Doc
pprFields = sep . punctuate comma . map (\(s,e) -> ppr s <+> equals <+> ppr e)

pprMaybeExp :: Precedence -> Maybe Exp -> Doc
pprMaybeExp _ Nothing = empty
pprMaybeExp i (Just e) = pprExp i e

------------------------------
instance Ppr Stmt where
    ppr (BindS p e) = ppr p <+> text "<-" <+> ppr e
    ppr (LetS ds) = text "let" <+> (braces (semiSep ds))
    ppr (NoBindS e) = ppr e
    ppr (ParS sss) = sep $ punctuate (text "|")
                         $ map commaSep sss

------------------------------
instance Ppr Match where
    ppr (Match p rhs ds) = ppr p <+> pprBody False rhs
                        $$ where_clause ds

------------------------------
pprGuarded :: Doc -> (Guard, Exp) -> Doc
pprGuarded eqDoc (guard, expr) = case guard of
  NormalG guardExpr -> char '|' <+> ppr guardExpr <+> eqDoc <+> ppr expr
  PatG    stmts     -> char '|' <+> vcat (punctuate comma $ map ppr stmts) $$
                         nest nestDepth (eqDoc <+> ppr expr)

------------------------------
pprBody :: Bool -> Body -> Doc
pprBody eq body = case body of
    GuardedB xs -> nest nestDepth $ vcat $ map (pprGuarded eqDoc) xs
    NormalB  e  -> eqDoc <+> ppr e
  where eqDoc | eq        = equals
              | otherwise = arrow

------------------------------
instance Ppr Lit where
  ppr = pprLit noPrec

pprLit :: Precedence -> Lit -> Doc
pprLit i (IntPrimL x)    = parensIf (i > noPrec && x < 0)
                                    (integer x <> char '#')
pprLit _ (WordPrimL x)    = integer x <> text "##"
pprLit i (FloatPrimL x)  = parensIf (i > noPrec && x < 0)
                                    (float (fromRational x) <> char '#')
pprLit i (DoublePrimL x) = parensIf (i > noPrec && x < 0)
                                    (double (fromRational x) <> text "##")
pprLit i (IntegerL x)    = parensIf (i > noPrec && x < 0) (integer x)
pprLit _ (CharL c)       = text (show c)
pprLit _ (CharPrimL c)   = text (show c) <> char '#'
pprLit _ (StringL s)     = pprString s
pprLit _ (StringPrimL s) = pprString (bytesToString s) <> char '#'
pprLit i (RationalL rat) = parensIf (i > noPrec) $
                           integer (numerator rat) <+> char '/'
                              <+> integer (denominator rat)

bytesToString :: [Word8] -> String
bytesToString = map (chr . fromIntegral)

pprString :: String -> Doc
-- Print newlines as newlines with Haskell string escape notation,
-- not as '\n'.  For other non-printables use regular escape notation.
pprString s = vcat (map text (showMultiLineString s))

------------------------------
instance Ppr Pat where
    ppr = pprPat noPrec

pprPat :: Precedence -> Pat -> Doc
pprPat i (LitP l)     = pprLit i l
pprPat _ (VarP v)     = pprName' Applied v
pprPat _ (TupP ps)    = parens (commaSep ps)
pprPat _ (UnboxedTupP ps) = hashParens (commaSep ps)
pprPat i (ConP s ps)  = parensIf (i >= appPrec) $ pprName' Applied s
                                              <+> sep (map (pprPat appPrec) ps)
pprPat _ (ParensP p)  = parens $ pprPat noPrec p
pprPat i (UInfixP p1 n p2)
                      = parensIf (i > unopPrec) (pprPat unopPrec p1 <+>
                                                 pprName' Infix n   <+>
                                                 pprPat unopPrec p2)
pprPat i (InfixP p1 n p2)
                      = parensIf (i >= opPrec) (pprPat opPrec p1 <+>
                                                pprName' Infix n <+>
                                                pprPat opPrec p2)
pprPat i (TildeP p)   = parensIf (i > noPrec) $ char '~' <> pprPat appPrec p
pprPat i (BangP p)    = parensIf (i > noPrec) $ char '!' <> pprPat appPrec p
pprPat i (AsP v p)    = parensIf (i > noPrec) $ ppr v <> text "@"
                                                      <> pprPat appPrec p
pprPat _ WildP        = text "_"
pprPat _ (RecP nm fs)
 = parens $     ppr nm
            <+> braces (sep $ punctuate comma $
                        map (\(s,p) -> ppr s <+> equals <+> ppr p) fs)
pprPat _ (ListP ps) = brackets (commaSep ps)
pprPat i (SigP p t) = parensIf (i > noPrec) $ ppr p <+> dcolon <+> ppr t
pprPat _ (ViewP e p) = parens $ pprExp noPrec e <+> text "->" <+> pprPat noPrec p

------------------------------
instance Ppr Dec where
    ppr = ppr_dec True

ppr_dec :: Bool     -- declaration on the toplevel?
        -> Dec
        -> Doc
ppr_dec _ (FunD f cs)   = vcat $ map (\c -> pprPrefixOcc f <+> ppr c) cs
ppr_dec _ (ValD p r ds) = ppr p <+> pprBody True r
                          $$ where_clause ds
ppr_dec _ (TySynD t xs rhs)
  = ppr_tySyn empty t (hsep (map ppr xs)) rhs
ppr_dec _ (DataD ctxt t xs ksig cs decs)
  = ppr_data empty ctxt t (hsep (map ppr xs)) ksig cs decs
ppr_dec _ (NewtypeD ctxt t xs ksig c decs)
  = ppr_newtype empty ctxt t (sep (map ppr xs)) ksig c decs
ppr_dec _  (ClassD ctxt c xs fds ds)
  = text "class" <+> pprCxt ctxt <+> ppr c <+> hsep (map ppr xs) <+> ppr fds
    $$ where_clause ds
ppr_dec _ (InstanceD o ctxt i ds) =
        text "instance" <+> maybe empty ppr_overlap o <+> pprCxt ctxt <+> ppr i
                                  $$ where_clause ds
ppr_dec _ (SigD f t)    = pprPrefixOcc f <+> dcolon <+> ppr t
ppr_dec _ (ForeignD f)  = ppr f
ppr_dec _ (InfixD fx n) = pprFixity n fx
ppr_dec _ (PragmaD p)   = ppr p
ppr_dec isTop (DataFamilyD tc tvs kind)
  = text "data" <+> maybeFamily <+> ppr tc <+> hsep (map ppr tvs) <+> maybeKind
  where
    maybeFamily | isTop     = text "family"
                | otherwise = empty
    maybeKind | (Just k') <- kind = dcolon <+> ppr k'
              | otherwise = empty
ppr_dec isTop (DataInstD ctxt tc tys ksig cs decs)
  = ppr_data maybeInst ctxt tc (sep (map pprParendType tys)) ksig cs decs
  where
    maybeInst | isTop     = text "instance"
              | otherwise = empty
ppr_dec isTop (NewtypeInstD ctxt tc tys ksig c decs)
  = ppr_newtype maybeInst ctxt tc (sep (map pprParendType tys)) ksig c decs
  where
    maybeInst | isTop     = text "instance"
              | otherwise = empty
ppr_dec isTop (TySynInstD tc (TySynEqn tys rhs))
  = ppr_tySyn maybeInst tc (sep (map pprParendType tys)) rhs
  where
    maybeInst | isTop     = text "instance"
              | otherwise = empty
ppr_dec isTop (OpenTypeFamilyD tfhead)
  = text "type" <+> maybeFamily <+> ppr_tf_head tfhead
  where
    maybeFamily | isTop     = text "family"
                | otherwise = empty
ppr_dec _ (ClosedTypeFamilyD tfhead@(TypeFamilyHead tc _ _ _) eqns)
  = hang (text "type family" <+> ppr_tf_head tfhead <+> text "where")
      nestDepth (vcat (map ppr_eqn eqns))
  where
    ppr_eqn (TySynEqn lhs rhs)
      = ppr tc <+> sep (map pprParendType lhs) <+> text "=" <+> ppr rhs

ppr_dec _ (RoleAnnotD name roles)
  = hsep [ text "type role", ppr name ] <+> hsep (map ppr roles)

ppr_dec _ (StandaloneDerivD cxt ty)
  = hsep [ text "deriving instance", pprCxt cxt, ppr ty ]

ppr_dec _ (DefaultSigD n ty)
  = hsep [ text "default", pprPrefixOcc n, dcolon, ppr ty ]


ppr_overlap :: Overlap -> Doc
ppr_overlap o = text $
  case o of
    Overlaps      -> "{-# OVERLAPS #-}"
    Overlappable  -> "{-# OVERLAPPABLE #-}"
    Overlapping   -> "{-# OVERLAPPING #-}"
    Incoherent    -> "{-# INCOHERENT #-}"

ppr_data :: Doc -> Cxt -> Name -> Doc -> Maybe Kind -> [Con] -> Cxt -> Doc
ppr_data maybeInst ctxt t argsDoc ksig cs decs
  = sep [text "data" <+> maybeInst
            <+> pprCxt ctxt
            <+> ppr t <+> argsDoc <+> ksigDoc <+> maybeWhere,
         nest nestDepth (sep (pref $ map ppr cs)),
         if null decs
           then empty
           else nest nestDepth
              $ text "deriving" <+> ppr_cxt_preds decs]
  where
    pref :: [Doc] -> [Doc]
    pref xs | isGadtDecl = xs
    pref []              = []      -- No constructors; can't happen in H98
    pref (d:ds)          = (char '=' <+> d):map (char '|' <+>) ds

    maybeWhere :: Doc
    maybeWhere | isGadtDecl = text "where"
               | otherwise  = empty

    isGadtDecl :: Bool
    isGadtDecl = not (null cs) && all isGadtCon cs
        where isGadtCon (GadtC _ _ _   ) = True
              isGadtCon (RecGadtC _ _ _) = True
              isGadtCon (ForallC _ _ x ) = isGadtCon x
              isGadtCon  _               = False

    ksigDoc = case ksig of
                Nothing -> empty
                Just k  -> dcolon <+> ppr k

ppr_newtype :: Doc -> Cxt -> Name -> Doc -> Maybe Kind -> Con -> Cxt -> Doc
ppr_newtype maybeInst ctxt t argsDoc ksig c decs
  = sep [text "newtype" <+> maybeInst
            <+> pprCxt ctxt
            <+> ppr t <+> argsDoc <+> ksigDoc,
         nest 2 (char '=' <+> ppr c),
         if null decs
           then empty
           else nest nestDepth
                $ text "deriving" <+> ppr_cxt_preds decs]
  where
    ksigDoc = case ksig of
                Nothing -> empty
                Just k  -> dcolon <+> ppr k

ppr_tySyn :: Doc -> Name -> Doc -> Type -> Doc
ppr_tySyn maybeInst t argsDoc rhs
  = text "type" <+> maybeInst <+> ppr t <+> argsDoc <+> text "=" <+> ppr rhs

ppr_tf_head :: TypeFamilyHead -> Doc
ppr_tf_head (TypeFamilyHead tc tvs res inj)
  = ppr tc <+> hsep (map ppr tvs) <+> ppr res <+> maybeInj
  where
    maybeInj | (Just inj') <- inj = ppr inj'
             | otherwise          = empty

------------------------------
instance Ppr FunDep where
    ppr (FunDep xs ys) = hsep (map ppr xs) <+> text "->" <+> hsep (map ppr ys)
    ppr_list [] = empty
    ppr_list xs = char '|' <+> commaSep xs

------------------------------
instance Ppr FamFlavour where
    ppr DataFam = text "data"
    ppr TypeFam = text "type"

------------------------------
instance Ppr FamilyResultSig where
    ppr NoSig           = empty
    ppr (KindSig k)     = dcolon <+> ppr k
    ppr (TyVarSig bndr) = text "=" <+> ppr bndr

------------------------------
instance Ppr InjectivityAnn where
    ppr (InjectivityAnn lhs rhs) =
        char '|' <+> ppr lhs <+> text "->" <+> hsep (map ppr rhs)

------------------------------
instance Ppr Foreign where
    ppr (ImportF callconv safety impent as typ)
       = text "foreign import"
     <+> showtextl callconv
     <+> showtextl safety
     <+> text (show impent)
     <+> ppr as
     <+> dcolon <+> ppr typ
    ppr (ExportF callconv expent as typ)
        = text "foreign export"
      <+> showtextl callconv
      <+> text (show expent)
      <+> ppr as
      <+> dcolon <+> ppr typ

------------------------------
instance Ppr Pragma where
    ppr (InlineP n inline rm phases)
       = text "{-#"
     <+> ppr inline
     <+> ppr rm
     <+> ppr phases
     <+> ppr n
     <+> text "#-}"
    ppr (SpecialiseP n ty inline phases)
       =   text "{-# SPECIALISE"
       <+> maybe empty ppr inline
       <+> ppr phases
       <+> sep [ ppr n <+> dcolon
               , nest 2 $ ppr ty ]
       <+> text "#-}"
    ppr (SpecialiseInstP inst)
       = text "{-# SPECIALISE instance" <+> ppr inst <+> text "#-}"
    ppr (RuleP n bndrs lhs rhs phases)
       = sep [ text "{-# RULES" <+> pprString n <+> ppr phases
             , nest 4 $ ppr_forall <+> ppr lhs
             , nest 4 $ char '=' <+> ppr rhs <+> text "#-}" ]
      where ppr_forall | null bndrs =   empty
                       | otherwise  =   text "forall"
                                    <+> fsep (map ppr bndrs)
                                    <+> char '.'
    ppr (AnnP tgt expr)
       = text "{-# ANN" <+> target1 tgt <+> ppr expr <+> text "#-}"
      where target1 ModuleAnnotation    = text "module"
            target1 (TypeAnnotation t)  = text "type" <+> ppr t
            target1 (ValueAnnotation v) = ppr v
    ppr (LineP line file)
       = text "{-# LINE" <+> int line <+> text (show file) <+> text "#-}"

------------------------------
instance Ppr Inline where
    ppr NoInline  = text "NOINLINE"
    ppr Inline    = text "INLINE"
    ppr Inlinable = text "INLINABLE"

------------------------------
instance Ppr RuleMatch where
    ppr ConLike = text "CONLIKE"
    ppr FunLike = empty

------------------------------
instance Ppr Phases where
    ppr AllPhases       = empty
    ppr (FromPhase i)   = brackets $ int i
    ppr (BeforePhase i) = brackets $ char '~' <> int i

------------------------------
instance Ppr RuleBndr where
    ppr (RuleVar n)         = ppr n
    ppr (TypedRuleVar n ty) = parens $ ppr n <+> dcolon <+> ppr ty

------------------------------
instance Ppr Clause where
    ppr (Clause ps rhs ds) = hsep (map (pprPat appPrec) ps) <+> pprBody True rhs
                             $$ where_clause ds

------------------------------
instance Ppr Con where
    ppr (NormalC c sts) = ppr c <+> sep (map pprBangType sts)

    ppr (RecC c vsts)
        = ppr c <+> braces (sep (punctuate comma $ map pprVarBangType vsts))

    ppr (InfixC st1 c st2) = pprBangType st1
                         <+> pprName' Infix c
                         <+> pprBangType st2

    ppr (ForallC ns ctxt (GadtC c sts ty))
        = commaSepApplied c <+> dcolon <+> pprForall ns ctxt
      <+> pprGadtRHS sts ty

    ppr (ForallC ns ctxt (RecGadtC c vsts ty))
        = commaSepApplied c <+> dcolon <+> pprForall ns ctxt
      <+> pprRecFields vsts ty

    ppr (ForallC ns ctxt con)
        = pprForall ns ctxt <+> ppr con

    ppr (GadtC c sts ty)
        = commaSepApplied c <+> dcolon <+> pprGadtRHS sts ty

    ppr (RecGadtC c vsts ty)
        = commaSepApplied c <+> dcolon <+> pprRecFields vsts ty

commaSepApplied :: [Name] -> Doc
commaSepApplied = commaSepWith (pprName' Applied)

pprForall :: [TyVarBndr] -> Cxt -> Doc
pprForall ns ctxt
    = text "forall" <+> hsep (map ppr ns)
  <+> char '.' <+> pprCxt ctxt

pprRecFields :: [(Name, Strict, Type)] -> Type -> Doc
pprRecFields vsts ty
    = braces (sep (punctuate comma $ map pprVarBangType vsts))
  <+> arrow <+> ppr ty

pprGadtRHS :: [(Strict, Type)] -> Type -> Doc
pprGadtRHS [] ty
    = ppr ty
pprGadtRHS sts ty
    = sep (punctuate (space <> arrow) (map pprBangType sts))
  <+> arrow <+> ppr ty

------------------------------
pprVarBangType :: VarBangType -> Doc
-- Slight infelicity: with print non-atomic type with parens
pprVarBangType (v, bang, t) = ppr v <+> dcolon <+> pprBangType (bang, t)

------------------------------
pprBangType :: BangType -> Doc
-- Make sure we print
--
-- Con {-# UNPACK #-} a
--
-- rather than
--
-- Con {-# UNPACK #-}a
--
-- when there's no strictness annotation. If there is a strictness annotation,
-- it's okay to not put a space between it and the type.
pprBangType (bt@(Bang _ NoSourceStrictness), t) = ppr bt <+> pprParendType t
pprBangType (bt, t) = ppr bt <> pprParendType t

------------------------------
instance Ppr Bang where
    ppr (Bang su ss) = ppr su <+> ppr ss

------------------------------
instance Ppr SourceUnpackedness where
    ppr NoSourceUnpackedness = empty
    ppr SourceNoUnpack       = text "{-# NOUNPACK #-}"
    ppr SourceUnpack         = text "{-# UNPACK #-}"

------------------------------
instance Ppr SourceStrictness where
    ppr NoSourceStrictness = empty
    ppr SourceLazy         = char '~'
    ppr SourceStrict       = char '!'

------------------------------
instance Ppr DecidedStrictness where
    ppr DecidedLazy   = empty
    ppr DecidedStrict = char '!'
    ppr DecidedUnpack = text "{-# UNPACK #-} !"

------------------------------
{-# DEPRECATED pprVarStrictType
               "As of @template-haskell-2.11.0.0@, 'VarStrictType' has been replaced by 'VarBangType'. Please use 'pprVarBangType' instead." #-}
pprVarStrictType :: (Name, Strict, Type) -> Doc
pprVarStrictType = pprVarBangType

------------------------------
{-# DEPRECATED pprStrictType
               "As of @template-haskell-2.11.0.0@, 'StrictType' has been replaced by 'BangType'. Please use 'pprBangType' instead." #-}
pprStrictType :: (Strict, Type) -> Doc
pprStrictType = pprBangType

------------------------------
pprParendType :: Type -> Doc
pprParendType (VarT v)            = ppr v
pprParendType (ConT c)            = ppr c
pprParendType (TupleT 0)          = text "()"
pprParendType (TupleT n)          = parens (hcat (replicate (n-1) comma))
pprParendType (UnboxedTupleT n)   = hashParens $ hcat $ replicate (n-1) comma
pprParendType ArrowT              = parens (text "->")
pprParendType ListT               = text "[]"
pprParendType (LitT l)            = pprTyLit l
pprParendType (PromotedT c)       = text "'" <> ppr c
pprParendType (PromotedTupleT 0)  = text "'()"
pprParendType (PromotedTupleT n)  = quoteParens (hcat (replicate (n-1) comma))
pprParendType PromotedNilT        = text "'[]"
pprParendType PromotedConsT       = text "(':)"
pprParendType StarT               = char '*'
pprParendType ConstraintT         = text "Constraint"
pprParendType (SigT ty k)         = parens (ppr ty <+> text "::" <+> ppr k)
pprParendType WildCardT           = char '_'
pprParendType (InfixT x n y)      = parens (ppr x <+> pprName' Infix n <+> ppr y)
pprParendType t@(UInfixT {})      = parens (pprUInfixT t)
pprParendType (ParensT t)         = ppr t
pprParendType tuple | (TupleT n, args) <- split tuple
                    , length args == n
                    = parens (commaSep args)
pprParendType other               = parens (ppr other)

pprUInfixT :: Type -> Doc
pprUInfixT (UInfixT x n y) = pprUInfixT x <+> pprName' Infix n <+> pprUInfixT y
pprUInfixT t               = ppr t

instance Ppr Type where
    ppr (ForallT tvars ctxt ty)
      = text "forall" <+> hsep (map ppr tvars) <+> text "."
                      <+> sep [pprCxt ctxt, ppr ty]
    ppr ty = pprTyApp (split ty)
       -- Works, in a degnerate way, for SigT, and puts parens round (ty :: kind)
       -- See Note [Pretty-printing kind signatures]

{- Note [Pretty-printing kind signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC's parser only recognises a kind signature in a type when there are
parens around it.  E.g. the parens are required here:
   f :: (Int :: *)
   type instance F Int = (Bool :: *)
So we always print a SigT with parens (see Trac #10050). -}

pprTyApp :: (Type, [Type]) -> Doc
pprTyApp (ArrowT, [arg1,arg2]) = sep [pprFunArgType arg1 <+> text "->", ppr arg2]
pprTyApp (EqualityT, [arg1, arg2]) =
    sep [pprFunArgType arg1 <+> text "~", ppr arg2]
pprTyApp (ListT, [arg]) = brackets (ppr arg)
pprTyApp (TupleT n, args)
 | length args == n = parens (commaSep args)
pprTyApp (PromotedTupleT n, args)
 | length args == n = quoteParens (commaSep args)
pprTyApp (fun, args) = pprParendType fun <+> sep (map pprParendType args)

pprFunArgType :: Type -> Doc    -- Should really use a precedence argument
-- Everything except forall and (->) binds more tightly than (->)
pprFunArgType ty@(ForallT {})                 = parens (ppr ty)
pprFunArgType ty@((ArrowT `AppT` _) `AppT` _) = parens (ppr ty)
pprFunArgType ty@(SigT _ _)                   = parens (ppr ty)
pprFunArgType ty                              = ppr ty

split :: Type -> (Type, [Type])    -- Split into function and args
split t = go t []
    where go (AppT t1 t2) args = go t1 (t2:args)
          go ty           args = (ty, args)

pprTyLit :: TyLit -> Doc
pprTyLit (NumTyLit n) = integer n
pprTyLit (StrTyLit s) = text (show s)

instance Ppr TyLit where
  ppr = pprTyLit

------------------------------
instance Ppr TyVarBndr where
    ppr (PlainTV nm)    = ppr nm
    ppr (KindedTV nm k) = parens (ppr nm <+> dcolon <+> ppr k)

instance Ppr Role where
    ppr NominalR          = text "nominal"
    ppr RepresentationalR = text "representational"
    ppr PhantomR          = text "phantom"
    ppr InferR            = text "_"

------------------------------
pprCxt :: Cxt -> Doc
pprCxt [] = empty
pprCxt ts = ppr_cxt_preds ts <+> text "=>"

ppr_cxt_preds :: Cxt -> Doc
ppr_cxt_preds [] = empty
ppr_cxt_preds [t] = ppr t
ppr_cxt_preds ts = parens (commaSep ts)

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
where_clause ds = nest nestDepth $ text "where" <+> vcat (map (ppr_dec False) ds)

showtextl :: Show a => a -> Doc
showtextl = text . map toLower . show

hashParens :: Doc -> Doc
hashParens d = text "(# " <> d <> text " #)"

quoteParens :: Doc -> Doc
quoteParens d = text "'(" <> d <> text ")"

-----------------------------
instance Ppr Loc where
  ppr (Loc { loc_module = md
           , loc_package = pkg
           , loc_start = (start_ln, start_col)
           , loc_end = (end_ln, end_col) })
    = hcat [ text pkg, colon, text md, colon
           , parens $ int start_ln <> comma <> int start_col
           , text "-"
           , parens $ int end_ln <> comma <> int end_col ]

-- Takes a list of printable things and prints them separated by commas followed
-- by space.
commaSep :: Ppr a => [a] -> Doc
commaSep = commaSepWith ppr

-- Takes a list of things and prints them with the given pretty-printing
-- function, separated by commas followed by space.
commaSepWith :: (a -> Doc) -> [a] -> Doc
commaSepWith pprFun = sep . punctuate comma . map pprFun

-- Takes a list of printable things and prints them separated by semicolons
-- followed by space.
semiSep :: Ppr a => [a] -> Doc
semiSep = sep . punctuate semi . map ppr
