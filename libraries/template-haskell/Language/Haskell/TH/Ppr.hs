{-# LANGUAGE Safe #-}
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
import Data.Char ( toLower, chr)
import GHC.Show  ( showMultiLineString )
import GHC.Lexeme( startsVarSym )
import Data.Ratio ( numerator, denominator )
import Data.Foldable ( toList )
import Prelude hiding ((<>))

nestDepth :: Int
nestDepth = 4

type Precedence = Int
appPrec, opPrec, unopPrec, sigPrec, noPrec :: Precedence
appPrec  = 4    -- Argument of a function application
opPrec   = 3    -- Argument of an infix operator
unopPrec = 2    -- Argument of an unresolved infix operator
sigPrec  = 1    -- Argument of an explicit type signature
noPrec   = 0    -- Others

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
    ppr (PatSynI nm ty) = pprPatSynSig nm ty
    ppr (TyVarI v ty)
      = text "Type variable" <+> ppr v <+> equals <+> ppr ty
    ppr (VarI v ty mb_d)
      = vcat [ppr_sig v ty,
              case mb_d of { Nothing -> empty; Just d -> ppr d }]

ppr_sig :: Name -> Type -> Doc
ppr_sig v ty = pprName' Applied v <+> dcolon <+> ppr ty

pprFixity :: Name -> Fixity -> Doc
pprFixity _ f | f == defaultFixity = empty
pprFixity v (Fixity i d) = ppr_fix d <+> int i <+> pprName' Infix v
    where ppr_fix InfixR = text "infixr"
          ppr_fix InfixL = text "infixl"
          ppr_fix InfixN = text "infix"

-- | Pretty prints a pattern synonym type signature
pprPatSynSig :: Name -> PatSynType -> Doc
pprPatSynSig nm ty
  = text "pattern" <+> pprPrefixOcc nm <+> dcolon <+> pprPatSynType ty

-- | Pretty prints a pattern synonym's type; follows the usual
-- conventions to print a pattern synonym type compactly, yet
-- unambiguously. See the note on 'PatSynType' and the section on
-- pattern synonyms in the GHC user's guide for more information.
pprPatSynType :: PatSynType -> Doc
pprPatSynType ty@(ForallT uniTys reqs ty'@(ForallT exTys provs ty''))
  | null exTys,  null provs = ppr (ForallT uniTys reqs ty'')
  | null uniTys, null reqs  = noreqs <+> ppr ty'
  | null reqs               = forall uniTys <+> noreqs <+> ppr ty'
  | otherwise               = ppr ty
  where noreqs     = text "() =>"
        forall tvs = text "forall" <+> (hsep (map ppr tvs)) <+> text "."
pprPatSynType ty            = ppr ty

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
      (c:_) -> startsVarSym c
                   -- c.f. OccName.startsVarSym in GHC itself

pprInfixExp :: Exp -> Doc
pprInfixExp (VarE v) = pprName' Infix v
pprInfixExp (ConE v) = pprName' Infix v
pprInfixExp (UnboundVarE v) = pprName' Infix v
-- This case will only ever be reached in exceptional circumstances.
-- For example, when printing an error message in case of a malformed expression.
pprInfixExp e = text "`" <> ppr e <> text "`"

pprExp :: Precedence -> Exp -> Doc
pprExp _ (VarE v)     = pprName' Applied v
pprExp _ (ConE c)     = pprName' Applied c
pprExp i (LitE l)     = pprLit i l
pprExp i (AppE e1 e2) = parensIf (i >= appPrec) $ pprExp opPrec e1
                                              <+> pprExp appPrec e2
pprExp i (AppTypeE e t)
 = parensIf (i >= appPrec) $ pprExp opPrec e <+> char '@' <> pprParendType t
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
pprExp i (LamE [] e) = pprExp i e -- #13856
pprExp i (LamE ps e) = parensIf (i > noPrec) $ char '\\' <> hsep (map (pprPat appPrec) ps)
                                           <+> text "->" <+> ppr e
pprExp i (LamCaseE ms) = parensIf (i > noPrec)
                       $ text "\\case" $$ nest nestDepth (ppr ms)
pprExp i (TupE es)
  | [Just e] <- es
  = pprExp i (ConE (tupleDataName 1) `AppE` e)
  | otherwise
  = parens (commaSepWith (pprMaybeExp noPrec) es)
pprExp _ (UnboxedTupE es) = hashParens (commaSepWith (pprMaybeExp noPrec) es)
pprExp _ (UnboxedSumE e alt arity) = unboxedSumBars (ppr e) alt arity
-- Nesting in Cond is to avoid potential problems in do statements
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
pprExp i (DoE m ss_) = parensIf (i > noPrec) $
    pprQualifier m <> text "do" <+> pprStms ss_
  where
    pprQualifier Nothing = empty
    pprQualifier (Just modName) = text (modString modName) <> char '.'
    pprStms []  = empty
    pprStms [s] = ppr s
    pprStms ss  = braces (semiSep ss)
pprExp i (MDoE m ss_) = parensIf (i > noPrec) $
    pprQualifier m <> text "mdo" <+> pprStms ss_
  where
    pprQualifier Nothing = empty
    pprQualifier (Just modName) = text (modString modName) <> char '.'
    pprStms []  = empty
    pprStms [s] = ppr s
    pprStms ss  = braces (semiSep ss)

pprExp _ (CompE []) = text "<<Empty CompExp>>"
-- This will probably break with fixity declarations - would need a ';'
pprExp _ (CompE ss) =
    if null ss'
       -- If there are no statements in a list comprehension besides the last
       -- one, we simply treat it like a normal list.
       then text "[" <> ppr s <> text "]"
       else text "[" <> ppr s
        <+> bar
        <+> commaSep ss'
         <> text "]"
  where s = last ss
        ss' = init ss
pprExp _ (ArithSeqE d) = ppr d
pprExp _ (ListE es) = brackets (commaSep es)
pprExp i (SigE e t) = parensIf (i > noPrec) $ pprExp sigPrec e
                                          <+> dcolon <+> ppr t
pprExp _ (RecConE nm fs) = pprName' Applied nm <> braces (pprFields fs)
pprExp _ (RecUpdE e fs) = pprExp appPrec e <> braces (pprFields fs)
pprExp i (StaticE e) = parensIf (i >= appPrec) $
                         text "static"<+> pprExp appPrec e
pprExp _ (UnboundVarE v) = pprName' Applied v
pprExp _ (LabelE s) = text "#" <> text s
pprExp _ (ImplicitParamVarE n) = text ('?' : n)
pprExp _ (GetFieldE e f) = pprExp appPrec e <> text ('.': f)
pprExp _ (ProjectionE xs) = parens $ hcat $ map ((char '.'<>) . text) $ toList xs

pprFields :: [(Name,Exp)] -> Doc
pprFields = sep . punctuate comma . map (\(s,e) -> pprName' Applied s <+> equals <+> ppr e)

pprMaybeExp :: Precedence -> Maybe Exp -> Doc
pprMaybeExp _ Nothing = empty
pprMaybeExp i (Just e) = pprExp i e

------------------------------
instance Ppr Stmt where
    ppr (BindS p e) = ppr p <+> text "<-" <+> ppr e
    ppr (LetS ds) = text "let" <+> (braces (semiSep ds))
    ppr (NoBindS e) = ppr e
    ppr (ParS sss) = sep $ punctuate bar
                         $ map commaSep sss
    ppr (RecS ss) = text "rec" <+> (braces (semiSep ss))

------------------------------
instance Ppr Match where
    ppr (Match p rhs ds) = pprMatchPat p <+> pprBody False rhs
                        $$ where_clause ds

pprMatchPat :: Pat -> Doc
-- Everything except pattern signatures bind more tightly than (->)
pprMatchPat p@(SigP {}) = parens (ppr p)
pprMatchPat p           = ppr p

------------------------------
pprGuarded :: Doc -> (Guard, Exp) -> Doc
pprGuarded eqDoc (guard, expr) = case guard of
  NormalG guardExpr -> bar <+> ppr guardExpr <+> eqDoc <+> ppr expr
  PatG    stmts     -> bar <+> vcat (punctuate comma $ map ppr stmts) $$
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
pprLit _ (BytesPrimL {}) = pprString "<binary data>"
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
pprPat i (TupP ps)
  | [_] <- ps
  = pprPat i (ConP (tupleDataName 1) [] ps)
  | otherwise
  = parens (commaSep ps)
pprPat _ (UnboxedTupP ps) = hashParens (commaSep ps)
pprPat _ (UnboxedSumP p alt arity) = unboxedSumBars (ppr p) alt arity
pprPat i (ConP s ts ps)  = parensIf (i >= appPrec) $
      pprName' Applied s
  <+> sep (map (\t -> char '@' <> pprParendType t) ts)
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
 = parens $     pprName' Applied nm
            <+> braces (sep $ punctuate comma $
                        map (\(s,p) -> pprName' Applied s <+> equals <+> ppr p) fs)
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
  = ppr_tySyn empty (Just t) (hsep (map ppr xs)) rhs
ppr_dec _ (DataD ctxt t xs ksig cs decs)
  = ppr_data empty ctxt (Just t) (hsep (map ppr xs)) ksig cs decs
ppr_dec _ (NewtypeD ctxt t xs ksig c decs)
  = ppr_newtype empty ctxt (Just t) (sep (map ppr xs)) ksig c decs
ppr_dec _  (ClassD ctxt c xs fds ds)
  = text "class" <+> pprCxt ctxt <+> ppr c <+> hsep (map ppr xs) <+> ppr fds
    $$ where_clause ds
ppr_dec _ (InstanceD o ctxt i ds) =
        text "instance" <+> maybe empty ppr_overlap o <+> pprCxt ctxt <+> ppr i
                                  $$ where_clause ds
ppr_dec _ (SigD f t)    = pprPrefixOcc f <+> dcolon <+> ppr t
ppr_dec _ (KiSigD f k)  = text "type" <+> pprPrefixOcc f <+> dcolon <+> ppr k
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
ppr_dec isTop (DataInstD ctxt bndrs ty ksig cs decs)
  = ppr_data (maybeInst <+> ppr_bndrs bndrs)
             ctxt Nothing (ppr ty) ksig cs decs
  where
    maybeInst | isTop     = text "instance"
              | otherwise = empty
ppr_dec isTop (NewtypeInstD ctxt bndrs ty ksig c decs)
  = ppr_newtype (maybeInst <+> ppr_bndrs bndrs)
                ctxt Nothing (ppr ty) ksig c decs
  where
    maybeInst | isTop     = text "instance"
              | otherwise = empty
ppr_dec isTop (TySynInstD (TySynEqn mb_bndrs ty rhs))
  = ppr_tySyn (maybeInst <+> ppr_bndrs mb_bndrs)
              Nothing (ppr ty) rhs
  where
    maybeInst | isTop     = text "instance"
              | otherwise = empty
ppr_dec isTop (OpenTypeFamilyD tfhead)
  = text "type" <+> maybeFamily <+> ppr_tf_head tfhead
  where
    maybeFamily | isTop     = text "family"
                | otherwise = empty
ppr_dec _ (ClosedTypeFamilyD tfhead eqns)
  = hang (text "type family" <+> ppr_tf_head tfhead <+> text "where")
      nestDepth (vcat (map ppr_eqn eqns))
  where
    ppr_eqn (TySynEqn mb_bndrs lhs rhs)
      = ppr_bndrs mb_bndrs <+> ppr lhs <+> text "=" <+> ppr rhs
ppr_dec _ (RoleAnnotD name roles)
  = hsep [ text "type role", ppr name ] <+> hsep (map ppr roles)
ppr_dec _ (StandaloneDerivD ds cxt ty)
  = hsep [ text "deriving"
         , maybe empty ppr_deriv_strategy ds
         , text "instance"
         , pprCxt cxt
         , ppr ty ]
ppr_dec _ (DefaultSigD n ty)
  = hsep [ text "default", pprPrefixOcc n, dcolon, ppr ty ]
ppr_dec _ (PatSynD name args dir pat)
  = text "pattern" <+> pprNameArgs <+> ppr dir <+> pprPatRHS
  where
    pprNameArgs | InfixPatSyn a1 a2 <- args = ppr a1 <+> pprName' Infix name <+> ppr a2
                | otherwise                 = pprName' Applied name <+> ppr args
    pprPatRHS   | ExplBidir cls <- dir = hang (ppr pat <+> text "where")
                                           nestDepth (pprName' Applied name <+> ppr cls)
                | otherwise            = ppr pat
ppr_dec _ (PatSynSigD name ty)
  = pprPatSynSig name ty
ppr_dec _ (ImplicitParamBindD n e)
  = hsep [text ('?' : n), text "=", ppr e]

ppr_deriv_strategy :: DerivStrategy -> Doc
ppr_deriv_strategy ds =
  case ds of
    StockStrategy    -> text "stock"
    AnyclassStrategy -> text "anyclass"
    NewtypeStrategy  -> text "newtype"
    ViaStrategy ty   -> text "via" <+> pprParendType ty

ppr_overlap :: Overlap -> Doc
ppr_overlap o = text $
  case o of
    Overlaps      -> "{-# OVERLAPS #-}"
    Overlappable  -> "{-# OVERLAPPABLE #-}"
    Overlapping   -> "{-# OVERLAPPING #-}"
    Incoherent    -> "{-# INCOHERENT #-}"

ppr_data :: Doc -> Cxt -> Maybe Name -> Doc -> Maybe Kind -> [Con] -> [DerivClause]
         -> Doc
ppr_data maybeInst ctxt t argsDoc ksig cs decs
  = sep [text "data" <+> maybeInst
            <+> pprCxt ctxt
            <+> case t of
                 Just n -> pprName' Applied n <+> argsDoc
                 Nothing -> argsDoc
            <+> ksigDoc <+> maybeWhere,
         nest nestDepth (sep (pref $ map ppr cs)),
         if null decs
           then empty
           else nest nestDepth
              $ vcat $ map ppr_deriv_clause decs]
  where
    pref :: [Doc] -> [Doc]
    pref xs | isGadtDecl = xs
    pref []              = []      -- No constructors; can't happen in H98
    pref (d:ds)          = (char '=' <+> d):map (bar <+>) ds

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

ppr_newtype :: Doc -> Cxt -> Maybe Name -> Doc -> Maybe Kind -> Con -> [DerivClause]
            -> Doc
ppr_newtype maybeInst ctxt t argsDoc ksig c decs
  = sep [text "newtype" <+> maybeInst
            <+> pprCxt ctxt
            <+> case t of
                 Just n -> ppr n <+> argsDoc
                 Nothing -> argsDoc
            <+> ksigDoc,
         nest 2 (char '=' <+> ppr c),
         if null decs
           then empty
           else nest nestDepth
                $ vcat $ map ppr_deriv_clause decs]
  where
    ksigDoc = case ksig of
                Nothing -> empty
                Just k  -> dcolon <+> ppr k

ppr_deriv_clause :: DerivClause -> Doc
ppr_deriv_clause (DerivClause ds ctxt)
  = text "deriving" <+> pp_strat_before
                    <+> ppr_cxt_preds ctxt
                    <+> pp_strat_after
  where
    -- @via@ is unique in that in comes /after/ the class being derived,
    -- so we must special-case it.
    (pp_strat_before, pp_strat_after) =
      case ds of
        Just (via@ViaStrategy{}) -> (empty, ppr_deriv_strategy via)
        _                        -> (maybe empty ppr_deriv_strategy ds, empty)

ppr_tySyn :: Doc -> Maybe Name -> Doc -> Type -> Doc
ppr_tySyn maybeInst t argsDoc rhs
  = text "type" <+> maybeInst
    <+> case t of
         Just n -> pprName' Applied n <+> argsDoc
         Nothing -> argsDoc
    <+> text "=" <+> ppr rhs

ppr_tf_head :: TypeFamilyHead -> Doc
ppr_tf_head (TypeFamilyHead tc tvs res inj)
  = pprName' Applied tc <+> hsep (map ppr tvs) <+> ppr res <+> maybeInj
  where
    maybeInj | (Just inj') <- inj = ppr inj'
             | otherwise          = empty

ppr_bndrs :: PprFlag flag => Maybe [TyVarBndr flag] -> Doc
ppr_bndrs (Just bndrs) = text "forall" <+> sep (map ppr bndrs) <> text "."
ppr_bndrs Nothing = empty

------------------------------
instance Ppr FunDep where
    ppr (FunDep xs ys) = hsep (map ppr xs) <+> text "->" <+> hsep (map ppr ys)
    ppr_list [] = empty
    ppr_list xs = bar <+> commaSep xs

------------------------------
instance Ppr FamilyResultSig where
    ppr NoSig           = empty
    ppr (KindSig k)     = dcolon <+> ppr k
    ppr (TyVarSig bndr) = text "=" <+> ppr bndr

------------------------------
instance Ppr InjectivityAnn where
    ppr (InjectivityAnn lhs rhs) =
        bar <+> ppr lhs <+> text "->" <+> hsep (map ppr rhs)

------------------------------
instance Ppr Foreign where
    ppr (ImportF callconv safety impent as typ)
       = text "foreign import"
     <+> showtextl callconv
     <+> showtextl safety
     <+> text (show impent)
     <+> pprName' Applied as
     <+> dcolon <+> ppr typ
    ppr (ExportF callconv expent as typ)
        = text "foreign export"
      <+> showtextl callconv
      <+> text (show expent)
      <+> pprName' Applied as
      <+> dcolon <+> ppr typ

------------------------------
instance Ppr Pragma where
    ppr (InlineP n inline rm phases)
       = text "{-#"
     <+> ppr inline
     <+> ppr rm
     <+> ppr phases
     <+> pprName' Applied n
     <+> text "#-}"
    ppr (SpecialiseP n ty inline phases)
       =   text "{-# SPECIALISE"
       <+> maybe empty ppr inline
       <+> ppr phases
       <+> sep [ pprName' Applied n <+> dcolon
               , nest 2 $ ppr ty ]
       <+> text "#-}"
    ppr (SpecialiseInstP inst)
       = text "{-# SPECIALISE instance" <+> ppr inst <+> text "#-}"
    ppr (RuleP n ty_bndrs tm_bndrs lhs rhs phases)
       = sep [ text "{-# RULES" <+> pprString n <+> ppr phases
             , nest 4 $ ppr_ty_forall ty_bndrs <+> ppr_tm_forall ty_bndrs
                                               <+> ppr lhs
             , nest 4 $ char '=' <+> ppr rhs <+> text "#-}" ]
      where ppr_ty_forall Nothing      = empty
            ppr_ty_forall (Just bndrs) = text "forall"
                                         <+> fsep (map ppr bndrs)
                                         <+> char '.'
            ppr_tm_forall Nothing | null tm_bndrs = empty
            ppr_tm_forall _ = text "forall"
                              <+> fsep (map ppr tm_bndrs)
                              <+> char '.'
    ppr (AnnP tgt expr)
       = text "{-# ANN" <+> target1 tgt <+> ppr expr <+> text "#-}"
      where target1 ModuleAnnotation    = text "module"
            target1 (TypeAnnotation t)  = text "type" <+> pprName' Applied t
            target1 (ValueAnnotation v) = pprName' Applied v
    ppr (LineP line file)
       = text "{-# LINE" <+> int line <+> text (show file) <+> text "#-}"
    ppr (CompleteP cls mty)
       = text "{-# COMPLETE" <+> (fsep $ punctuate comma $ map (pprName' Applied) cls)
                <+> maybe empty (\ty -> dcolon <+> pprName' Applied ty) mty <+> text "#-}"

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
    ppr (NormalC c sts) = pprName' Applied c <+> sep (map pprBangType sts)

    ppr (RecC c vsts)
        = pprName' Applied c <+> braces (sep (punctuate comma $ map pprVarBangType vsts))

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

instance Ppr PatSynDir where
  ppr Unidir        = text "<-"
  ppr ImplBidir     = text "="
  ppr (ExplBidir _) = text "<-"
    -- the ExplBidir's clauses are pretty printed together with the
    -- entire pattern synonym; so only print the direction here.

instance Ppr PatSynArgs where
  ppr (PrefixPatSyn args) = sep $ map ppr args
  ppr (InfixPatSyn a1 a2) = ppr a1 <+> ppr a2
  ppr (RecordPatSyn sels) = braces $ sep (punctuate comma (map (pprName' Applied) sels))

commaSepApplied :: [Name] -> Doc
commaSepApplied = commaSepWith (pprName' Applied)

pprForall :: [TyVarBndr Specificity] -> Cxt -> Doc
pprForall = pprForall' ForallInvis

pprForallVis :: [TyVarBndr ()] -> Cxt -> Doc
pprForallVis = pprForall' ForallVis

pprForall' :: PprFlag flag => ForallVisFlag -> [TyVarBndr flag] -> Cxt -> Doc
pprForall' fvf tvs cxt
  -- even in the case without any tvs, there could be a non-empty
  -- context cxt (e.g., in the case of pattern synonyms, where there
  -- are multiple forall binders and contexts).
  | [] <- tvs = pprCxt cxt
  | otherwise = text "forall" <+> hsep (map ppr tvs)
                              <+> separator <+> pprCxt cxt
  where
    separator = case fvf of
                  ForallVis   -> text "->"
                  ForallInvis -> char '.'

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
pprVarBangType (v, bang, t) = pprName' Applied v <+> dcolon <+> pprBangType (bang, t)

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
pprParendType (VarT v)            = pprName' Applied v
-- `Applied` is used here instead of `ppr` because of infix names (#13887)
pprParendType (ConT c)            = pprName' Applied c
pprParendType (TupleT 0)          = text "()"
pprParendType (TupleT 1)          = pprParendType (ConT (tupleTypeName 1))
pprParendType (TupleT n)          = parens (hcat (replicate (n-1) comma))
pprParendType (UnboxedTupleT n)   = hashParens $ hcat $ replicate (n-1) comma
pprParendType (UnboxedSumT arity) = hashParens $ hcat $ replicate (arity-1) bar
pprParendType ArrowT              = parens (text "->")
pprParendType MulArrowT           = text "FUN"
pprParendType ListT               = text "[]"
pprParendType (LitT l)            = pprTyLit l
pprParendType (PromotedT c)       = text "'" <> pprName' Applied c
pprParendType (PromotedTupleT 0)  = text "'()"
pprParendType (PromotedTupleT 1)  = pprParendType (PromotedT (tupleDataName 1))
pprParendType (PromotedTupleT n)  = quoteParens (hcat (replicate (n-1) comma))
pprParendType PromotedNilT        = text "'[]"
pprParendType PromotedConsT       = text "'(:)"
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
pprParendType (ImplicitParamT n t)= text ('?':n) <+> text "::" <+> ppr t
pprParendType EqualityT           = text "(~)"
pprParendType t@(ForallT {})      = parens (ppr t)
pprParendType t@(ForallVisT {})   = parens (ppr t)
pprParendType t@(AppT {})         = parens (ppr t)
pprParendType t@(AppKindT {})     = parens (ppr t)

pprUInfixT :: Type -> Doc
pprUInfixT (UInfixT x n y) = pprUInfixT x <+> pprName' Infix n <+> pprUInfixT y
pprUInfixT t               = ppr t

instance Ppr Type where
    ppr (ForallT tvars ctxt ty) = sep [pprForall tvars ctxt, ppr ty]
    ppr (ForallVisT tvars ty)   = sep [pprForallVis tvars [], ppr ty]
    ppr ty = pprTyApp (split ty)
       -- Works, in a degenerate way, for SigT, and puts parens round (ty :: kind)
       -- See Note [Pretty-printing kind signatures]
instance Ppr TypeArg where
    ppr (TANormal ty) = parensIf (isStarT ty) (ppr ty)
    ppr (TyArg ki) = char '@' <> parensIf (isStarT ki) (ppr ki)

pprParendTypeArg :: TypeArg -> Doc
pprParendTypeArg (TANormal ty) = parensIf (isStarT ty) (pprParendType ty)
pprParendTypeArg (TyArg ki) = char '@' <> parensIf (isStarT ki) (pprParendType ki)

isStarT :: Type -> Bool
isStarT StarT = True
isStarT _ = False

{- Note [Pretty-printing kind signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC's parser only recognises a kind signature in a type when there are
parens around it.  E.g. the parens are required here:
   f :: (Int :: *)
   type instance F Int = (Bool :: *)
So we always print a SigT with parens (see #10050). -}

pprTyApp :: (Type, [TypeArg]) -> Doc
pprTyApp (MulArrowT, [TANormal (PromotedT c), TANormal arg1, TANormal arg2])
  | c == oneName  = sep [pprFunArgType arg1 <+> text "%1 ->", ppr arg2]
  | c == manyName = sep [pprFunArgType arg1 <+> text "->", ppr arg2]
pprTyApp (MulArrowT, [TANormal argm, TANormal arg1, TANormal arg2]) =
                     sep [pprFunArgType arg1 <+> text "%" <> ppr argm <+> text "->", ppr arg2]
pprTyApp (ArrowT, [TANormal arg1, TANormal arg2]) = sep [pprFunArgType arg1 <+> text "->", ppr arg2]
pprTyApp (EqualityT, [TANormal arg1, TANormal arg2]) =
    sep [pprFunArgType arg1 <+> text "~", ppr arg2]
pprTyApp (ListT, [TANormal arg]) = brackets (ppr arg)
pprTyApp (TupleT 1, args) = pprTyApp (ConT (tupleTypeName 1), args)
pprTyApp (PromotedTupleT 1, args) = pprTyApp (PromotedT (tupleDataName 1), args)
pprTyApp (TupleT n, args)
 | length args == n, Just args' <- traverse fromTANormal args
 = parens (commaSep args')
pprTyApp (PromotedTupleT n, args)
 | length args == n, Just args' <- traverse fromTANormal args
 = quoteParens (commaSep args')
pprTyApp (fun, args) = pprParendType fun <+> sep (map pprParendTypeArg args)

fromTANormal :: TypeArg -> Maybe Type
fromTANormal (TANormal arg) = Just arg
fromTANormal (TyArg _) = Nothing

pprFunArgType :: Type -> Doc    -- Should really use a precedence argument
-- Everything except forall and (->) binds more tightly than (->)
pprFunArgType ty@(ForallT {})                 = parens (ppr ty)
pprFunArgType ty@(ForallVisT {})              = parens (ppr ty)
pprFunArgType ty@(((MulArrowT `AppT` _) `AppT` _) `AppT` _)  = parens (ppr ty)
pprFunArgType ty@((ArrowT `AppT` _) `AppT` _) = parens (ppr ty)
pprFunArgType ty@(SigT _ _)                   = parens (ppr ty)
pprFunArgType ty                              = ppr ty

data ForallVisFlag = ForallVis   -- forall a -> {...}
                   | ForallInvis -- forall a.   {...}
  deriving Show

data TypeArg = TANormal Type
             | TyArg Kind

split :: Type -> (Type, [TypeArg])    -- Split into function and args
split t = go t []
    where go (AppT t1 t2) args = go t1 (TANormal t2:args)
          go (AppKindT ty ki) args = go ty (TyArg ki:args)
          go ty           args = (ty, args)

pprTyLit :: TyLit -> Doc
pprTyLit (NumTyLit n) = integer n
pprTyLit (StrTyLit s) = text (show s)
pprTyLit (CharTyLit c) = text (show c)

instance Ppr TyLit where
  ppr = pprTyLit

------------------------------
class PprFlag flag where
    pprTyVarBndr :: (TyVarBndr flag) -> Doc

instance PprFlag () where
    pprTyVarBndr (PlainTV nm ())    = ppr nm
    pprTyVarBndr (KindedTV nm () k) = parens (ppr nm <+> dcolon <+> ppr k)

instance PprFlag Specificity where
    pprTyVarBndr (PlainTV nm SpecifiedSpec)    = ppr nm
    pprTyVarBndr (PlainTV nm InferredSpec)     = braces (ppr nm)
    pprTyVarBndr (KindedTV nm SpecifiedSpec k) = parens (ppr nm <+> dcolon <+> ppr k)
    pprTyVarBndr (KindedTV nm InferredSpec  k) = braces (ppr nm <+> dcolon <+> ppr k)

instance PprFlag flag => Ppr (TyVarBndr flag) where
    ppr bndr = pprTyVarBndr bndr

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
ppr_cxt_preds [t@ImplicitParamT{}] = parens (ppr t)
ppr_cxt_preds [t@ForallT{}] = parens (ppr t)
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

-- Prints out the series of vertical bars that wraps an expression or pattern
-- used in an unboxed sum.
unboxedSumBars :: Doc -> SumAlt -> SumArity -> Doc
unboxedSumBars d alt arity = hashParens $
    bars (alt-1) <> d <> bars (arity - alt)
  where
    bars i = hsep (replicate i bar)

-- Text containing the vertical bar character.
bar :: Doc
bar = char '|'
