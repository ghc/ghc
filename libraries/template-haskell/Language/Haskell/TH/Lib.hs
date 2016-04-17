-- |
-- TH.Lib contains lots of useful helper functions for
-- generating and manipulating Template Haskell terms

{-# LANGUAGE CPP #-}

module Language.Haskell.TH.Lib where
    -- All of the exports from this module should
    -- be "public" functions.  The main module TH
    -- re-exports them all.

import Language.Haskell.TH.Syntax hiding (Role, InjectivityAnn)
import qualified Language.Haskell.TH.Syntax as TH
import Control.Monad( liftM, liftM2 )
import Data.Word( Word8 )

----------------------------------------------------------
-- * Type synonyms
----------------------------------------------------------

type InfoQ               = Q Info
type PatQ                = Q Pat
type FieldPatQ           = Q FieldPat
type ExpQ                = Q Exp
type TExpQ a             = Q (TExp a)
type DecQ                = Q Dec
type DecsQ               = Q [Dec]
type ConQ                = Q Con
type TypeQ               = Q Type
type TyLitQ              = Q TyLit
type CxtQ                = Q Cxt
type PredQ               = Q Pred
type MatchQ              = Q Match
type ClauseQ             = Q Clause
type BodyQ               = Q Body
type GuardQ              = Q Guard
type StmtQ               = Q Stmt
type RangeQ              = Q Range
type SourceStrictnessQ   = Q SourceStrictness
type SourceUnpackednessQ = Q SourceUnpackedness
type BangQ               = Q Bang
type BangTypeQ           = Q BangType
type VarBangTypeQ        = Q VarBangType
type StrictTypeQ         = Q StrictType
type VarStrictTypeQ      = Q VarStrictType
type FieldExpQ           = Q FieldExp
type RuleBndrQ           = Q RuleBndr
type TySynEqnQ           = Q TySynEqn

-- must be defined here for DsMeta to find it
type Role                = TH.Role
type InjectivityAnn      = TH.InjectivityAnn

----------------------------------------------------------
-- * Lowercase pattern syntax functions
----------------------------------------------------------

intPrimL    :: Integer -> Lit
intPrimL    = IntPrimL
wordPrimL    :: Integer -> Lit
wordPrimL    = WordPrimL
floatPrimL  :: Rational -> Lit
floatPrimL  = FloatPrimL
doublePrimL :: Rational -> Lit
doublePrimL = DoublePrimL
integerL    :: Integer -> Lit
integerL    = IntegerL
charL       :: Char -> Lit
charL       = CharL
charPrimL   :: Char -> Lit
charPrimL   = CharPrimL
stringL     :: String -> Lit
stringL     = StringL
stringPrimL :: [Word8] -> Lit
stringPrimL = StringPrimL
rationalL   :: Rational -> Lit
rationalL   = RationalL

litP :: Lit -> PatQ
litP l = return (LitP l)
varP :: Name -> PatQ
varP v = return (VarP v)
tupP :: [PatQ] -> PatQ
tupP ps = do { ps1 <- sequence ps; return (TupP ps1)}
unboxedTupP :: [PatQ] -> PatQ
unboxedTupP ps = do { ps1 <- sequence ps; return (UnboxedTupP ps1)}
conP :: Name -> [PatQ] -> PatQ
conP n ps = do ps' <- sequence ps
               return (ConP n ps')
infixP :: PatQ -> Name -> PatQ -> PatQ
infixP p1 n p2 = do p1' <- p1
                    p2' <- p2
                    return (InfixP p1' n p2')
uInfixP :: PatQ -> Name -> PatQ -> PatQ
uInfixP p1 n p2 = do p1' <- p1
                     p2' <- p2
                     return (UInfixP p1' n p2')
parensP :: PatQ -> PatQ
parensP p = do p' <- p
               return (ParensP p')

tildeP :: PatQ -> PatQ
tildeP p = do p' <- p
              return (TildeP p')
bangP :: PatQ -> PatQ
bangP p = do p' <- p
             return (BangP p')
asP :: Name -> PatQ -> PatQ
asP n p = do p' <- p
             return (AsP n p')
wildP :: PatQ
wildP = return WildP
recP :: Name -> [FieldPatQ] -> PatQ
recP n fps = do fps' <- sequence fps
                return (RecP n fps')
listP :: [PatQ] -> PatQ
listP ps = do ps' <- sequence ps
              return (ListP ps')
sigP :: PatQ -> TypeQ -> PatQ
sigP p t = do p' <- p
              t' <- t
              return (SigP p' t')
viewP :: ExpQ -> PatQ -> PatQ
viewP e p = do e' <- e
               p' <- p
               return (ViewP e' p')

fieldPat :: Name -> PatQ -> FieldPatQ
fieldPat n p = do p' <- p
                  return (n, p')


-------------------------------------------------------------------------------
-- *   Stmt

bindS :: PatQ -> ExpQ -> StmtQ
bindS p e = liftM2 BindS p e

letS :: [DecQ] -> StmtQ
letS ds = do { ds1 <- sequence ds; return (LetS ds1) }

noBindS :: ExpQ -> StmtQ
noBindS e = do { e1 <- e; return (NoBindS e1) }

parS :: [[StmtQ]] -> StmtQ
parS sss = do { sss1 <- mapM sequence sss; return (ParS sss1) }

-------------------------------------------------------------------------------
-- *   Range

fromR :: ExpQ -> RangeQ
fromR x = do { a <- x; return (FromR a) }

fromThenR :: ExpQ -> ExpQ -> RangeQ
fromThenR x y = do { a <- x; b <- y; return (FromThenR a b) }

fromToR :: ExpQ -> ExpQ -> RangeQ
fromToR x y = do { a <- x; b <- y; return (FromToR a b) }

fromThenToR :: ExpQ -> ExpQ -> ExpQ -> RangeQ
fromThenToR x y z = do { a <- x; b <- y; c <- z;
                         return (FromThenToR a b c) }
-------------------------------------------------------------------------------
-- *   Body

normalB :: ExpQ -> BodyQ
normalB e = do { e1 <- e; return (NormalB e1) }

guardedB :: [Q (Guard,Exp)] -> BodyQ
guardedB ges = do { ges' <- sequence ges; return (GuardedB ges') }

-------------------------------------------------------------------------------
-- *   Guard

normalG :: ExpQ -> GuardQ
normalG e = do { e1 <- e; return (NormalG e1) }

normalGE :: ExpQ -> ExpQ -> Q (Guard, Exp)
normalGE g e = do { g1 <- g; e1 <- e; return (NormalG g1, e1) }

patG :: [StmtQ] -> GuardQ
patG ss = do { ss' <- sequence ss; return (PatG ss') }

patGE :: [StmtQ] -> ExpQ -> Q (Guard, Exp)
patGE ss e = do { ss' <- sequence ss;
                  e'  <- e;
                  return (PatG ss', e') }

-------------------------------------------------------------------------------
-- *   Match and Clause

-- | Use with 'caseE'
match :: PatQ -> BodyQ -> [DecQ] -> MatchQ
match p rhs ds = do { p' <- p;
                      r' <- rhs;
                      ds' <- sequence ds;
                      return (Match p' r' ds') }

-- | Use with 'funD'
clause :: [PatQ] -> BodyQ -> [DecQ] -> ClauseQ
clause ps r ds = do { ps' <- sequence ps;
                      r' <- r;
                      ds' <- sequence ds;
                      return (Clause ps' r' ds') }


---------------------------------------------------------------------------
-- *   Exp

-- | Dynamically binding a variable (unhygenic)
dyn :: String -> ExpQ
dyn s = return (VarE (mkName s))

varE :: Name -> ExpQ
varE s = return (VarE s)

conE :: Name -> ExpQ
conE s =  return (ConE s)

litE :: Lit -> ExpQ
litE c = return (LitE c)

appE :: ExpQ -> ExpQ -> ExpQ
appE x y = do { a <- x; b <- y; return (AppE a b)}

parensE :: ExpQ -> ExpQ
parensE x = do { x' <- x; return (ParensE x') }

uInfixE :: ExpQ -> ExpQ -> ExpQ -> ExpQ
uInfixE x s y = do { x' <- x; s' <- s; y' <- y;
                     return (UInfixE x' s' y') }

infixE :: Maybe ExpQ -> ExpQ -> Maybe ExpQ -> ExpQ
infixE (Just x) s (Just y) = do { a <- x; s' <- s; b <- y;
                                  return (InfixE (Just a) s' (Just b))}
infixE Nothing  s (Just y) = do { s' <- s; b <- y;
                                  return (InfixE Nothing s' (Just b))}
infixE (Just x) s Nothing  = do { a <- x; s' <- s;
                                  return (InfixE (Just a) s' Nothing)}
infixE Nothing  s Nothing  = do { s' <- s; return (InfixE Nothing s' Nothing) }

infixApp :: ExpQ -> ExpQ -> ExpQ -> ExpQ
infixApp x y z = infixE (Just x) y (Just z)
sectionL :: ExpQ -> ExpQ -> ExpQ
sectionL x y = infixE (Just x) y Nothing
sectionR :: ExpQ -> ExpQ -> ExpQ
sectionR x y = infixE Nothing x (Just y)

lamE :: [PatQ] -> ExpQ -> ExpQ
lamE ps e = do ps' <- sequence ps
               e' <- e
               return (LamE ps' e')

-- | Single-arg lambda
lam1E :: PatQ -> ExpQ -> ExpQ
lam1E p e = lamE [p] e

lamCaseE :: [MatchQ] -> ExpQ
lamCaseE ms = sequence ms >>= return . LamCaseE

tupE :: [ExpQ] -> ExpQ
tupE es = do { es1 <- sequence es; return (TupE es1)}

unboxedTupE :: [ExpQ] -> ExpQ
unboxedTupE es = do { es1 <- sequence es; return (UnboxedTupE es1)}

condE :: ExpQ -> ExpQ -> ExpQ -> ExpQ
condE x y z =  do { a <- x; b <- y; c <- z; return (CondE a b c)}

multiIfE :: [Q (Guard, Exp)] -> ExpQ
multiIfE alts = sequence alts >>= return . MultiIfE

letE :: [DecQ] -> ExpQ -> ExpQ
letE ds e = do { ds2 <- sequence ds; e2 <- e; return (LetE ds2 e2) }

caseE :: ExpQ -> [MatchQ] -> ExpQ
caseE e ms = do { e1 <- e; ms1 <- sequence ms; return (CaseE e1 ms1) }

doE :: [StmtQ] -> ExpQ
doE ss = do { ss1 <- sequence ss; return (DoE ss1) }

compE :: [StmtQ] -> ExpQ
compE ss = do { ss1 <- sequence ss; return (CompE ss1) }

arithSeqE :: RangeQ -> ExpQ
arithSeqE r = do { r' <- r; return (ArithSeqE r') }

listE :: [ExpQ] -> ExpQ
listE es = do { es1 <- sequence es; return (ListE es1) }

sigE :: ExpQ -> TypeQ -> ExpQ
sigE e t = do { e1 <- e; t1 <- t; return (SigE e1 t1) }

recConE :: Name -> [Q (Name,Exp)] -> ExpQ
recConE c fs = do { flds <- sequence fs; return (RecConE c flds) }

recUpdE :: ExpQ -> [Q (Name,Exp)] -> ExpQ
recUpdE e fs = do { e1 <- e; flds <- sequence fs; return (RecUpdE e1 flds) }

stringE :: String -> ExpQ
stringE = litE . stringL

fieldExp :: Name -> ExpQ -> Q (Name, Exp)
fieldExp s e = do { e' <- e; return (s,e') }

-- | @staticE x = [| static x |]@
staticE :: ExpQ -> ExpQ
staticE = fmap StaticE

unboundVarE :: Name -> ExpQ
unboundVarE s = return (UnboundVarE s)

-- ** 'arithSeqE' Shortcuts
fromE :: ExpQ -> ExpQ
fromE x = do { a <- x; return (ArithSeqE (FromR a)) }

fromThenE :: ExpQ -> ExpQ -> ExpQ
fromThenE x y = do { a <- x; b <- y; return (ArithSeqE (FromThenR a b)) }

fromToE :: ExpQ -> ExpQ -> ExpQ
fromToE x y = do { a <- x; b <- y; return (ArithSeqE (FromToR a b)) }

fromThenToE :: ExpQ -> ExpQ -> ExpQ -> ExpQ
fromThenToE x y z = do { a <- x; b <- y; c <- z;
                         return (ArithSeqE (FromThenToR a b c)) }


-------------------------------------------------------------------------------
-- *   Dec

valD :: PatQ -> BodyQ -> [DecQ] -> DecQ
valD p b ds =
  do { p' <- p
     ; ds' <- sequence ds
     ; b' <- b
     ; return (ValD p' b' ds')
     }

funD :: Name -> [ClauseQ] -> DecQ
funD nm cs =
 do { cs1 <- sequence cs
    ; return (FunD nm cs1)
    }

tySynD :: Name -> [TyVarBndr] -> TypeQ -> DecQ
tySynD tc tvs rhs = do { rhs1 <- rhs; return (TySynD tc tvs rhs1) }

dataD :: CxtQ -> Name -> [TyVarBndr] -> Maybe Kind -> [ConQ] -> CxtQ -> DecQ
dataD ctxt tc tvs ksig cons derivs =
  do
    ctxt1 <- ctxt
    cons1 <- sequence cons
    derivs1 <- derivs
    return (DataD ctxt1 tc tvs ksig cons1 derivs1)

newtypeD :: CxtQ -> Name -> [TyVarBndr] -> Maybe Kind -> ConQ -> CxtQ -> DecQ
newtypeD ctxt tc tvs ksig con derivs =
  do
    ctxt1 <- ctxt
    con1 <- con
    derivs1 <- derivs
    return (NewtypeD ctxt1 tc tvs ksig con1 derivs1)

classD :: CxtQ -> Name -> [TyVarBndr] -> [FunDep] -> [DecQ] -> DecQ
classD ctxt cls tvs fds decs =
  do
    decs1 <- sequence decs
    ctxt1 <- ctxt
    return $ ClassD ctxt1 cls tvs fds decs1

instanceD :: CxtQ -> TypeQ -> [DecQ] -> DecQ
instanceD = instanceWithOverlapD Nothing

instanceWithOverlapD :: Maybe Overlap -> CxtQ -> TypeQ -> [DecQ] -> DecQ
instanceWithOverlapD o ctxt ty decs =
  do
    ctxt1 <- ctxt
    decs1 <- sequence decs
    ty1   <- ty
    return $ InstanceD o ctxt1 ty1 decs1



sigD :: Name -> TypeQ -> DecQ
sigD fun ty = liftM (SigD fun) $ ty

forImpD :: Callconv -> Safety -> String -> Name -> TypeQ -> DecQ
forImpD cc s str n ty
 = do ty' <- ty
      return $ ForeignD (ImportF cc s str n ty')

infixLD :: Int -> Name -> DecQ
infixLD prec nm = return (InfixD (Fixity prec InfixL) nm)

infixRD :: Int -> Name -> DecQ
infixRD prec nm = return (InfixD (Fixity prec InfixR) nm)

infixND :: Int -> Name -> DecQ
infixND prec nm = return (InfixD (Fixity prec InfixN) nm)

pragInlD :: Name -> Inline -> RuleMatch -> Phases -> DecQ
pragInlD name inline rm phases
  = return $ PragmaD $ InlineP name inline rm phases

pragSpecD :: Name -> TypeQ -> Phases -> DecQ
pragSpecD n ty phases
  = do
      ty1    <- ty
      return $ PragmaD $ SpecialiseP n ty1 Nothing phases

pragSpecInlD :: Name -> TypeQ -> Inline -> Phases -> DecQ
pragSpecInlD n ty inline phases
  = do
      ty1    <- ty
      return $ PragmaD $ SpecialiseP n ty1 (Just inline) phases

pragSpecInstD :: TypeQ -> DecQ
pragSpecInstD ty
  = do
      ty1    <- ty
      return $ PragmaD $ SpecialiseInstP ty1

pragRuleD :: String -> [RuleBndrQ] -> ExpQ -> ExpQ -> Phases -> DecQ
pragRuleD n bndrs lhs rhs phases
  = do
      bndrs1 <- sequence bndrs
      lhs1   <- lhs
      rhs1   <- rhs
      return $ PragmaD $ RuleP n bndrs1 lhs1 rhs1 phases

pragAnnD :: AnnTarget -> ExpQ -> DecQ
pragAnnD target expr
  = do
      exp1 <- expr
      return $ PragmaD $ AnnP target exp1

pragLineD :: Int -> String -> DecQ
pragLineD line file = return $ PragmaD $ LineP line file

dataInstD :: CxtQ -> Name -> [TypeQ] -> Maybe Kind -> [ConQ] -> CxtQ -> DecQ
dataInstD ctxt tc tys ksig cons derivs =
  do
    ctxt1 <- ctxt
    tys1  <- sequence tys
    cons1 <- sequence cons
    derivs1 <- derivs
    return (DataInstD ctxt1 tc tys1 ksig cons1 derivs1)

newtypeInstD :: CxtQ -> Name -> [TypeQ] -> Maybe Kind -> ConQ -> CxtQ -> DecQ
newtypeInstD ctxt tc tys ksig con derivs =
  do
    ctxt1 <- ctxt
    tys1  <- sequence tys
    con1  <- con
    derivs1 <- derivs
    return (NewtypeInstD ctxt1 tc tys1 ksig con1 derivs1)

tySynInstD :: Name -> TySynEqnQ -> DecQ
tySynInstD tc eqn =
  do
    eqn1 <- eqn
    return (TySynInstD tc eqn1)

dataFamilyD :: Name -> [TyVarBndr] -> Maybe Kind -> DecQ
dataFamilyD tc tvs kind
    = return $ DataFamilyD tc tvs kind

openTypeFamilyD :: Name -> [TyVarBndr] -> FamilyResultSig
                -> Maybe InjectivityAnn -> DecQ
openTypeFamilyD tc tvs res inj
    = return $ OpenTypeFamilyD (TypeFamilyHead tc tvs res inj)

closedTypeFamilyD :: Name -> [TyVarBndr] -> FamilyResultSig
                  -> Maybe InjectivityAnn -> [TySynEqnQ] -> DecQ
closedTypeFamilyD tc tvs result injectivity eqns =
  do eqns1 <- sequence eqns
     return (ClosedTypeFamilyD (TypeFamilyHead tc tvs result injectivity) eqns1)

-- These were deprecated in GHC 8.0 with a plan to remove them in 8.2. If you
-- remove this check please also:
--   1. remove deprecated functions
--   2. remove CPP language extension from top of this module
--   3. remove the FamFlavour data type from Syntax module
--   4. make sure that all references to FamFlavour are gone from DsMeta,
--      Convert, TcSplice (follows from 3)
#if __GLASGOW_HASKELL__ > 800
#error Remove deprecated familyNoKindD, familyKindD, closedTypeFamilyNoKindD and closedTypeFamilyKindD
#endif

{-# DEPRECATED familyNoKindD, familyKindD
               "This function will be removed in the next stable release. Use openTypeFamilyD/dataFamilyD instead." #-}
familyNoKindD :: FamFlavour -> Name -> [TyVarBndr] -> DecQ
familyNoKindD flav tc tvs =
    case flav of
      TypeFam -> return $ OpenTypeFamilyD (TypeFamilyHead tc tvs NoSig Nothing)
      DataFam -> return $ DataFamilyD tc tvs Nothing

familyKindD :: FamFlavour -> Name -> [TyVarBndr] -> Kind -> DecQ
familyKindD flav tc tvs k =
    case flav of
      TypeFam ->
        return $ OpenTypeFamilyD (TypeFamilyHead tc tvs (KindSig k) Nothing)
      DataFam -> return $ DataFamilyD tc tvs (Just k)

{-# DEPRECATED closedTypeFamilyNoKindD, closedTypeFamilyKindD
               "This function will be removed in the next stable release. Use closedTypeFamilyD instead." #-}
closedTypeFamilyNoKindD :: Name -> [TyVarBndr] -> [TySynEqnQ] -> DecQ
closedTypeFamilyNoKindD tc tvs eqns =
 do eqns1 <- sequence eqns
    return (ClosedTypeFamilyD (TypeFamilyHead tc tvs NoSig Nothing) eqns1)

closedTypeFamilyKindD :: Name -> [TyVarBndr] -> Kind -> [TySynEqnQ] -> DecQ
closedTypeFamilyKindD tc tvs kind eqns =
 do eqns1 <- sequence eqns
    return (ClosedTypeFamilyD (TypeFamilyHead tc tvs (KindSig kind) Nothing)
            eqns1)

roleAnnotD :: Name -> [Role] -> DecQ
roleAnnotD name roles = return $ RoleAnnotD name roles

standaloneDerivD :: CxtQ -> TypeQ -> DecQ
standaloneDerivD ctxtq tyq =
  do
    ctxt <- ctxtq
    ty   <- tyq
    return $ StandaloneDerivD ctxt ty

defaultSigD :: Name -> TypeQ -> DecQ
defaultSigD n tyq =
  do
    ty <- tyq
    return $ DefaultSigD n ty

tySynEqn :: [TypeQ] -> TypeQ -> TySynEqnQ
tySynEqn lhs rhs =
  do
    lhs1 <- sequence lhs
    rhs1 <- rhs
    return (TySynEqn lhs1 rhs1)

cxt :: [PredQ] -> CxtQ
cxt = sequence

normalC :: Name -> [BangTypeQ] -> ConQ
normalC con strtys = liftM (NormalC con) $ sequence strtys

recC :: Name -> [VarBangTypeQ] -> ConQ
recC con varstrtys = liftM (RecC con) $ sequence varstrtys

infixC :: Q (Bang, Type) -> Name -> Q (Bang, Type) -> ConQ
infixC st1 con st2 = do st1' <- st1
                        st2' <- st2
                        return $ InfixC st1' con st2'

forallC :: [TyVarBndr] -> CxtQ -> ConQ -> ConQ
forallC ns ctxt con = liftM2 (ForallC ns) ctxt con

gadtC :: [Name] -> [StrictTypeQ] -> TypeQ -> ConQ
gadtC cons strtys ty = liftM2 (GadtC cons) (sequence strtys) ty

recGadtC :: [Name] -> [VarStrictTypeQ] -> TypeQ -> ConQ
recGadtC cons varstrtys ty = liftM2 (RecGadtC cons) (sequence varstrtys) ty

-------------------------------------------------------------------------------
-- *   Type

forallT :: [TyVarBndr] -> CxtQ -> TypeQ -> TypeQ
forallT tvars ctxt ty = do
    ctxt1 <- ctxt
    ty1   <- ty
    return $ ForallT tvars ctxt1 ty1

varT :: Name -> TypeQ
varT = return . VarT

conT :: Name -> TypeQ
conT = return . ConT

infixT :: TypeQ -> Name -> TypeQ -> TypeQ
infixT t1 n t2 = do t1' <- t1
                    t2' <- t2
                    return (InfixT t1' n t2')

uInfixT :: TypeQ -> Name -> TypeQ -> TypeQ
uInfixT t1 n t2 = do t1' <- t1
                     t2' <- t2
                     return (UInfixT t1' n t2')

parensT :: TypeQ -> TypeQ
parensT t = do t' <- t
               return (ParensT t')

appT :: TypeQ -> TypeQ -> TypeQ
appT t1 t2 = do
           t1' <- t1
           t2' <- t2
           return $ AppT t1' t2'

arrowT :: TypeQ
arrowT = return ArrowT

listT :: TypeQ
listT = return ListT

litT :: TyLitQ -> TypeQ
litT l = fmap LitT l

tupleT :: Int -> TypeQ
tupleT i = return (TupleT i)

unboxedTupleT :: Int -> TypeQ
unboxedTupleT i = return (UnboxedTupleT i)

sigT :: TypeQ -> Kind -> TypeQ
sigT t k
  = do
      t' <- t
      return $ SigT t' k

equalityT :: TypeQ
equalityT = return EqualityT

wildCardT :: TypeQ
wildCardT = return WildCardT

{-# DEPRECATED classP "As of template-haskell-2.10, constraint predicates (Pred) are just types (Type), in keeping with ConstraintKinds. Please use 'conT' and 'appT'." #-}
classP :: Name -> [Q Type] -> Q Pred
classP cla tys
  = do
      tysl <- sequence tys
      return (foldl AppT (ConT cla) tysl)

{-# DEPRECATED equalP "As of template-haskell-2.10, constraint predicates (Pred) are just types (Type), in keeping with ConstraintKinds. Please see 'equalityT'." #-}
equalP :: TypeQ -> TypeQ -> PredQ
equalP tleft tright
  = do
      tleft1  <- tleft
      tright1 <- tright
      eqT <- equalityT
      return (foldl AppT eqT [tleft1, tright1])

promotedT :: Name -> TypeQ
promotedT = return . PromotedT

promotedTupleT :: Int -> TypeQ
promotedTupleT i = return (PromotedTupleT i)

promotedNilT :: TypeQ
promotedNilT = return PromotedNilT

promotedConsT :: TypeQ
promotedConsT = return PromotedConsT

noSourceUnpackedness, sourceNoUnpack, sourceUnpack :: SourceUnpackednessQ
noSourceUnpackedness = return NoSourceUnpackedness
sourceNoUnpack       = return SourceNoUnpack
sourceUnpack         = return SourceUnpack

noSourceStrictness, sourceLazy, sourceStrict :: SourceStrictnessQ
noSourceStrictness = return NoSourceStrictness
sourceLazy         = return SourceLazy
sourceStrict       = return SourceStrict

{-# DEPRECATED isStrict
    ["Use 'bang'. See https://ghc.haskell.org/trac/ghc/wiki/Migration/8.0. ",
     "Example usage: 'bang noSourceUnpackedness sourceStrict'"] #-}
{-# DEPRECATED notStrict
    ["Use 'bang'. See https://ghc.haskell.org/trac/ghc/wiki/Migration/8.0. ",
     "Example usage: 'bang noSourceUnpackedness noSourceStrictness'"] #-}
{-# DEPRECATED unpacked
    ["Use 'bang'. See https://ghc.haskell.org/trac/ghc/wiki/Migration/8.0. ",
     "Example usage: 'bang sourceUnpack sourceStrict'"] #-}
isStrict, notStrict, unpacked :: Q Strict
isStrict = bang noSourceUnpackedness sourceStrict
notStrict = bang noSourceUnpackedness noSourceStrictness
unpacked = bang sourceUnpack sourceStrict

bang :: SourceUnpackednessQ -> SourceStrictnessQ -> BangQ
bang u s = do u' <- u
              s' <- s
              return (Bang u' s')

bangType :: BangQ -> TypeQ -> BangTypeQ
bangType = liftM2 (,)

varBangType :: Name -> BangTypeQ -> VarBangTypeQ
varBangType v bt = do (b, t) <- bt
                      return (v, b, t)

{-# DEPRECATED strictType
               "As of @template-haskell-2.11.0.0@, 'StrictType' has been replaced by 'BangType'. Please use 'bangType' instead." #-}
strictType :: Q Strict -> TypeQ -> StrictTypeQ
strictType = bangType

{-# DEPRECATED varStrictType
               "As of @template-haskell-2.11.0.0@, 'VarStrictType' has been replaced by 'VarBangType'. Please use 'varBangType' instead." #-}
varStrictType :: Name -> StrictTypeQ -> VarStrictTypeQ
varStrictType = varBangType

-- * Type Literals

numTyLit :: Integer -> TyLitQ
numTyLit n = if n >= 0 then return (NumTyLit n)
                       else fail ("Negative type-level number: " ++ show n)

strTyLit :: String -> TyLitQ
strTyLit s = return (StrTyLit s)



-------------------------------------------------------------------------------
-- *   Kind

plainTV :: Name -> TyVarBndr
plainTV = PlainTV

kindedTV :: Name -> Kind -> TyVarBndr
kindedTV = KindedTV

varK :: Name -> Kind
varK = VarT

conK :: Name -> Kind
conK = ConT

tupleK :: Int -> Kind
tupleK = TupleT

arrowK :: Kind
arrowK = ArrowT

listK :: Kind
listK = ListT

appK :: Kind -> Kind -> Kind
appK = AppT

starK :: Kind
starK = StarT

constraintK :: Kind
constraintK = ConstraintT

-------------------------------------------------------------------------------
-- *   Type family result

noSig :: FamilyResultSig
noSig = NoSig

kindSig :: Kind -> FamilyResultSig
kindSig = KindSig

tyVarSig :: TyVarBndr -> FamilyResultSig
tyVarSig = TyVarSig

-------------------------------------------------------------------------------
-- *   Injectivity annotation

injectivityAnn :: Name -> [Name] -> InjectivityAnn
injectivityAnn = TH.InjectivityAnn

-------------------------------------------------------------------------------
-- *   Role

nominalR, representationalR, phantomR, inferR :: Role
nominalR          = NominalR
representationalR = RepresentationalR
phantomR          = PhantomR
inferR            = InferR

-------------------------------------------------------------------------------
-- *   Callconv

cCall, stdCall, cApi, prim, javaScript :: Callconv
cCall      = CCall
stdCall    = StdCall
cApi       = CApi
prim       = Prim
javaScript = JavaScript

-------------------------------------------------------------------------------
-- *   Safety

unsafe, safe, interruptible :: Safety
unsafe = Unsafe
safe = Safe
interruptible = Interruptible

-------------------------------------------------------------------------------
-- *   FunDep

funDep :: [Name] -> [Name] -> FunDep
funDep = FunDep

-------------------------------------------------------------------------------
-- *   FamFlavour

typeFam, dataFam :: FamFlavour
typeFam = TypeFam
dataFam = DataFam

-------------------------------------------------------------------------------
-- *   RuleBndr
ruleVar :: Name -> RuleBndrQ
ruleVar = return . RuleVar

typedRuleVar :: Name -> TypeQ -> RuleBndrQ
typedRuleVar n ty = ty >>= return . TypedRuleVar n

-------------------------------------------------------------------------------
-- *   AnnTarget
valueAnnotation :: Name -> AnnTarget
valueAnnotation = ValueAnnotation

typeAnnotation :: Name -> AnnTarget
typeAnnotation = TypeAnnotation

moduleAnnotation :: AnnTarget
moduleAnnotation = ModuleAnnotation

--------------------------------------------------------------
-- * Useful helper function

appsE :: [ExpQ] -> ExpQ
appsE [] = error "appsE []"
appsE [x] = x
appsE (x:y:zs) = appsE ( (appE x y) : zs )

-- | Return the Module at the place of splicing.  Can be used as an
-- input for 'reifyModule'.
thisModule :: Q Module
thisModule = do
  loc <- location
  return $ Module (mkPkgName $ loc_package loc) (mkModName $ loc_module loc)
