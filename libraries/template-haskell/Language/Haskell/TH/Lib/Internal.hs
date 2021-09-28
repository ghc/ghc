{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Language.Haskell.TH.Lib.Internal exposes some additional functionality that
-- is used internally in GHC's integration with Template Haskell. This is not a
-- part of the public API, and as such, there are no API guarantees for this
-- module from version to version.

-- Why do we have both Language.Haskell.TH.Lib.Internal and
-- Language.Haskell.TH.Lib? Ultimately, it's because the functions in the
-- former (which are tailored for GHC's use) need different type signatures
-- than the ones in the latter. Syncing up the Internal type signatures would
-- involve a massive amount of breaking changes, so for the time being, we
-- relegate as many changes as we can to just the Internal module, where it
-- is safe to break things.

module Language.Haskell.TH.Lib.Internal where

import Language.Haskell.TH.Syntax hiding (Role, InjectivityAnn)
import qualified Language.Haskell.TH.Syntax as TH
import Control.Applicative(liftA, liftA2)
import qualified Data.Kind as Kind (Type)
import Data.Word( Word8 )
import Data.List.NonEmpty ( NonEmpty(..) )
import GHC.Exts (TYPE)
import Prelude

----------------------------------------------------------
-- * Type synonyms
----------------------------------------------------------

-- | Levity-polymorphic since /template-haskell-2.17.0.0/.
type TExpQ :: TYPE r -> Kind.Type
type TExpQ a = Q (TExp a)

type CodeQ :: TYPE r -> Kind.Type
type CodeQ = Code Q

type InfoQ               = Q Info
type PatQ                = Q Pat
type FieldPatQ           = Q FieldPat
type ExpQ                = Q Exp
type DecQ                = Q Dec
type DecsQ               = Q [Dec]
type Decs                = [Dec] -- Defined as it is more convenient to wire-in
type ConQ                = Q Con
type TypeQ               = Q Type
type KindQ               = Q Kind
type TyLitQ              = Q TyLit
type CxtQ                = Q Cxt
type PredQ               = Q Pred
type DerivClauseQ        = Q DerivClause
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
type PatSynDirQ          = Q PatSynDir
type PatSynArgsQ         = Q PatSynArgs
type FamilyResultSigQ    = Q FamilyResultSig
type DerivStrategyQ      = Q DerivStrategy

-- must be defined here for DsMeta to find it
type Role                = TH.Role
type InjectivityAnn      = TH.InjectivityAnn

type TyVarBndrUnit       = TyVarBndr ()
type TyVarBndrSpec       = TyVarBndr Specificity

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
bytesPrimL :: Bytes -> Lit
bytesPrimL = BytesPrimL
rationalL   :: Rational -> Lit
rationalL   = RationalL

litP :: Quote m => Lit -> m Pat
litP l = pure (LitP l)

varP :: Quote m => Name -> m Pat
varP v = pure (VarP v)

tupP :: Quote m => [m Pat] -> m Pat
tupP ps = do { ps1 <- sequenceA ps; pure (TupP ps1)}

unboxedTupP :: Quote m => [m Pat] -> m Pat
unboxedTupP ps = do { ps1 <- sequenceA ps; pure (UnboxedTupP ps1)}

unboxedSumP :: Quote m => m Pat -> SumAlt -> SumArity -> m Pat
unboxedSumP p alt arity = do { p1 <- p; pure (UnboxedSumP p1 alt arity) }

conP :: Quote m => Name -> [m Type] -> [m Pat] -> m Pat
conP n ts ps = do ps' <- sequenceA ps
                  ts' <- sequenceA ts
                  pure (ConP n ts' ps')
infixP :: Quote m => m Pat -> Name -> m Pat -> m Pat
infixP p1 n p2 = do p1' <- p1
                    p2' <- p2
                    pure (InfixP p1' n p2')
uInfixP :: Quote m => m Pat -> Name -> m Pat -> m Pat
uInfixP p1 n p2 = do p1' <- p1
                     p2' <- p2
                     pure (UInfixP p1' n p2')
parensP :: Quote m => m Pat -> m Pat
parensP p = do p' <- p
               pure (ParensP p')

tildeP :: Quote m => m Pat -> m Pat
tildeP p = do p' <- p
              pure (TildeP p')
bangP :: Quote m => m Pat -> m Pat
bangP p = do p' <- p
             pure (BangP p')
asP :: Quote m => Name -> m Pat -> m Pat
asP n p = do p' <- p
             pure (AsP n p')
wildP :: Quote m => m Pat
wildP = pure WildP
recP :: Quote m => Name -> [m FieldPat] -> m Pat
recP n fps = do fps' <- sequenceA fps
                pure (RecP n fps')
listP :: Quote m => [m Pat] -> m Pat
listP ps = do ps' <- sequenceA ps
              pure (ListP ps')
sigP :: Quote m => m Pat -> m Type -> m Pat
sigP p t = do p' <- p
              t' <- t
              pure (SigP p' t')
viewP :: Quote m => m Exp -> m Pat -> m Pat
viewP e p = do e' <- e
               p' <- p
               pure (ViewP e' p')

fieldPat :: Quote m => Name -> m Pat -> m FieldPat
fieldPat n p = do p' <- p
                  pure (n, p')


-------------------------------------------------------------------------------
-- *   Stmt

bindS :: Quote m => m Pat -> m Exp -> m Stmt
bindS p e = liftA2 BindS p e

letS :: Quote m => [m Dec] -> m Stmt
letS ds = do { ds1 <- sequenceA ds; pure (LetS ds1) }

noBindS :: Quote m => m Exp -> m Stmt
noBindS e = do { e1 <- e; pure (NoBindS e1) }

parS :: Quote m => [[m Stmt]] -> m Stmt
parS sss = do { sss1 <- traverse sequenceA sss; pure (ParS sss1) }

recS :: Quote m => [m Stmt] -> m Stmt
recS ss = do { ss1 <- sequenceA ss; pure (RecS ss1) }

-------------------------------------------------------------------------------
-- *   Range

fromR :: Quote m => m Exp -> m Range
fromR x = do { a <- x; pure (FromR a) }

fromThenR :: Quote m => m Exp -> m Exp -> m Range
fromThenR x y = do { a <- x; b <- y; pure (FromThenR a b) }

fromToR :: Quote m => m Exp -> m Exp -> m Range
fromToR x y = do { a <- x; b <- y; pure (FromToR a b) }

fromThenToR :: Quote m => m Exp -> m Exp -> m Exp -> m Range
fromThenToR x y z = do { a <- x; b <- y; c <- z;
                         pure (FromThenToR a b c) }
-------------------------------------------------------------------------------
-- *   Body

normalB :: Quote m => m Exp -> m Body
normalB e = do { e1 <- e; pure (NormalB e1) }

guardedB :: Quote m => [m (Guard,Exp)] -> m Body
guardedB ges = do { ges' <- sequenceA ges; pure (GuardedB ges') }

-------------------------------------------------------------------------------
-- *   Guard

normalG :: Quote m => m Exp -> m Guard
normalG e = do { e1 <- e; pure (NormalG e1) }

normalGE :: Quote m => m Exp -> m Exp -> m (Guard, Exp)
normalGE g e = do { g1 <- g; e1 <- e; pure (NormalG g1, e1) }

patG :: Quote m => [m Stmt] -> m Guard
patG ss = do { ss' <- sequenceA ss; pure (PatG ss') }

patGE :: Quote m => [m Stmt] -> m Exp -> m (Guard, Exp)
patGE ss e = do { ss' <- sequenceA ss;
                  e'  <- e;
                  pure (PatG ss', e') }

-------------------------------------------------------------------------------
-- *   Match and Clause

-- | Use with 'caseE'
match :: Quote m => m Pat -> m Body -> [m Dec] -> m Match
match p rhs ds = do { p' <- p;
                      r' <- rhs;
                      ds' <- sequenceA ds;
                      pure (Match p' r' ds') }

-- | Use with 'funD'
clause :: Quote m => [m Pat] -> m Body -> [m Dec] -> m Clause
clause ps r ds = do { ps' <- sequenceA ps;
                      r' <- r;
                      ds' <- sequenceA ds;
                      pure (Clause ps' r' ds') }


---------------------------------------------------------------------------
-- *   Exp

-- | Dynamically binding a variable (unhygenic)
dyn :: Quote m => String -> m Exp
dyn s = pure (VarE (mkName s))

varE :: Quote m => Name -> m Exp
varE s = pure (VarE s)

conE :: Quote m => Name -> m Exp
conE s =  pure (ConE s)

litE :: Quote m => Lit -> m Exp
litE c = pure (LitE c)

appE :: Quote m => m Exp -> m Exp -> m Exp
appE x y = do { a <- x; b <- y; pure (AppE a b)}

appTypeE :: Quote m => m Exp -> m Type -> m Exp
appTypeE x t = do { a <- x; s <- t; pure (AppTypeE a s) }

parensE :: Quote m => m Exp -> m Exp
parensE x = do { x' <- x; pure (ParensE x') }

uInfixE :: Quote m => m Exp -> m Exp -> m Exp -> m Exp
uInfixE x s y = do { x' <- x; s' <- s; y' <- y;
                     pure (UInfixE x' s' y') }

infixE :: Quote m => Maybe (m Exp) -> m Exp -> Maybe (m Exp) -> m Exp
infixE (Just x) s (Just y) = do { a <- x; s' <- s; b <- y;
                                  pure (InfixE (Just a) s' (Just b))}
infixE Nothing  s (Just y) = do { s' <- s; b <- y;
                                  pure (InfixE Nothing s' (Just b))}
infixE (Just x) s Nothing  = do { a <- x; s' <- s;
                                  pure (InfixE (Just a) s' Nothing)}
infixE Nothing  s Nothing  = do { s' <- s; pure (InfixE Nothing s' Nothing) }

infixApp :: Quote m => m Exp -> m Exp -> m Exp -> m Exp
infixApp x y z = infixE (Just x) y (Just z)
sectionL :: Quote m => m Exp -> m Exp -> m Exp
sectionL x y = infixE (Just x) y Nothing
sectionR :: Quote m => m Exp -> m Exp -> m Exp
sectionR x y = infixE Nothing x (Just y)

lamE :: Quote m => [m Pat] -> m Exp -> m Exp
lamE ps e = do ps' <- sequenceA ps
               e' <- e
               pure (LamE ps' e')

-- | Single-arg lambda
lam1E :: Quote m => m Pat -> m Exp -> m Exp
lam1E p e = lamE [p] e

lamCaseE :: Quote m => [m Match] -> m Exp
lamCaseE ms = LamCaseE <$> sequenceA ms

tupE :: Quote m => [Maybe (m Exp)] -> m Exp
tupE es = do { es1 <- traverse sequenceA es; pure (TupE es1)}

unboxedTupE :: Quote m => [Maybe (m Exp)] -> m Exp
unboxedTupE es = do { es1 <- traverse sequenceA es; pure (UnboxedTupE es1)}

unboxedSumE :: Quote m => m Exp -> SumAlt -> SumArity -> m Exp
unboxedSumE e alt arity = do { e1 <- e; pure (UnboxedSumE e1 alt arity) }

condE :: Quote m => m Exp -> m Exp -> m Exp -> m Exp
condE x y z =  do { a <- x; b <- y; c <- z; pure (CondE a b c)}

multiIfE :: Quote m => [m (Guard, Exp)] -> m Exp
multiIfE alts = MultiIfE <$> sequenceA alts

letE :: Quote m => [m Dec] -> m Exp -> m Exp
letE ds e = do { ds2 <- sequenceA ds; e2 <- e; pure (LetE ds2 e2) }

caseE :: Quote m => m Exp -> [m Match] -> m Exp
caseE e ms = do { e1 <- e; ms1 <- sequenceA ms; pure (CaseE e1 ms1) }

doE :: Quote m => Maybe ModName -> [m Stmt] -> m Exp
doE m ss = do { ss1 <- sequenceA ss; pure (DoE m ss1) }

mdoE :: Quote m => Maybe ModName -> [m Stmt] -> m Exp
mdoE m ss = do { ss1 <- sequenceA ss; pure (MDoE m ss1) }

compE :: Quote m => [m Stmt] -> m Exp
compE ss = do { ss1 <- sequenceA ss; pure (CompE ss1) }

arithSeqE :: Quote m => m Range -> m Exp
arithSeqE r = do { r' <- r; pure (ArithSeqE r') }

listE :: Quote m => [m Exp] -> m Exp
listE es = do { es1 <- sequenceA es; pure (ListE es1) }

sigE :: Quote m => m Exp -> m Type -> m Exp
sigE e t = do { e1 <- e; t1 <- t; pure (SigE e1 t1) }

recConE :: Quote m => Name -> [m (Name,Exp)] -> m Exp
recConE c fs = do { flds <- sequenceA fs; pure (RecConE c flds) }

recUpdE :: Quote m => m Exp -> [m (Name,Exp)] -> m Exp
recUpdE e fs = do { e1 <- e; flds <- sequenceA fs; pure (RecUpdE e1 flds) }

stringE :: Quote m => String -> m Exp
stringE = litE . stringL

fieldExp :: Quote m => Name -> m Exp -> m (Name, Exp)
fieldExp s e = do { e' <- e; pure (s,e') }

-- | @staticE x = [| static x |]@
staticE :: Quote m => m Exp -> m Exp
staticE = fmap StaticE

unboundVarE :: Quote m => Name -> m Exp
unboundVarE s = pure (UnboundVarE s)

labelE :: Quote m => String -> m Exp
labelE s = pure (LabelE s)

implicitParamVarE :: Quote m => String -> m Exp
implicitParamVarE n = pure (ImplicitParamVarE n)

getFieldE :: Quote m => m Exp -> String -> m Exp
getFieldE e f = do
  e' <- e
  pure (GetFieldE e' f)

projectionE :: Quote m => NonEmpty String -> m Exp
projectionE xs = pure (ProjectionE xs)

-- ** 'arithSeqE' Shortcuts
fromE :: Quote m => m Exp -> m Exp
fromE x = do { a <- x; pure (ArithSeqE (FromR a)) }

fromThenE :: Quote m => m Exp -> m Exp -> m Exp
fromThenE x y = do { a <- x; b <- y; pure (ArithSeqE (FromThenR a b)) }

fromToE :: Quote m => m Exp -> m Exp -> m Exp
fromToE x y = do { a <- x; b <- y; pure (ArithSeqE (FromToR a b)) }

fromThenToE :: Quote m => m Exp -> m Exp -> m Exp -> m Exp
fromThenToE x y z = do { a <- x; b <- y; c <- z;
                         pure (ArithSeqE (FromThenToR a b c)) }


-------------------------------------------------------------------------------
-- *   Dec

valD :: Quote m => m Pat -> m Body -> [m Dec] -> m Dec
valD p b ds =
  do { p' <- p
     ; ds' <- sequenceA ds
     ; b' <- b
     ; pure (ValD p' b' ds')
     }

funD :: Quote m => Name -> [m Clause] -> m Dec
funD nm cs =
 do { cs1 <- sequenceA cs
    ; pure (FunD nm cs1)
    }

tySynD :: Quote m => Name -> [m (TyVarBndr ())] -> m Type -> m Dec
tySynD tc tvs rhs =
  do { tvs1 <- sequenceA tvs
     ; rhs1 <- rhs
     ; pure (TySynD tc tvs1 rhs1)
     }

dataD :: Quote m => m Cxt -> Name -> [m (TyVarBndr ())] -> Maybe (m Kind) -> [m Con]
      -> [m DerivClause] -> m Dec
dataD ctxt tc tvs ksig cons derivs =
  do
    ctxt1   <- ctxt
    tvs1    <- sequenceA tvs
    ksig1   <- sequenceA ksig
    cons1   <- sequenceA cons
    derivs1 <- sequenceA derivs
    pure (DataD ctxt1 tc tvs1 ksig1 cons1 derivs1)

newtypeD :: Quote m => m Cxt -> Name -> [m (TyVarBndr ())] -> Maybe (m Kind) -> m Con
         -> [m DerivClause] -> m Dec
newtypeD ctxt tc tvs ksig con derivs =
  do
    ctxt1   <- ctxt
    tvs1    <- sequenceA tvs
    ksig1   <- sequenceA ksig
    con1    <- con
    derivs1 <- sequenceA derivs
    pure (NewtypeD ctxt1 tc tvs1 ksig1 con1 derivs1)

classD :: Quote m => m Cxt -> Name -> [m (TyVarBndr ())] -> [FunDep] -> [m Dec] -> m Dec
classD ctxt cls tvs fds decs =
  do
    tvs1  <- sequenceA tvs
    decs1 <- sequenceA decs
    ctxt1 <- ctxt
    pure $ ClassD ctxt1 cls tvs1 fds decs1

instanceD :: Quote m => m Cxt -> m Type -> [m Dec] -> m Dec
instanceD = instanceWithOverlapD Nothing

instanceWithOverlapD :: Quote m => Maybe Overlap -> m Cxt -> m Type -> [m Dec] -> m Dec
instanceWithOverlapD o ctxt ty decs =
  do
    ctxt1 <- ctxt
    decs1 <- sequenceA decs
    ty1   <- ty
    pure $ InstanceD o ctxt1 ty1 decs1



sigD :: Quote m => Name -> m Type -> m Dec
sigD fun ty = liftA (SigD fun) $ ty

kiSigD :: Quote m => Name -> m Kind -> m Dec
kiSigD fun ki = liftA (KiSigD fun) $ ki

forImpD :: Quote m => Callconv -> Safety -> String -> Name -> m Type -> m Dec
forImpD cc s str n ty
 = do ty' <- ty
      pure $ ForeignD (ImportF cc s str n ty')

infixLD :: Quote m => Int -> Name -> m Dec
infixLD prec nm = pure (InfixD (Fixity prec InfixL) nm)

infixRD :: Quote m => Int -> Name -> m Dec
infixRD prec nm = pure (InfixD (Fixity prec InfixR) nm)

infixND :: Quote m => Int -> Name -> m Dec
infixND prec nm = pure (InfixD (Fixity prec InfixN) nm)

pragInlD :: Quote m => Name -> Inline -> RuleMatch -> Phases -> m Dec
pragInlD name inline rm phases
  = pure $ PragmaD $ InlineP name inline rm phases

pragSpecD :: Quote m => Name -> m Type -> Phases -> m Dec
pragSpecD n ty phases
  = do
      ty1    <- ty
      pure $ PragmaD $ SpecialiseP n ty1 Nothing phases

pragSpecInlD :: Quote m => Name -> m Type -> Inline -> Phases -> m Dec
pragSpecInlD n ty inline phases
  = do
      ty1    <- ty
      pure $ PragmaD $ SpecialiseP n ty1 (Just inline) phases

pragSpecInstD :: Quote m => m Type -> m Dec
pragSpecInstD ty
  = do
      ty1    <- ty
      pure $ PragmaD $ SpecialiseInstP ty1

pragRuleD :: Quote m => String -> Maybe [m (TyVarBndr ())] -> [m RuleBndr] -> m Exp -> m Exp
          -> Phases -> m Dec
pragRuleD n ty_bndrs tm_bndrs lhs rhs phases
  = do
      ty_bndrs1 <- traverse sequenceA ty_bndrs
      tm_bndrs1 <- sequenceA tm_bndrs
      lhs1   <- lhs
      rhs1   <- rhs
      pure $ PragmaD $ RuleP n ty_bndrs1 tm_bndrs1 lhs1 rhs1 phases

pragAnnD :: Quote m => AnnTarget -> m Exp -> m Dec
pragAnnD target expr
  = do
      exp1 <- expr
      pure $ PragmaD $ AnnP target exp1

pragLineD :: Quote m => Int -> String -> m Dec
pragLineD line file = pure $ PragmaD $ LineP line file

pragCompleteD :: Quote m => [Name] -> Maybe Name -> m Dec
pragCompleteD cls mty = pure $ PragmaD $ CompleteP cls mty

dataInstD :: Quote m => m Cxt -> (Maybe [m (TyVarBndr ())]) -> m Type -> Maybe (m Kind) -> [m Con]
          -> [m DerivClause] -> m Dec
dataInstD ctxt mb_bndrs ty ksig cons derivs =
  do
    ctxt1   <- ctxt
    mb_bndrs1 <- traverse sequenceA mb_bndrs
    ty1    <- ty
    ksig1   <- sequenceA ksig
    cons1   <- sequenceA cons
    derivs1 <- sequenceA derivs
    pure (DataInstD ctxt1 mb_bndrs1 ty1 ksig1 cons1 derivs1)

newtypeInstD :: Quote m => m Cxt -> (Maybe [m (TyVarBndr ())]) -> m Type -> Maybe (m Kind) -> m Con
             -> [m DerivClause] -> m Dec
newtypeInstD ctxt mb_bndrs ty ksig con derivs =
  do
    ctxt1   <- ctxt
    mb_bndrs1 <- traverse sequenceA mb_bndrs
    ty1    <- ty
    ksig1   <- sequenceA ksig
    con1    <- con
    derivs1 <- sequenceA derivs
    pure (NewtypeInstD ctxt1 mb_bndrs1 ty1 ksig1 con1 derivs1)

tySynInstD :: Quote m => m TySynEqn -> m Dec
tySynInstD eqn =
  do
    eqn1 <- eqn
    pure (TySynInstD eqn1)

dataFamilyD :: Quote m => Name -> [m (TyVarBndr ())] -> Maybe (m Kind) -> m Dec
dataFamilyD tc tvs kind =
  do tvs'  <- sequenceA tvs
     kind' <- sequenceA kind
     pure $ DataFamilyD tc tvs' kind'

openTypeFamilyD :: Quote m => Name -> [m (TyVarBndr ())] -> m FamilyResultSig
                -> Maybe InjectivityAnn -> m Dec
openTypeFamilyD tc tvs res inj =
  do tvs' <- sequenceA tvs
     res' <- res
     pure $ OpenTypeFamilyD (TypeFamilyHead tc tvs' res' inj)

closedTypeFamilyD :: Quote m => Name -> [m (TyVarBndr ())] -> m FamilyResultSig
                  -> Maybe InjectivityAnn -> [m TySynEqn] -> m Dec
closedTypeFamilyD tc tvs result injectivity eqns =
  do tvs1    <- sequenceA tvs
     result1 <- result
     eqns1   <- sequenceA eqns
     pure (ClosedTypeFamilyD (TypeFamilyHead tc tvs1 result1 injectivity) eqns1)

roleAnnotD :: Quote m => Name -> [Role] -> m Dec
roleAnnotD name roles = pure $ RoleAnnotD name roles

standaloneDerivD :: Quote m => m Cxt -> m Type -> m Dec
standaloneDerivD = standaloneDerivWithStrategyD Nothing

standaloneDerivWithStrategyD :: Quote m => Maybe (m DerivStrategy) -> m Cxt -> m Type -> m Dec
standaloneDerivWithStrategyD mdsq ctxtq tyq =
  do
    mds  <- sequenceA mdsq
    ctxt <- ctxtq
    ty   <- tyq
    pure $ StandaloneDerivD mds ctxt ty

defaultSigD :: Quote m => Name -> m Type -> m Dec
defaultSigD n tyq =
  do
    ty <- tyq
    pure $ DefaultSigD n ty

-- | Pattern synonym declaration
patSynD :: Quote m => Name -> m PatSynArgs -> m PatSynDir -> m Pat -> m Dec
patSynD name args dir pat = do
  args'    <- args
  dir'     <- dir
  pat'     <- pat
  pure (PatSynD name args' dir' pat')

-- | Pattern synonym type signature
patSynSigD :: Quote m => Name -> m Type -> m Dec
patSynSigD nm ty =
  do ty' <- ty
     pure $ PatSynSigD nm ty'

-- | Implicit parameter binding declaration. Can only be used in let
-- and where clauses which consist entirely of implicit bindings.
implicitParamBindD :: Quote m => String -> m Exp -> m Dec
implicitParamBindD n e =
  do
    e' <- e
    pure $ ImplicitParamBindD n e'

tySynEqn :: Quote m => (Maybe [m (TyVarBndr ())]) -> m Type -> m Type -> m TySynEqn
tySynEqn mb_bndrs lhs rhs =
  do
    mb_bndrs1 <- traverse sequenceA mb_bndrs
    lhs1 <- lhs
    rhs1 <- rhs
    pure (TySynEqn mb_bndrs1 lhs1 rhs1)

cxt :: Quote m => [m Pred] -> m Cxt
cxt = sequenceA

derivClause :: Quote m => Maybe (m DerivStrategy) -> [m Pred] -> m DerivClause
derivClause mds p = do mds' <- sequenceA mds
                       p'   <- cxt p
                       pure $ DerivClause mds' p'

stockStrategy :: Quote m => m DerivStrategy
stockStrategy = pure StockStrategy

anyclassStrategy :: Quote m => m DerivStrategy
anyclassStrategy = pure AnyclassStrategy

newtypeStrategy :: Quote m => m DerivStrategy
newtypeStrategy = pure NewtypeStrategy

viaStrategy :: Quote m => m Type -> m DerivStrategy
viaStrategy = fmap ViaStrategy

normalC :: Quote m => Name -> [m BangType] -> m Con
normalC con strtys = liftA (NormalC con) $ sequenceA strtys

recC :: Quote m => Name -> [m VarBangType] -> m Con
recC con varstrtys = liftA (RecC con) $ sequenceA varstrtys

infixC :: Quote m => m (Bang, Type) -> Name -> m (Bang, Type) -> m Con
infixC st1 con st2 = do st1' <- st1
                        st2' <- st2
                        pure $ InfixC st1' con st2'

forallC :: Quote m => [m (TyVarBndr Specificity)] -> m Cxt -> m Con -> m Con
forallC ns ctxt con = do
  ns'   <- sequenceA ns
  ctxt' <- ctxt
  con'  <- con
  pure $ ForallC ns' ctxt' con'

gadtC :: Quote m => [Name] -> [m StrictType] -> m Type -> m Con
gadtC cons strtys ty = liftA2 (GadtC cons) (sequenceA strtys) ty

recGadtC :: Quote m => [Name] -> [m VarStrictType] -> m Type -> m Con
recGadtC cons varstrtys ty = liftA2 (RecGadtC cons) (sequenceA varstrtys) ty

-------------------------------------------------------------------------------
-- *   Type

forallT :: Quote m => [m (TyVarBndr Specificity)] -> m Cxt -> m Type -> m Type
forallT tvars ctxt ty = do
    tvars1 <- sequenceA tvars
    ctxt1  <- ctxt
    ty1    <- ty
    pure $ ForallT tvars1 ctxt1 ty1

forallVisT :: Quote m => [m (TyVarBndr ())] -> m Type -> m Type
forallVisT tvars ty = ForallVisT <$> sequenceA tvars <*> ty

varT :: Quote m => Name -> m Type
varT = pure . VarT

conT :: Quote m => Name -> m Type
conT = pure . ConT

infixT :: Quote m => m Type -> Name -> m Type -> m Type
infixT t1 n t2 = do t1' <- t1
                    t2' <- t2
                    pure (InfixT t1' n t2')

uInfixT :: Quote m => m Type -> Name -> m Type -> m Type
uInfixT t1 n t2 = do t1' <- t1
                     t2' <- t2
                     pure (UInfixT t1' n t2')

parensT :: Quote m => m Type -> m Type
parensT t = do t' <- t
               pure (ParensT t')

appT :: Quote m => m Type -> m Type -> m Type
appT t1 t2 = do
           t1' <- t1
           t2' <- t2
           pure $ AppT t1' t2'

appKindT :: Quote m => m Type -> m Kind -> m Type
appKindT ty ki = do
               ty' <- ty
               ki' <- ki
               pure $ AppKindT ty' ki'

arrowT :: Quote m => m Type
arrowT = pure ArrowT

mulArrowT :: Quote m => m Type
mulArrowT = pure MulArrowT

listT :: Quote m => m Type
listT = pure ListT

litT :: Quote m => m TyLit -> m Type
litT l = fmap LitT l

tupleT :: Quote m => Int -> m Type
tupleT i = pure (TupleT i)

unboxedTupleT :: Quote m => Int -> m Type
unboxedTupleT i = pure (UnboxedTupleT i)

unboxedSumT :: Quote m => SumArity -> m Type
unboxedSumT arity = pure (UnboxedSumT arity)

sigT :: Quote m => m Type -> m Kind -> m Type
sigT t k
  = do
      t' <- t
      k' <- k
      pure $ SigT t' k'

equalityT :: Quote m => m Type
equalityT = pure EqualityT

wildCardT :: Quote m => m Type
wildCardT = pure WildCardT

implicitParamT :: Quote m => String -> m Type -> m Type
implicitParamT n t
  = do
      t' <- t
      pure $ ImplicitParamT n t'

{-# DEPRECATED classP "As of template-haskell-2.10, constraint predicates (Pred) are just types (Type), in keeping with ConstraintKinds. Please use 'conT' and 'appT'." #-}
classP :: Quote m => Name -> [m Type] -> m Pred
classP cla tys
  = do
      tysl <- sequenceA tys
      pure (foldl AppT (ConT cla) tysl)

{-# DEPRECATED equalP "As of template-haskell-2.10, constraint predicates (Pred) are just types (Type), in keeping with ConstraintKinds. Please see 'equalityT'." #-}
equalP :: Quote m => m Type -> m Type -> m Pred
equalP tleft tright
  = do
      tleft1  <- tleft
      tright1 <- tright
      eqT <- equalityT
      pure (foldl AppT eqT [tleft1, tright1])

promotedT :: Quote m => Name -> m Type
promotedT = pure . PromotedT

promotedTupleT :: Quote m => Int -> m Type
promotedTupleT i = pure (PromotedTupleT i)

promotedNilT :: Quote m => m Type
promotedNilT = pure PromotedNilT

promotedConsT :: Quote m => m Type
promotedConsT = pure PromotedConsT

noSourceUnpackedness, sourceNoUnpack, sourceUnpack :: Quote m => m SourceUnpackedness
noSourceUnpackedness = pure NoSourceUnpackedness
sourceNoUnpack       = pure SourceNoUnpack
sourceUnpack         = pure SourceUnpack

noSourceStrictness, sourceLazy, sourceStrict :: Quote m => m SourceStrictness
noSourceStrictness = pure NoSourceStrictness
sourceLazy         = pure SourceLazy
sourceStrict       = pure SourceStrict

{-# DEPRECATED isStrict
    ["Use 'bang'. See https://gitlab.haskell.org/ghc/ghc/wikis/migration/8.0. ",
     "Example usage: 'bang noSourceUnpackedness sourceStrict'"] #-}
{-# DEPRECATED notStrict
    ["Use 'bang'. See https://gitlab.haskell.org/ghc/ghc/wikis/migration/8.0. ",
     "Example usage: 'bang noSourceUnpackedness noSourceStrictness'"] #-}
{-# DEPRECATED unpacked
    ["Use 'bang'. See https://gitlab.haskell.org/ghc/ghc/wikis/migration/8.0. ",
     "Example usage: 'bang sourceUnpack sourceStrict'"] #-}
isStrict, notStrict, unpacked :: Quote m => m Strict
isStrict = bang noSourceUnpackedness sourceStrict
notStrict = bang noSourceUnpackedness noSourceStrictness
unpacked = bang sourceUnpack sourceStrict

bang :: Quote m => m SourceUnpackedness -> m SourceStrictness -> m Bang
bang u s = do u' <- u
              s' <- s
              pure (Bang u' s')

bangType :: Quote m => m Bang -> m Type -> m BangType
bangType = liftA2 (,)

varBangType :: Quote m => Name -> m BangType -> m VarBangType
varBangType v bt = (\(b, t) -> (v, b, t)) <$> bt

{-# DEPRECATED strictType
               "As of @template-haskell-2.11.0.0@, 'StrictType' has been replaced by 'BangType'. Please use 'bangType' instead." #-}
strictType :: Quote m => m Strict -> m Type -> m StrictType
strictType = bangType

{-# DEPRECATED varStrictType
               "As of @template-haskell-2.11.0.0@, 'VarStrictType' has been replaced by 'VarBangType'. Please use 'varBangType' instead." #-}
varStrictType :: Quote m => Name -> m StrictType -> m VarStrictType
varStrictType = varBangType

-- * Type Literals

-- MonadFail here complicates things (a lot) because it would mean we would
-- have to emit a MonadFail constraint during typechecking if there was any
-- chance the desugaring would use numTyLit, which in general is hard to
-- predict.
numTyLit :: Quote m => Integer -> m TyLit
numTyLit n = if n >= 0 then pure (NumTyLit n)
                       else error ("Negative type-level number: " ++ show n)

strTyLit :: Quote m => String -> m TyLit
strTyLit s = pure (StrTyLit s)

charTyLit :: Quote m => Char -> m TyLit
charTyLit c = pure (CharTyLit c)

-------------------------------------------------------------------------------
-- *   Kind

plainTV :: Quote m => Name -> m (TyVarBndr ())
plainTV n = pure $ PlainTV n ()

plainInvisTV :: Quote m => Name -> Specificity -> m (TyVarBndr Specificity)
plainInvisTV n s = pure $ PlainTV n s

kindedTV :: Quote m => Name -> m Kind -> m (TyVarBndr ())
kindedTV n = fmap (KindedTV n ())

kindedInvisTV :: Quote m => Name -> Specificity -> m Kind -> m (TyVarBndr Specificity)
kindedInvisTV n s = fmap (KindedTV n s)

specifiedSpec :: Specificity
specifiedSpec = SpecifiedSpec

inferredSpec :: Specificity
inferredSpec = InferredSpec

varK :: Name -> Kind
varK = VarT

conK :: Name -> Kind
conK = ConT

tupleK :: Int -> Kind
tupleK = TupleT

arrowK ::  Kind
arrowK = ArrowT

listK ::  Kind
listK = ListT

appK :: Kind -> Kind -> Kind
appK = AppT

starK :: Quote m => m Kind
starK = pure StarT

constraintK :: Quote m => m Kind
constraintK = pure ConstraintT

-------------------------------------------------------------------------------
-- *   Type family result

noSig :: Quote m => m FamilyResultSig
noSig = pure NoSig

kindSig :: Quote m => m Kind -> m FamilyResultSig
kindSig = fmap KindSig

tyVarSig :: Quote m => m (TyVarBndr ()) -> m FamilyResultSig
tyVarSig = fmap TyVarSig

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

funDep ::  [Name] -> [Name] -> FunDep
funDep = FunDep

-------------------------------------------------------------------------------
-- *   RuleBndr
ruleVar :: Quote m => Name -> m RuleBndr
ruleVar = pure . RuleVar

typedRuleVar :: Quote m => Name -> m Type -> m RuleBndr
typedRuleVar n ty = TypedRuleVar n <$> ty

-------------------------------------------------------------------------------
-- *   AnnTarget
valueAnnotation ::  Name -> AnnTarget
valueAnnotation = ValueAnnotation

typeAnnotation ::  Name -> AnnTarget
typeAnnotation = TypeAnnotation

moduleAnnotation :: AnnTarget
moduleAnnotation = ModuleAnnotation

-------------------------------------------------------------------------------
-- * Pattern Synonyms (sub constructs)

unidir, implBidir :: Quote m => m PatSynDir
unidir    = pure Unidir
implBidir = pure ImplBidir

explBidir :: Quote m => [m Clause] -> m PatSynDir
explBidir cls = do
  cls' <- sequenceA cls
  pure (ExplBidir cls')

prefixPatSyn :: Quote m => [Name] -> m PatSynArgs
prefixPatSyn args = pure $ PrefixPatSyn args

recordPatSyn :: Quote m => [Name] -> m PatSynArgs
recordPatSyn sels = pure $ RecordPatSyn sels

infixPatSyn :: Quote m => Name -> Name -> m PatSynArgs
infixPatSyn arg1 arg2 = pure $ InfixPatSyn arg1 arg2

--------------------------------------------------------------
-- * Useful helper function

appsE :: Quote m => [m Exp] -> m Exp
appsE [] = error "appsE []"
appsE [x] = x
appsE (x:y:zs) = appsE ( (appE x y) : zs )

-- | pure the Module at the place of splicing.  Can be used as an
-- input for 'reifyModule'.
thisModule :: Q Module
thisModule = do
  loc <- location
  pure $ Module (mkPkgName $ loc_package loc) (mkModName $ loc_module loc)

--------------------------------------------------------------
-- * Documentation combinators

-- | Attaches Haddock documentation to the declaration provided. Unlike
-- 'putDoc', the names do not need to be in scope when calling this function so
-- it can be used for quoted declarations and anything else currently being
-- spliced.
-- Not all declarations can have documentation attached to them. For those that
-- can't, 'withDecDoc' will return it unchanged without any side effects.
withDecDoc :: String -> Q Dec -> Q Dec
withDecDoc doc dec = do
  dec' <- dec
  case doc_loc dec' of
    Just loc -> qAddModFinalizer $ qPutDoc loc doc
    Nothing  -> pure ()
  pure dec'
  where
    doc_loc (FunD n _)                                     = Just $ DeclDoc n
    doc_loc (ValD (VarP n) _ _)                            = Just $ DeclDoc n
    doc_loc (DataD _ n _ _ _ _)                            = Just $ DeclDoc n
    doc_loc (NewtypeD _ n _ _ _ _)                         = Just $ DeclDoc n
    doc_loc (TySynD n _ _)                                 = Just $ DeclDoc n
    doc_loc (ClassD _ n _ _ _)                             = Just $ DeclDoc n
    doc_loc (SigD n _)                                     = Just $ DeclDoc n
    doc_loc (ForeignD (ImportF _ _ _ n _))                 = Just $ DeclDoc n
    doc_loc (ForeignD (ExportF _ _ n _))                   = Just $ DeclDoc n
    doc_loc (InfixD _ n)                                   = Just $ DeclDoc n
    doc_loc (DataFamilyD n _ _)                            = Just $ DeclDoc n
    doc_loc (OpenTypeFamilyD (TypeFamilyHead n _ _ _))     = Just $ DeclDoc n
    doc_loc (ClosedTypeFamilyD (TypeFamilyHead n _ _ _) _) = Just $ DeclDoc n
    doc_loc (PatSynD n _ _ _)                              = Just $ DeclDoc n
    doc_loc (PatSynSigD n _)                               = Just $ DeclDoc n

    -- For instances we just pass along the full type
    doc_loc (InstanceD _ _ t _)           = Just $ InstDoc t
    doc_loc (DataInstD _ _ t _ _ _)       = Just $ InstDoc t
    doc_loc (NewtypeInstD _ _ t _ _ _)    = Just $ InstDoc t
    doc_loc (TySynInstD (TySynEqn _ t _)) = Just $ InstDoc t

    -- Declarations that can't have documentation attached to
    -- ValDs that aren't a simple variable pattern
    doc_loc (ValD _ _ _)             = Nothing
    doc_loc (KiSigD _ _)             = Nothing
    doc_loc (PragmaD _)              = Nothing
    doc_loc (RoleAnnotD _ _)         = Nothing
    doc_loc (StandaloneDerivD _ _ _) = Nothing
    doc_loc (DefaultSigD _ _)        = Nothing
    doc_loc (ImplicitParamBindD _ _) = Nothing

-- | Variant of 'withDecDoc' that applies the same documentation to
-- multiple declarations. Useful for documenting quoted declarations.
withDecsDoc :: String -> Q [Dec] -> Q [Dec]
withDecsDoc doc decs = decs >>= mapM (withDecDoc doc . pure)

-- | Variant of 'funD' that attaches Haddock documentation.
funD_doc :: Name -> [Q Clause]
         -> Maybe String -- ^ Documentation to attach to function
         -> [Maybe String] -- ^ Documentation to attach to arguments
         -> Q Dec
funD_doc nm cs mfun_doc arg_docs = do
  qAddModFinalizer $ sequence_
    [putDoc (ArgDoc nm i) s | (i, Just s) <- zip [0..] arg_docs]
  let dec = funD nm cs
  case mfun_doc of
    Just fun_doc -> withDecDoc fun_doc dec
    Nothing -> funD nm cs

-- | Variant of 'dataD' that attaches Haddock documentation.
dataD_doc :: Q Cxt -> Name -> [Q (TyVarBndr ())] -> Maybe (Q Kind)
          -> [(Q Con, Maybe String, [Maybe String])]
          -- ^ List of constructors, documentation for the constructor, and
          -- documentation for the arguments
          -> [Q DerivClause]
          -> Maybe String
          -- ^ Documentation to attach to the data declaration
          -> Q Dec
dataD_doc ctxt tc tvs ksig cons_with_docs derivs mdoc = do
  qAddModFinalizer $ mapM_ docCons cons_with_docs
  let dec = dataD ctxt tc tvs ksig (map (\(con, _, _) -> con) cons_with_docs) derivs
  maybe dec (flip withDecDoc dec) mdoc

-- | Variant of 'newtypeD' that attaches Haddock documentation.
newtypeD_doc :: Q Cxt -> Name -> [Q (TyVarBndr ())] -> Maybe (Q Kind)
             -> (Q Con, Maybe String, [Maybe String])
             -- ^ The constructor, documentation for the constructor, and
             -- documentation for the arguments
             -> [Q DerivClause]
             -> Maybe String
             -- ^ Documentation to attach to the newtype declaration
             -> Q Dec
newtypeD_doc ctxt tc tvs ksig con_with_docs@(con, _, _) derivs mdoc = do
  qAddModFinalizer $ docCons con_with_docs
  let dec = newtypeD ctxt tc tvs ksig con derivs
  maybe dec (flip withDecDoc dec) mdoc

-- | Variant of 'dataInstD' that attaches Haddock documentation.
dataInstD_doc :: Q Cxt -> (Maybe [Q (TyVarBndr ())]) -> Q Type -> Maybe (Q Kind)
              -> [(Q Con, Maybe String, [Maybe String])]
              -- ^ List of constructors, documentation for the constructor, and
              -- documentation for the arguments
              -> [Q DerivClause]
              -> Maybe String
              -- ^ Documentation to attach to the instance declaration
              -> Q Dec
dataInstD_doc ctxt mb_bndrs ty ksig cons_with_docs derivs mdoc = do
  qAddModFinalizer $ mapM_ docCons cons_with_docs
  let dec = dataInstD ctxt mb_bndrs ty ksig (map (\(con, _, _) -> con) cons_with_docs)
              derivs
  maybe dec (flip withDecDoc dec) mdoc

-- | Variant of 'newtypeInstD' that attaches Haddock documentation.
newtypeInstD_doc :: Q Cxt -> (Maybe [Q (TyVarBndr ())]) -> Q Type
                 -> Maybe (Q Kind)
                 -> (Q Con, Maybe String, [Maybe String])
                 -- ^ The constructor, documentation for the constructor, and
                 -- documentation for the arguments
                 -> [Q DerivClause]
                 -> Maybe String
                 -- ^ Documentation to attach to the instance declaration
                 -> Q Dec
newtypeInstD_doc ctxt mb_bndrs ty ksig con_with_docs@(con, _, _) derivs mdoc = do
  qAddModFinalizer $ docCons con_with_docs
  let dec = newtypeInstD ctxt mb_bndrs ty ksig con derivs
  maybe dec (flip withDecDoc dec) mdoc

-- | Variant of 'patSynD' that attaches Haddock documentation.
patSynD_doc :: Name -> Q PatSynArgs -> Q PatSynDir -> Q Pat
            -> Maybe String   -- ^ Documentation to attach to the pattern synonym
            -> [Maybe String] -- ^ Documentation to attach to the pattern arguments
            -> Q Dec
patSynD_doc name args dir pat mdoc arg_docs = do
  qAddModFinalizer $ sequence_
    [putDoc (ArgDoc name i) s | (i, Just s) <- zip [0..] arg_docs]
  let dec = patSynD name args dir pat
  maybe dec (flip withDecDoc dec) mdoc

-- | Document a data/newtype constructor with its arguments.
docCons :: (Q Con, Maybe String, [Maybe String]) -> Q ()
docCons (c, md, arg_docs) = do
  c' <- c
  -- Attach docs to the constructors
  sequence_ [ putDoc (DeclDoc nm) d | Just d <- [md], nm <- get_cons_names c' ]
  -- Attach docs to the arguments
  case c' of
    -- Record selector documentation isn't stored in the argument map,
    -- but in the declaration map instead
    RecC _ var_bang_types ->
      sequence_ [ putDoc (DeclDoc nm) arg_doc
                  | (Just arg_doc, (nm, _, _)) <- zip arg_docs var_bang_types
                ]
    _ ->
      sequence_ [ putDoc (ArgDoc nm i) arg_doc
                    | nm <- get_cons_names c'
                    , (i, Just arg_doc) <- zip [0..] arg_docs
                ]
  where
    get_cons_names :: Con -> [Name]
    get_cons_names (NormalC n _) = [n]
    get_cons_names (RecC n _) = [n]
    get_cons_names (InfixC _ n _) = [n]
    get_cons_names (ForallC _ _ cons) = get_cons_names cons
    -- GadtC can have multiple names, e.g
    -- > data Bar a where
    -- >   MkBar1, MkBar2 :: a -> Bar a
    -- Will have one GadtC with [MkBar1, MkBar2] as names
    get_cons_names (GadtC ns _ _) = ns
    get_cons_names (RecGadtC ns _ _) = ns
