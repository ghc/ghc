{-# LANGUAGE Rank2Types #-}
module Supercompile.Core.Syntax (
    module Supercompile.Core.Syntax,
    Coercion, NormalCo, mkAxInstCo, mkReflCo,
    DataCon, Var, Literal, Type, PrimOp
  ) where

#include "HsVersions.h"

import Supercompile.Utilities
import Supercompile.StaticFlags

import OptCoercion
import VarEnv   (InScopeSet)
import DataCon  (DataCon, dataConWorkId)
import Var      (TyVar, Var, varName, isTyVar, varType)
import Name     (Name, nameOccName)
import OccName  (occNameString)
import Id       (Id, idType)
import PrimOp   (primOpType)
import Literal  (Literal, literalType)
import Type     (Type, mkTyVarTy, applyTy, applyTys, mkForAllTy, mkFunTy, splitFunTy_maybe, eqType)
import Coercion (CoVar, Coercion, coercionType, coercionKind, mkCvSubst, mkAxInstCo, mkReflCo, isReflCo)
import PrimOp   (PrimOp)
import Pair     (pSnd)
import PprCore  ()


mkSymCo :: InScopeSet -> NormalCo -> NormalCo
mkSymCo iss co = optCoercion (mkCvSubst iss []) co

mkTransCo :: InScopeSet -> NormalCo -> NormalCo -> NormalCo
mkTransCo = opt_trans

mkNthCo :: Int -> NormalCo -> NormalCo
mkNthCo = opt_nth

mkInstCo :: InScopeSet -> NormalCo -> Type -> NormalCo
mkInstCo = opt_inst

decomposeCo :: Int -> NormalCo -> [NormalCo]
decomposeCo arity co = [mkNthCo n co | n <- [0..(arity-1)] ]


class Outputable a => OutputableLambdas a where
    pprPrecLam :: a -> ([Var], Rational -> SDoc)

class Outputable1 f => OutputableLambdas1 f where
    pprPrecLam1 :: OutputableLambdas a => f a -> ([Var], Rational -> SDoc)

instance (OutputableLambdas1 f, OutputableLambdas a) => OutputableLambdas (Wrapper1 f a) where
    pprPrecLam = pprPrecLam1 . unWrapper1

instance OutputableLambdas1 Identity where
    pprPrecLam1 (I x) = pprPrecLam x

instance (Functor f, OutputableLambdas1 f, OutputableLambdas1 g) => OutputableLambdas1 (O f g) where
    pprPrecLam1 (Comp x) = pprPrecLam1 (fmap Wrapper1 x)

instance OutputableLambdas1 Tagged where
    pprPrecLam1 (Tagged tg x) = second ((braces (ppr tg) <+>) .) (pprPrecLam x)

instance OutputableLambdas1 Sized where
    pprPrecLam1 (Sized sz x) = second ((bananas (text (show sz)) <>) .) (pprPrecLam x)

pprPrecDefault :: OutputableLambdas a => Rational -> a -> SDoc
pprPrecDefault prec e = pPrintPrecLam prec xs (PrettyFunction ppr_prec)
  where (xs, ppr_prec) = pprPrecLam e


-- NB: don't use GHC's pprBndr because its way too noisy, printing unfoldings etc
pPrintBndr :: BindingSite -> Var -> SDoc
pPrintBndr bs x = prettyParen needs_parens $ ppr x <+> text "::" <+> ppr (varType x)
  where needs_parens = case bs of LambdaBind -> True
                                  CaseBind   -> True
                                  LetBind    -> False

data AltCon = DataAlt DataCon [TyVar] [CoVar] [Id] | LiteralAlt Literal | DefaultAlt
            deriving (Eq, Show)

-- Note [Case wildcards]
-- ~~~~~~~~~~~~~~~~~~~~~
--
-- Simon thought that I should use the variable in the DefaultAlt to agressively rewrite occurences of a scrutinised variable.
-- The motivation is that this lets us do more inlining above the case. For example, take this code fragment from foldl':
--
--   let n' = c n y
--   in case n' of wild -> foldl' c n' ys
--
-- If we rewrite, n' becomes linear:
--
--   let n' = c n y
--   in case n' of wild -> foldl c wild ys
--
-- This lets us potentially inline n' directly into the scrutinee position (operationally, this prevent creation of a thunk for n').
-- However, I don't think that this particular form of improving linearity helps the supercompiler. We only want to inline n' in
-- somewhere if it meets some interesting context, with which it can cancel. But if we are creating an update frame for n' at all,
-- it is *probably* because we had no information about what it evaluated to.
--
-- An interesting exception is when n' binds a case expression:
--
--   let n' = case unk of T -> F; F -> T
--   in case (case n' of T -> F; F -> T) of
--        wild -> e[n']
--
-- You might think that we want n' to be linear so we can inline it into the case on it. However, the splitter will save us and produce:
--
--   case unk of
--     T -> let n' = F
--          in case (case n' of T -> F; F -> T) of wild -> e[n']
--     F -> let n' = T
--          in case (case n' of T -> F; F -> T) of wild -> e[n']
--
-- Since we now know the form of n', everything works out nicely.
--
-- Conclusion: I don't think rewriting to use the case wildcard buys us anything at all.

-- Note [CoApp]
-- ~~~~~~~~~~~~
-- CoApp might seem redundant because we almost never substitute CoVars for Coercions, so we you might think we could get away
-- with just reusing the App constructor but having the Var be either an Id or a CoVar. Unfortunately mkCoVarCo sometimes returns Refl so
-- we can't guarantee that all CoVar substitutions will be variable-for-variable. We add CoApp to work around this fragility.

type Term = Identity (TermF Identity)
type TaggedTerm = Tagged (TermF Tagged)
data TermF ann = Var Id
               | Value (ValueF ann)
               | TyApp (ann (TermF ann)) Type
               | CoApp (ann (TermF ann)) Coercion
               | App (ann (TermF ann)) Id
               | PrimOp PrimOp [Type] [ann (TermF ann)]     -- FIXME: arguably we have just Vars as arguments for better Tag behaviour (otherwise improving the arguments is hidden by the Tag on the whole PrimOp stack frames). FIXME: in fact, we need to change this because *NOT ALL PRIMOP ARGUMENTS ARE STRICT* (e.g. the lazy polymorphic arguments to newMutVar#, newArray#). FIXME: the reason I haven't done this is because it means I should remove the PrimApply frame, which breaks the "question or answer" evaluator normalisation property.
               | Case (ann (TermF ann)) Id Type [AltF ann]  -- NB: unlike GHC, for convenience we allow the list of alternatives to be empty
               | Let Id (ann (TermF ann)) (ann (TermF ann)) -- NB: might bind an unlifted thing, in which case evaluation changes. Unlike GHC, we do NOT assume the RHSes of unlifted bindings are ok-for-speculation.
               | LetRec [(Id, ann (TermF ann))] (ann (TermF ann))
               | Cast (ann (TermF ann)) Coercion

type Alt = AltF Identity
type TaggedAlt = AltF Tagged
type AltF ann = (AltCon, ann (TermF ann))

-- FIXME: I should probably implement a correct operational semantics for TyLambdas!
type Value = ValueF Identity
type TaggedValue = ValueF Tagged
data ValueF ann = Literal Literal | Coercion Coercion
                | TyLambda TyVar (ann (TermF ann)) | Lambda Id (ann (TermF ann)) -- NB: might bind a CoVar
                | Data DataCon [Type] [Coercion] [Id] -- NB: includes universal and existential type arguments, in that order
                                                      -- NB: not a newtype DataCon

instance Outputable AltCon where
    pprPrec prec altcon = case altcon of
        DataAlt dc as qs xs -> prettyParen (prec >= appPrec) $ ppr dc <+> hsep (map (pPrintBndr CaseBind) as ++ map (pPrintBndr CaseBind) qs ++ map (pPrintBndr CaseBind) xs)
        LiteralAlt l        -> pPrint l
        DefaultAlt          -> text "_"

instance (Functor ann, OutputableLambdas1 ann) => Outputable (TermF ann) where
    pprPrec = pprPrecDefault

instance (Functor ann, OutputableLambdas1 ann) => OutputableLambdas (TermF ann) where
    pprPrecLam e = case e of
        Let x e1 e2       -> ([], \prec -> pPrintPrecLet prec x (asPrettyFunction1 e1) (asPrettyFunction1 e2))
        LetRec xes e      -> ([], \prec -> pPrintPrecLetRec prec (map (second asPrettyFunction1) xes) (asPrettyFunction1 e))
        Var x             -> ([], \prec -> pPrintPrec prec x)
        Value v           -> pprPrecLam v
        TyApp e ty        -> ([], \prec -> pPrintPrecApp prec (asPrettyFunction1 e) ty)
        CoApp e co        -> ([], \prec -> pPrintPrecApp prec (asPrettyFunction1 e) co)
        App e x           -> ([], \prec -> pPrintPrecApp prec (asPrettyFunction1 e) x)
        PrimOp pop tys es -> ([], \prec -> pPrintPrecPrimOp prec pop (map asPrettyFunction tys) (map asPrettyFunction1 es))
        Case e x _ty alts -> ([], \prec -> pPrintPrecCase prec (asPrettyFunction1 e) x (map (second asPrettyFunction1) alts))
        Cast e co         -> ([], \prec -> pPrintPrecCast prec (asPrettyFunction1 e) co)

pPrintPrecCast :: (Outputable a) => Rational -> a -> Coercion -> SDoc
pPrintPrecCast prec e co = prettyParen (prec > noPrec) $ pPrintPrec opPrec e <+> text "|>" <+> pPrintPrec appPrec co

pPrintPrecApp :: (Outputable a, Outputable b) => Rational -> a -> b -> SDoc
pPrintPrecApp prec e1 e2 = prettyParen (prec >= appPrec) $ pPrintPrec opPrec e1 <+> pPrintPrec appPrec e2

pPrintPrecPrimOp :: (Outputable a, Outputable b, Outputable c) => Rational -> a -> [b] -> [c] -> SDoc
pPrintPrecPrimOp prec pop as xs = pPrintPrecApps prec (PrettyFunction (\prec -> pPrintPrecApps prec pop as)) xs

pPrintPrecCase :: (Outputable a, Outputable b, Outputable c) => Rational -> a -> Var -> [(b, c)] -> SDoc
pPrintPrecCase prec e x alts = prettyParen (prec > noPrec) $ hang (text "case" <+> pPrintPrec noPrec e <+> text "of" <+> pPrintBndr CaseBind x) 2 $ vcat (map (pPrintPrecAlt noPrec) alts)

pPrintPrecAlt :: (Outputable a, Outputable b) => Rational -> (a, b) -> SDoc
pPrintPrecAlt _ (alt_con, alt_e) = hang (pPrintPrec noPrec alt_con <+> text "->") 2 (pPrintPrec noPrec alt_e)

pPrintPrecLet :: (Outputable a, Outputable b) => Rational -> Var -> a -> b -> SDoc
pPrintPrecLet prec x e e_body = prettyParen (prec > noPrec) $ hang (text "let") 2 (pPrintBndr LetBind x <+> text "=" <+> pPrintPrec noPrec e) $$ text "in" <+> pPrintPrec noPrec e_body

pPrintPrecLetRec, pPrintPrecWhere :: (Outputable a, Outputable b) => Rational -> [(Var, a)] -> b -> SDoc
pPrintPrecLetRec prec xes e_body
  | [] <- xes = pPrintPrec prec e_body
  | otherwise = prettyParen (prec > noPrec) $ hang (text "letrec") 2 (vcat [hang (pPrintBndr LetBind x) 2 (text "=" <+> pPrintPrec noPrec e) | (x, e) <- xes]) $$ text "in" <+> pPrintPrec noPrec e_body

pPrintPrecWhere prec xes e_body
  | [] <- xes = pPrintPrec prec e_body
  | otherwise = prettyParen (prec > noPrec) $ hang (pPrintPrec noPrec e_body) 1 $ hang (text "where") 1 $ vcat [hang (pPrintBndr LetBind x) 2 (text "=" <+> pPrintPrec noPrec e) | (x, e) <- xes]

instance (Functor ann, OutputableLambdas1 ann) => Outputable (ValueF ann) where
    pprPrec = pprPrecDefault

instance (Functor ann, OutputableLambdas1 ann) => OutputableLambdas (ValueF ann) where
    pprPrecLam v = case v of
        TyLambda x e       -> (x:xs, ppr_prec)
          where (xs, ppr_prec) = pprPrecLam1 e
        Lambda x e         -> (x:xs, ppr_prec)
          where (xs, ppr_prec) = pprPrecLam1 e
        Data dc tys cos xs -> ([], \prec -> pPrintPrecApps prec dc ([asPrettyFunction ty | ty <- tys] ++ [asPrettyFunction co | co <- cos] ++ [asPrettyFunction x | x <- xs]))
        Literal l          -> ([], flip pPrintPrec l)
        Coercion co        -> ([], flip pPrintPrec co)

pPrintPrecLam :: Outputable a => Rational -> [Var] -> a -> SDoc
pPrintPrecLam prec [] e = pPrintPrec prec e
pPrintPrecLam prec xs e = prettyParen (prec > noPrec) $ text "\\" <> (vcat [pPrintBndr LambdaBind y | y <- xs] $$ text "->" <+> pPrintPrec noPrec e)

pPrintPrecApps :: (Outputable a, Outputable b) => Rational -> a -> [b] -> SDoc
pPrintPrecApps prec e1 es2 = prettyParen (not (null es2) && prec >= appPrec) $ pPrintPrec opPrec e1 <+> hsep (map (pPrintPrec appPrec) es2)


-- Find those things that are Values and cannot be further evaluated. Primarily used to prevent the
-- speculator from re-speculating values, but also as an approximation for what GHC considers a value.
termIsValue :: Copointed ann => ann (TermF ann) -> Bool
termIsValue = isValue . extract
  where
    isValue (Value _)                         = True
    isValue (Cast e _) | Value _ <- extract e = True
    isValue _                                 = False

-- Find those things that we are willing to duplicate.
termIsCheap :: Copointed ann => ann (TermF ann) -> Bool
termIsCheap = termIsCheap' . extract

termIsCheap' :: Copointed ann => TermF ann -> Bool
termIsCheap' _ | cALL_BY_NAME = True -- A cunning hack. I think this is all that should be required...
termIsCheap' (Var _)         = True
termIsCheap' (Value _)       = True
termIsCheap' (Cast e _)      = termIsCheap e
termIsCheap' (Case e _ _ []) = termIsCheap e -- NB: important for pushing down let-bound applications of ``error''
termIsCheap' _               = False

varString :: Var -> String
varString = nameString . varName

nameString :: Name -> String
nameString = occNameString . nameOccName


data CastBy = Uncast | CastBy NormalCo Tag -- INVARIANT: NormalCo is not Refl
type Coerced a = (CastBy, a)

castBy :: NormalCo -> Tag -> CastBy
castBy co tg | isReflCo co = Uncast -- TODO: this throws away a tag (and hence a deed). But do I care any longer?
             | otherwise   = CastBy co tg

castByCo :: CastBy -> Maybe NormalCo
castByCo Uncast        = Nothing
castByCo (CastBy co _) = Just co


valueType :: Copointed ann => ValueF ann -> Type
valueType (TyLambda a e)      = mkForAllTy a (termType e)
valueType (Lambda x e)        = idType x `mkFunTy` termType e
valueType (Data dc as cos xs) = ((idType (dataConWorkId dc) `applyTys` as) `applyFunTys` map coercionType cos) `applyFunTys` map idType xs
valueType (Literal l)         = literalType l
valueType (Coercion co)       = coercionType co

termType :: Copointed ann => ann (TermF ann) -> Type
termType = termType' . extract

termType' :: Copointed ann => TermF ann -> Type
termType' e = case e of
    Var x             -> idType x
    Value v           -> valueType v
    TyApp e a         -> termType e `applyTy` a
    CoApp e co        -> termType e `applyFunTy` coercionType co
    App e x           -> termType e `applyFunTy` idType x
    PrimOp pop tys es -> (primOpType pop `applyTys` tys) `applyFunTys` map termType es
    Case _ _ ty _     -> ty
    Let _ _ e         -> termType e
    LetRec _ e        -> termType e
    Cast _ co         -> pSnd (coercionKind co)

applyFunTy :: Type -> Type -> Type
applyFunTy fun_ty got_arg_ty = case splitFunTy_maybe fun_ty of
    Just (expected_arg_ty, res_ty) -> ASSERT2(got_arg_ty `eqType` expected_arg_ty, text "applyFunTy:" <+> ppr got_arg_ty <+> ppr expected_arg_ty) res_ty
    Nothing                        -> pprPanic "applyFunTy" (ppr fun_ty $$ ppr got_arg_ty)

applyFunTys :: Type -> [Type] -> Type
applyFunTys = foldl' applyFunTy


class Functor ann => Symantics ann where
    var    :: Var -> ann (TermF ann)
    value  :: ValueF ann -> ann (TermF ann)
    app    :: ann (TermF ann) -> Var -> ann (TermF ann)
    coApp  :: ann (TermF ann) -> Coercion -> ann (TermF ann)
    tyApp  :: ann (TermF ann) -> Type -> ann (TermF ann)
    primOp :: PrimOp -> [Type] -> [ann (TermF ann)] -> ann (TermF ann)
    case_  :: ann (TermF ann) -> Var -> Type -> [AltF ann] -> ann (TermF ann)
    let_   :: Var -> ann (TermF ann) -> ann (TermF ann) -> ann (TermF ann)
    letRec :: [(Var, ann (TermF ann))] -> ann (TermF ann) -> ann (TermF ann)
    cast   :: ann (TermF ann) -> Coercion -> ann (TermF ann)

instance Symantics Identity where
    var = I . Var
    value = I . Value
    tyApp e = I . TyApp e
    coApp e = I . CoApp e
    app e = I . App e
    primOp pop tys = I . PrimOp pop tys
    case_ e x ty = I . Case e x ty
    let_ x e1 = I . Let x e1
    letRec xes = I . LetRec xes
    cast e = I . Cast e


reify :: (forall ann. Symantics ann => ann (TermF ann)) -> Term
reify x = x

reflect :: Term -> (forall ann. Symantics ann => ann (TermF ann))
reflect (I e) = case e of
    Var x             -> var x
    Value v           -> value (reflectValue v)
    TyApp e ty        -> tyApp (reflect e) ty
    App e x           -> app (reflect e) x
    CoApp e co        -> coApp (reflect e) co
    PrimOp pop tys es -> primOp pop tys (map reflect es)
    Case e x ty alts  -> case_ (reflect e) x ty (map (second reflect) alts)
    Let x e1 e2       -> let_ x (reflect e1) (reflect e2)
    LetRec xes e      -> letRec (map (second reflect) xes) (reflect e)
    Cast e co         -> cast (reflect e) co
  where
    reflectValue :: Value -> (forall ann. Symantics ann => ValueF ann)
    reflectValue v = case v of
        TyLambda x e       -> TyLambda x (reflect e)
        Lambda x e         -> Lambda x (reflect e)
        Data dc tys cos xs -> Data dc tys cos xs
        Literal l          -> Literal l
        Coercion co        -> Coercion co


literal :: Symantics ann => Literal -> ann (TermF ann)
literal = value . Literal

coercion :: Symantics ann => Coercion -> ann (TermF ann)
coercion = value . Coercion

{-
lambda :: Symantics ann => Var -> ann (TermF ann) -> ann (TermF ann)
lambda x = value . Lambda x

data_ :: Symantics ann => DataCon -> [Var] -> ann (TermF ann)
data_ dc = value . Data dc
-}

tyLambdas :: Symantics ann => [TyVar] -> ann (TermF ann) -> ann (TermF ann)
tyLambdas = flip $ foldr (\x -> value . TyLambda x)

lambdas :: Symantics ann => [Id] -> ann (TermF ann) -> ann (TermF ann)
lambdas = flip $ foldr (\x -> value . Lambda x)

tyVarIdLambdas :: Symantics ann => [Var] -> ann (TermF ann) -> ann (TermF ann)
tyVarIdLambdas = flip $ foldr tyVarIdLambda

tyVarIdLambda :: Symantics ann => Var -> ann (TermF ann) -> ann (TermF ann)
tyVarIdLambda x e | isTyVar x = value $ TyLambda x e
                  | otherwise = value $ Lambda   x e

tyApps :: Symantics ann => ann (TermF ann) -> [Type] -> ann (TermF ann)
tyApps = foldl tyApp

coApps :: Symantics ann => ann (TermF ann) -> [Coercion] -> ann (TermF ann)
coApps = foldl coApp

apps :: Symantics ann => ann (TermF ann) -> [Id] -> ann (TermF ann)
apps = foldl app

tyVarIdApps :: Symantics ann => ann (TermF ann) -> [Var] -> ann (TermF ann)
tyVarIdApps = foldl tyVarIdApp

tyVarIdApp :: Symantics ann => ann (TermF ann) -> Var -> ann (TermF ann)
tyVarIdApp e x | isTyVar x = e `tyApp` mkTyVarTy x
               | otherwise = e `app` x

{-
strictLet :: Symantics ann => Var -> ann (TermF ann) -> ann (TermF ann) -> ann (TermF ann)
strictLet x e1 e2 = case_ e1 [(DefaultAlt (Just x), e2)]

collectLambdas :: Term -> ([Var], Term)
collectLambdas (I (Value (Lambda x e))) = first (x:) $ collectLambdas e
collectLambdas e                        = ([], e)

freshFloatVar :: IdSupply -> String -> Term -> (IdSupply, Maybe (Var, Term), Var)
freshFloatVar ids _ (I (Var x)) = (ids,  Nothing,     x)
freshFloatVar ids s e           = (ids', Just (y, e), y)
  where (ids', y) = freshName ids s

freshFloatVars :: IdSupply -> String -> [Term] -> (IdSupply, [(Var, Term)], [Var])
freshFloatVars ids s es = reassociate $ mapAccumL (\ids -> associate . freshFloatVar ids s) ids es
  where reassociate (ids, floats_xs) = let (mb_floats, xs) = unzip floats_xs in (ids, catMaybes mb_floats, xs)
        associate (ids, mb_float, x) = (ids, (mb_float, x))
-}
