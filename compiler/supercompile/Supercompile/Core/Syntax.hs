{-# LANGUAGE Rank2Types #-}
module Supercompile.Core.Syntax (
    module Supercompile.Core.Syntax,
    DataCon, Var, Literal, Type, Coercion, PrimOp
  ) where

import Supercompile.Utilities
import Supercompile.StaticFlags

import DataCon  (DataCon)
import Var      (TyVar, Var, varName, isTyVar, varType)
import Name     (Name, nameOccName)
import OccName  (occNameString)
import Id       (Id)
import Literal  (Literal)
import Type     (Type, mkTyVarTy, splitTyConApp_maybe, mkPredTy, mkEqPred)
import TysPrim  (eqPredPrimTyCon)
import Coercion (CoVar, Coercion, coercionKind)
import PrimOp   (PrimOp)
import PprCore  ()
import Pair


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
               | PrimOp PrimOp [Type] [ann (TermF ann)]
               | Case (ann (TermF ann)) Id Type [AltF ann]
               | Let Id (ann (TermF ann)) (ann (TermF ann)) -- NB: might bind an unlifted thing, in which case evaluation changes. Unlike GHC, we do NOT assume the RHSes of unlifted bindings are ok-for-speculation.
               | LetRec [(Id, ann (TermF ann))] (ann (TermF ann))
               | Cast (ann (TermF ann)) Coercion

type Alt = AltF Identity
type TaggedAlt = AltF Tagged
type AltF ann = (AltCon, ann (TermF ann))

type Value = ValueF Identity
type TaggedValue = ValueF Tagged
data ValueF ann = Indirect Id -- NB: for the avoidance of doubt, these cannot be CoVars
                | Literal Literal | Coercion Coercion
                | TyLambda TyVar (ann (TermF ann)) | Lambda Id (ann (TermF ann)) -- NB: might bind a CoVar
                | Data DataCon [Type] [Coercion] [Id] -- NB: includes universal and existential type arguments, in that order
                                                      -- NB: not a newtype DataCon

instance Outputable AltCon where
    pprPrec prec altcon = case altcon of
        DataAlt dc as qs xs -> prettyParen (prec >= appPrec) $ ppr dc <+> hsep (map (pPrintBndr CaseBind) as ++ map (pPrintBndr CaseBind) qs ++ map (pPrintBndr CaseBind) xs)
        LiteralAlt l        -> pPrint l
        DefaultAlt          -> text "_"

instance (Functor ann, Outputable1 ann) => Outputable (TermF ann) where
    pprPrec prec e = case e of
        Let x e1 e2       -> pPrintPrecLet prec x (asPrettyFunction1 e1) (asPrettyFunction1 e2)
        LetRec xes e      -> pPrintPrecLetRec prec (map (second asPrettyFunction1) xes) (asPrettyFunction1 e)
        Var x             -> pPrintPrec prec x
        Value v           -> pPrintPrec prec v
        TyApp e ty        -> pPrintPrecApp prec (asPrettyFunction1 e) ty
        CoApp e co        -> pPrintPrecApp prec (asPrettyFunction1 e) co
        App e x           -> pPrintPrecApp prec (asPrettyFunction1 e) x
        PrimOp pop tys es -> pPrintPrecPrimOp prec pop (map asPrettyFunction tys) (map asPrettyFunction1 es)
        Case e x _ty alts -> pPrintPrecCase prec (asPrettyFunction1 e) x (map (second asPrettyFunction1) alts)
        Cast e co         -> pPrintPrecCast prec (asPrettyFunction1 e) co

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

pPrintPrecLetRec :: (Outputable a, Outputable b) => Rational -> [(Var, a)] -> b -> SDoc
pPrintPrecLetRec prec xes e_body
  | [] <- xes = pPrintPrec prec e_body
  | otherwise = prettyParen (prec > noPrec) $ hang (text "letrec") 2 (vcat [pPrintBndr LetBind x <+> text "=" <+> pPrintPrec noPrec e | (x, e) <- xes]) $$ text "in" <+> pPrintPrec noPrec e_body

instance (Functor ann, Outputable1 ann) => Outputable (ValueF ann) where
    pprPrec prec v = case v of
        Indirect x         -> pPrintPrec prec x
        TyLambda x e       -> pPrintPrecLam prec [x] (asPrettyFunction1 e)
        -- Unfortunately, this nicer pretty-printing doesn't work for general (TermF ann):
        --Lambda x e     -> pPrintPrecLam prec (x:xs) e'
        --  where (xs, e') = collectLambdas e
        Lambda x e         -> pPrintPrecLam prec [x] (asPrettyFunction1 e)
        Data dc tys cos xs -> pPrintPrecApps prec dc ([asPrettyFunction ty | ty <- tys] ++ [asPrettyFunction co | co <- cos] ++ [asPrettyFunction x | x <- xs])
        Literal l          -> pPrintPrec prec l
        Coercion co        -> pPrintPrec prec co

pPrintPrecLam :: Outputable a => Rational -> [Var] -> a -> SDoc
pPrintPrecLam prec xs e = prettyParen (prec > noPrec) $ text "\\" <> hsep [pPrintBndr LambdaBind y | y <- xs] <+> text "->" <+> pPrintPrec noPrec e

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
termIsCheap = isCheap . extract
  where
    isCheap _ | cALL_BY_NAME = True -- A cunning hack. I think this is all that should be required...
    isCheap (Var _)         = True
    isCheap (Value _)       = True
    isCheap (Cast e _)      = isCheap (extract e)
    isCheap (Case e _ _ []) = isCheap (extract e) -- NB: important for pushing down let-bound applications of ``error''
    isCheap _               = False

varString :: Var -> String
varString = nameString . varName

nameString :: Name -> String
nameString = occNameString . nameOccName


type Coerced a = (Maybe (Coercion, Tag), a)


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
        Indirect x         -> Indirect x
        TyLambda x e       -> TyLambda x (reflect e)
        Lambda x e         -> Lambda x (reflect e)
        Data dc tys cos xs -> Data dc tys cos xs
        Literal l          -> Literal l
        Coercion co        -> Coercion co


{-
literal :: Symantics ann => Literal -> ann (TermF ann)
literal = value . Literal

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
tyVarIdLambdas = flip $ foldr (\x -> value . tyVarIdLambda x)
  where tyVarIdLambda x e | isTyVar x = TyLambda x e
                          | otherwise = Lambda   x e

tyApps :: Symantics ann => ann (TermF ann) -> [Type] -> ann (TermF ann)
tyApps = foldl tyApp

apps :: Symantics ann => ann (TermF ann) -> [Id] -> ann (TermF ann)
apps = foldl app

tyVarIdApps :: Symantics ann => ann (TermF ann) -> [Var] -> ann (TermF ann)
tyVarIdApps = foldl tyVarIdApp
  where tyVarIdApp e x | isTyVar x = tyApp e (mkTyVarTy x)
                       | otherwise = app   e x

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

-- OK, this is a bit bizarre.
--
-- The coercion handling stuff in GHC sometimes rewrites an equality PredTy
-- into its representation using eqPredPrimTyCon. We might observe such a type
-- in one of the return types of the coercionKind.
--
-- We CANNOT let this "representation" type reach the top level, e.g. be set
-- as the type of a variable. Reason: a variable with a type of ((~) ty1 ty2)
-- is NOT a CoVar is the sense of isCoVar
--
-- Our (slightly hacky) solution is to try to undo this representation type
-- rubbish right here. TODO: perhaps I should do it for other PredTy as well?
---
-- Simon says that the fact that things with ((~) ty1 ty2) types are not detected
-- as CoVars is actually a bug, and he will fix it. We need to keep this workaround
-- if we want to work on earlier GHCs (e.g. the GHC 7.2 RC).
coercionKindNonRepr :: Coercion -> Pair Type
coercionKindNonRepr co = Pair (repair from_ty) (repair to_ty)
  where Pair from_ty to_ty = coercionKind co
        repair ty | Just (tc, [ty1, ty2]) <- splitTyConApp_maybe ty
                  , tc == eqPredPrimTyCon
                  = mkPredTy (mkEqPred (ty1, ty2))
                  | otherwise
                  = ty
