{-# LANGUAGE Rank2Types #-}
module Supercompile.Core.Syntax (
    module Supercompile.Core.Syntax,
    DataCon, Var, Literal, Type, Coercion, PrimOp
  ) where

import Supercompile.Utilities
import Supercompile.StaticFlags

import DataCon  (DataCon)
import Var      (Var, varName)
import Name     (Name, nameOccName)
import OccName  (occNameString)
import Id       (idType)
import Literal  (Literal)
import Type     (Type)
import Coercion (Coercion, mkReflCo)
import PrimOp   (PrimOp)


data AltCon = DataAlt DataCon [Var] | LiteralAlt Literal | DefaultAlt
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

type Term = Identity (TermF Identity)
type TaggedTerm = Tagged (TermF Tagged)
data TermF ann = Var Var
               | Value (ValueF ann)
               | App (ann (TermF ann)) Var
               | TyApp (ann (TermF ann)) Type
               | PrimOp PrimOp [Type] [ann (TermF ann)]
               | Case (ann (TermF ann)) Var Type [AltF ann]
               | Let Var (ann (TermF ann)) (ann (TermF ann)) -- NB: might bind an unlifted thing, in which case the evaluation rules must change
               | LetRec [(Var, ann (TermF ann))] (ann (TermF ann))
               | Cast (ann (TermF ann)) Coercion

type Alt = AltF Identity
type TaggedAlt = AltF Tagged
type AltF ann = (AltCon, ann (TermF ann))

type Value = ValueF Identity
type TaggedValue = ValueF Tagged
data ValueF ann = Indirect Var | Literal Literal | Coercion Coercion
                | TyLambda Var (ann (TermF ann)) | Lambda Var (ann (TermF ann)) | Data DataCon [Type] [Var]

instance Outputable AltCon where
    pprPrec prec altcon = case altcon of
        DataAlt dc xs   -> prettyParen (prec >= appPrec) $ ppr dc <+> hsep (map (pPrintPrec appPrec) xs)
        LiteralAlt l    -> pPrint l
        DefaultAlt      -> text "_"

instance (Functor ann, Outputable1 ann) => Outputable (TermF ann) where
    pprPrec prec e = case e of
        Let x e1 e2       -> pPrintPrecLet prec x (asPrettyFunction1 e1) (asPrettyFunction1 e2)
        LetRec xes e      -> pPrintPrecLetRec prec (map (second asPrettyFunction1) xes) (asPrettyFunction1 e)
        Var x             -> pPrintPrec prec x
        Value v           -> pPrintPrec prec v
        TyApp e ty        -> pPrintPrecApp prec (asPrettyFunction1 e) ty
        App e x           -> pPrintPrecApp prec (asPrettyFunction1 e) x
        PrimOp pop tys es -> pPrintPrecPrimOp prec pop (map asPrettyFunction tys) (map asPrettyFunction1 es)
        Case e x _ty alts -> pPrintPrecCase prec (asPrettyFunction1 e) x (map (second asPrettyFunction1) alts)
        Cast e co         -> pPrintPrecCast prec (asPrettyFunction1 e) co

asPrettyFunction1 :: (Outputable1 f, Outputable a) => f a -> PrettyFunction
asPrettyFunction1 = asPrettyFunction . Wrapper1

pPrintPrecCast :: (Outputable a) => Rational -> a -> Coercion -> SDoc
pPrintPrecCast prec e co = prettyParen (prec > noPrec) $ pPrintPrec opPrec e <+> text "|>" <+> pPrintPrec appPrec co

pPrintPrecApp :: (Outputable a, Outputable b) => Rational -> a -> b -> SDoc
pPrintPrecApp prec e1 e2 = prettyParen (prec >= appPrec) $ pPrintPrec opPrec e1 <+> pPrintPrec appPrec e2

pPrintPrecPrimOp :: (Outputable a, Outputable b, Outputable c) => Rational -> a -> [b] -> [c] -> SDoc
pPrintPrecPrimOp prec pop as xs = pPrintPrecApps prec (PrettyFunction (\prec -> pPrintPrecApps prec pop as)) xs

pPrintPrecCase :: (Outputable a, Outputable b, Outputable c, Outputable d) => Rational -> a -> b -> [(c, d)] -> SDoc
pPrintPrecCase prec e x alts = prettyParen (prec > noPrec) $ hang (text "case" <+> pPrintPrec noPrec e <+> text "of" <+> pPrintPrec noPrec x) 2 $ vcat (map (pPrintPrecAlt noPrec) alts)

pPrintPrecAlt :: (Outputable a, Outputable b) => Rational -> (a, b) -> SDoc
pPrintPrecAlt _ (alt_con, alt_e) = hang (pPrintPrec noPrec alt_con <+> text "->") 2 (pPrintPrec noPrec alt_e)

pPrintPrecLet :: (Outputable a, Outputable b, Outputable c) => Rational -> a -> b -> c -> SDoc
pPrintPrecLet prec x e e_body = prettyParen (prec > noPrec) $ hang (text "let") 2 (pPrintPrec noPrec x <+> text "=" <+> pPrintPrec noPrec e) $$ text "in" <+> pPrintPrec noPrec e_body

pPrintPrecLetRec :: (Outputable a, Outputable b, Outputable c) => Rational -> [(a, b)] -> c -> SDoc
pPrintPrecLetRec prec xes e_body
  | [] <- xes = pPrintPrec prec e_body
  | otherwise = prettyParen (prec > noPrec) $ hang (text "letrec") 2 (vcat [pPrintPrec noPrec x <+> text "=" <+> pPrintPrec noPrec e | (x, e) <- xes]) $$ text "in" <+> pPrintPrec noPrec e_body

instance (Functor ann, Outputable1 ann) => Outputable (ValueF ann) where
    pprPrec prec v = case v of
        Indirect x     -> pPrintPrec prec x
        TyLambda x e   -> pPrintPrecLam prec [x] (asPrettyFunction1 e)
        -- Unfortunately, this nicer pretty-printing doesn't work for general (TermF ann):
        --Lambda x e     -> pPrintPrecLam prec (x:xs) e'
        --  where (xs, e') = collectLambdas e
        Lambda x e     -> pPrintPrecLam prec [x] (asPrettyFunction1 e)
        Data dc tys xs -> pPrintPrecApps prec (PrettyFunction (flip pPrintPrec dc)) ([PrettyFunction (flip pPrintPrec ty) | ty <- tys] ++ [PrettyFunction (flip pPrintPrec x) | x <- xs])
        Literal l      -> pPrintPrec prec l
        Coercion co    -> pPrintPrec prec co

pPrintPrecLam :: Outputable a => Rational -> [Var] -> a -> SDoc
pPrintPrecLam prec xs e = prettyParen (prec > noPrec) $ text "\\" <> hsep [pPrintPrec appPrec y | y <- xs] <+> text "->" <+> pPrintPrec noPrec e

pPrintPrecApps :: (Outputable a, Outputable b) => Rational -> a -> [b] -> SDoc
pPrintPrecApps prec e1 es2 = prettyParen (not (null es2) && prec >= appPrec) $ pPrintPrec opPrec e1 <+> hsep (map (pPrintPrec appPrec) es2)


altConBinders :: AltCon -> [Var]
altConBinders (DataAlt _ xs) = xs
altConBinders (LiteralAlt _) = []
altConBinders DefaultAlt     = []

termToValue :: Copointed ann => ann (TermF ann) -> Maybe (ann (ValueF ann))
termToValue e = case extract e of Value v -> Just (fmap (const v) e); _ -> Nothing

termIsValue :: Copointed ann => ann (TermF ann) -> Bool
termIsValue = isValue . extract

isValue :: TermF ann -> Bool
isValue (Value _) = True
isValue _         = False

termIsCheap :: Copointed ann => ann (TermF ann) -> Bool
termIsCheap = isCheap . extract

isCheap :: Copointed ann => TermF ann -> Bool
isCheap _ | cALL_BY_NAME = True -- A cunning hack. I think this is all that should be required...
isCheap (Var _)         = True
isCheap (Value _)       = True
isCheap (Case e _ _ []) = isCheap (extract e) -- NB: important for pushing down let-bound applications of ``error''
isCheap _               = False

termToVar :: Copointed ann => ann (TermF ann) -> Maybe (Coercion, Var)
termToVar e = case extract e of
    Value (Indirect x) -> Just (mkReflCo (idType x), x)
    Var x              -> Just (mkReflCo (idType x), x)
    _                  -> Nothing -- FIXME: cast things as well

varString :: Var -> String
varString = nameString . varName

nameString :: Name -> String
nameString = occNameString . nameOccName


type Coerced a = (Maybe (Coercion, Tag), a)


class Functor ann => Symantics ann where
    var    :: Var -> ann (TermF ann)
    value  :: ValueF ann -> ann (TermF ann)
    tyApp  :: ann (TermF ann) -> Type -> ann (TermF ann)
    app    :: ann (TermF ann) -> Var -> ann (TermF ann)
    primOp :: PrimOp -> [Type] -> [ann (TermF ann)] -> ann (TermF ann)
    case_  :: ann (TermF ann) -> Var -> Type -> [AltF ann] -> ann (TermF ann)
    let_   :: Var -> ann (TermF ann) -> ann (TermF ann) -> ann (TermF ann)
    letRec :: [(Var, ann (TermF ann))] -> ann (TermF ann) -> ann (TermF ann)
    cast   :: ann (TermF ann) -> Coercion -> ann (TermF ann)

instance Symantics Identity where
    var = I . Var
    value = I . Value
    tyApp e = I . TyApp e
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
    PrimOp pop tys es -> primOp pop tys (map reflect es)
    Case e x ty alts  -> case_ (reflect e) x ty (map (second reflect) alts)
    Let x e1 e2       -> let_ x (reflect e1) (reflect e2)
    LetRec xes e      -> letRec (map (second reflect) xes) (reflect e)
    Cast e co         -> cast (reflect e) co
  where
    reflectValue :: Value -> (forall ann. Symantics ann => ValueF ann)
    reflectValue v = case v of
        Indirect x     -> Indirect x
        TyLambda x e   -> TyLambda x (reflect e)
        Lambda x e     -> Lambda x (reflect e)
        Data dc tys xs -> Data dc tys xs
        Literal l      -> Literal l
        Coercion co    -> Coercion co


{-
literal :: Symantics ann => Literal -> ann (TermF ann)
literal = value . Literal

lambda :: Symantics ann => Var -> ann (TermF ann) -> ann (TermF ann)
lambda x = value . Lambda x

data_ :: Symantics ann => DataCon -> [Var] -> ann (TermF ann)
data_ dc = value . Data dc
-}

tyLambdas :: Symantics ann => [Var] -> ann (TermF ann) -> ann (TermF ann)
tyLambdas = flip $ foldr (\x -> value . TyLambda x)

lambdas :: Symantics ann => [Var] -> ann (TermF ann) -> ann (TermF ann)
lambdas = flip $ foldr (\x -> value . Lambda x)

apps :: Symantics ann => ann (TermF ann) -> [Var] -> ann (TermF ann)
apps = foldl app

varApps :: Symantics ann => Var -> [Var] -> ann (TermF ann)
varApps h xs = var h `apps` xs

letRecSmart :: Symantics ann => [(Var, ann (TermF ann))] -> ann (TermF ann) -> ann (TermF ann)
letRecSmart []  = id
letRecSmart xes = letRec xes

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
