module CharCertificate (
    Certificate,
    certOwner,
    certRows,
    certify,
    Failure (..),
) where

import Control.Monad (unless)
import GHC.Builtin.WiredIn.Prim (charPrimTy)
import GHC.Builtin.WiredIn.Types (charDataCon, charTy, unboxedUnitDataCon, unboxedUnitTy)
import GHC.Core (Alt (..), AltCon (..), Bind (..), CoreExpr, Expr (..))
import GHC.Core.DataCon (dataConWorkId)
import GHC.Core.FVs (exprFreeVars)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.Type (Type, isLiftedTypeKind, mkVisFunTyManyU, typeKind)
import GHC.Core.Utils (exprType)
import GHC.Types.Id (Id, idName, idType)
import GHC.Types.Literal (Literal (LitChar))
import GHC.Types.Name (Name, isExternalName)
import GHC.Types.Var (isId, varType)
import GHC.Types.Var.Set (elemVarSet)

{- | Owner and validated strict case. Identity is the only admitted default.
The constructor stays private. A user table cannot mint a certificate.
-}
data Certificate = Certificate Name [(Char, Char)] deriving (Eq)

-- | The resolved name of the binding that supplied the case.
certOwner :: Certificate -> Name
certOwner (Certificate owner _) = owner

-- | The literal alternatives in Core order.
certRows :: Certificate -> [(Char, Char)]
certRows (Certificate _ rows) = rows

-- | The first structural requirement the binding failed.
data Failure
    = WrongType
    | UnsupportedBody
    | InvalidContinuation
    | InvalidAlternative
    | AbsentDefault
    | InvalidDefault
    | DuplicateDefault
    | DuplicateLiteral
    | UnsortedLiteral
    deriving (Eq, Show)

{- | Inspect the whole checked body. No test character is supplied.
The generator supplies the desugared binding before optimization.
-}
certify :: Id -> CoreExpr -> Either Failure Certificate
certify owner rhs = do
    unless
        ( isExternalName (idName owner)
            && idType owner `eqType` arrow
            && exprType rhs `eqType` arrow
        )
        $ Left WrongType
    (param, body) <- case rhs of
        Lam v e | hasType charTy v -> Right (v, e)
        _ -> Left UnsupportedBody
    (outerInputs, preBox, box) <- peel [param] [] [] body
    (boxed, field, inner) <- case box of
        Case (Var v) bind ty [Alt (DataAlt con) [unboxed] e]
            | v `elem` outerInputs
            , con == charDataCon
            , hasType charTy bind
            , hasType charPrimTy unboxed
            , ty `eqType` charTy ->
                Right (bind, unboxed, e)
        _ -> Left UnsupportedBody
    (lifted, continuations, decision) <- peel (boxed : outerInputs) [field] preBox inner
    rows <- case decision of
        Case (Var v) prim ty alternatives
            | v == field
            , hasType charPrimTy prim
            , ty `eqType` charTy ->
                checkAlternatives lifted [field, prim] continuations alternatives
        _ -> Left UnsupportedBody
    pure (Certificate (idName owner) rows)
  where
    arrow = mkVisFunTyManyU charTy charTy

{- | Validate desugaring's failure continuation before it can be used.
The body must return the original character. The desugarer can also
leave an unused lifted failure let and a local alias of the input.
Inspect only reachable lifted bindings. An unlifted RHS can demand an
input even if the bound identifier is not used. Each NonRec RHS only
uses the already checked scope.
-}
peel :: [Id] -> [Id] -> [Id] -> CoreExpr -> Either Failure ([Id], [Id], CoreExpr)
peel lifted unboxed known (Let (NonRec binder _) rest)
    | isId binder
    , isLiftedTypeKind (typeKind (varType binder))
    , not (binder `elemVarSet` exprFreeVars rest) =
        peel lifted unboxed known rest
peel lifted unboxed known (Let (NonRec binder (Var input)) rest)
    | hasType charTy binder
    , input `elem` lifted =
        peel (binder : lifted) unboxed known rest
peel lifted unboxed known (Let (NonRec fn (Lam token body)) rest)
    | hasType (mkVisFunTyManyU unboxedUnitTy charTy) fn
    , hasType unboxedUnitTy token
    , defaultBody lifted unboxed known body =
        peel lifted unboxed (fn : known) rest
    | otherwise = Left InvalidContinuation
peel _ _ _ Let{} = Left UnsupportedBody
peel lifted _ known expr = Right (lifted, known, expr)

checkAlternatives :: [Id] -> [Id] -> [Id] -> [Alt Id] -> Either Failure [(Char, Char)]
checkAlternatives lifted unboxed known (Alt DEFAULT [] body : alts)
    | defaultBody lifted unboxed known body = go Nothing alts
    | otherwise = Left InvalidDefault
  where
    -- See Note [Case expression invariants] in GHC.Core. DEFAULT is first
    -- and the literal keys are strictly ordered.
    go _ [] = Right []
    go _ (Alt DEFAULT _ _ : _) = Left DuplicateDefault
    go previous (Alt (LitAlt (LitChar key)) [] rhs : rest)
        | Just key == previous = Left DuplicateLiteral
        | Just old <- previous, key < old = Left UnsortedLiteral
        | App (Var worker) (Lit (LitChar value)) <- rhs
        , worker == dataConWorkId charDataCon =
            ((key, value) :) <$> go (Just key) rest
        | otherwise = Left InvalidAlternative
    go _ _ = Left InvalidAlternative
checkAlternatives _ _ _ _ = Left AbsentDefault

defaultBody :: [Id] -> [Id] -> [Id] -> CoreExpr -> Bool
defaultBody lifted _ _ (Var v) = v `elem` lifted
defaultBody _ unboxed known (App (Var f) (Var a)) =
    (f == dataConWorkId charDataCon && a `elem` unboxed)
        || (f `elem` known && a == dataConWorkId unboxedUnitDataCon)
defaultBody _ _ _ _ = False

hasType :: Type -> Id -> Bool
hasType ty v = isId v && varType v `eqType` ty
