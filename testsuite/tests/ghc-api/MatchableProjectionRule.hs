module Main where

import Control.Monad (unless)
import Data.Maybe (isNothing)
import GHC.Builtin.CoercionRules (builtInCoAxiomRules)
import GHC.Builtin.WiredIn.Types
import GHC.Core.Coercion.Axiom (CoAxiomRule(..))
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.Type
import GHC.Data.FastString (fsLit)
import GHC.Data.Pair (Pair(..))
import GHC.Types.Basic (LeftOrRight(..))
import GHC.Types.Name (mkSystemName)
import GHC.Types.Name.Occurrence (mkTyVarOcc)
import GHC.Types.Unique (mkUniqueGrimilyWithTag)
import GHC.Types.Unique.FM (lookupUFM)
import GHC.Types.Var (mkTyVar)

-- | Check that a projection validates its two applied types and their head kinds.
main :: IO ()
main = do
    let tyvar n ident k = mkTyVarTy (mkTyVar (mkSystemName (mkUniqueGrimilyWithTag 't' ident) (mkTyVarOcc n)) k)
        headKind m = mkFunctionType m manyDataConTy liftedTypeKind liftedTypeKind
        f = tyvar "f" 82001 (headKind matchableDataConTy)
        g = mkTyConTy maybeTyCon
        u = tyvar "u" 82002 (headKind unmatchableDataConTy)
        args lhs rhs lf x rg y = [Pair lhs rhs, both lf, both x, both rg, both y]
        both x = Pair x x
        ruleName CLeft = fsLit "MatchableAppLeft"
        ruleName CRight = fsLit "MatchableAppRight"
        check lr = case lookupUFM builtInCoAxiomRules (ruleName lr) of
            Just (BuiltInRule _ _ _ proves) -> proves
            _ -> error "expected a checked projection rule"
        lhs = mkAppTy f boolTy
        rhs = mkAppTy g charTy
        ok = args lhs rhs f boolTy g charTy
        same l r (Just (Pair a b)) = eqType a l && eqType b r
        same _ _ _ = False
    unless (same f g (check CLeft ok) && same boolTy charTy (check CRight ok)) $
        fail "wrong projected component"
    let noHead = args (mkAppTy u boolTy) rhs u boolTy g charTy
        wrongHead = args lhs rhs g boolTy g charTy
        wrongArg = args lhs rhs f charTy g charTy
        wrongKind = args (mkAppTy f matchableDataConTy) rhs f matchableDataConTy g charTy
        noRightHead = args lhs (mkAppTy u charTy) f boolTy u charTy
        wrongRightArg = args lhs rhs f boolTy g boolTy
        invalid = [noHead, wrongHead, wrongArg, wrongKind, noRightHead, wrongRightArg, tail ok]
    unless (all (\lr -> all (isNothing . check lr) invalid) [CLeft, CRight]) $
        fail "accepted a projection with an unproved head, argument, or kind"
