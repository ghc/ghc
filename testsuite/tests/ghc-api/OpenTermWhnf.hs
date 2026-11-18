module Main (main) where

import Control.Monad (unless, void)
import Data.Functor.Identity (Identity(..))
import Data.List.NonEmpty (NonEmpty(..))
import GHC (Ghc, InteractiveImport(..), TyThing(..), getSessionDynFlags, lookupName,
            mkModuleName, parseName, runGhc, setContext, setSessionDynFlags,
            simpleImportDecl)
import GHC.Builtin.WiredIn.Prim (charPrimTy)
import GHC.Builtin.WiredIn.Types (boolTy, liftedTypeKind, manyDataConTy)
import GHC.Core (Alt(..), AltCon(..), Bind(..), CoreExpr, Expr(..), maybeUnfoldingTemplate)
import GHC.Core.OpenWhnf
import GHC.Core.FVs (exprSomeFreeVarsList)
import GHC.Core.Type (Type, mkTyVarTy, mkVisFunTyManyU, typeKind)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.Utils (exprType)
import GHC.Data.FastString (fsLit)
import GHC.Driver.Session (GeneralFlag(Opt_IgnoreInterfacePragmas), gopt_unset)
import GHC.Tc.Utils.TcType (vanillaSkolemTvUnk)
import GHC.Types.Id (Id, mkSysLocal, realIdUnfolding)
import GHC.Types.Literal (mkLitChar)
import GHC.Types.Name (mkSysTvName)
import GHC.Types.Unique (UniqueTag(LocalTag), mkUnique)
import GHC.Types.Unique.Supply (uniqFromTag)
import GHC.Types.Var (isGlobalId, mkTcTyVar, tyVarKind, varType)
import GHC.Types.Var.Env (VarEnv, emptyVarEnv, extendVarEnv, lookupVarEnv)
import GHC.Types.Var.Set (VarSet, elemVarSet, emptyVarSet, extendVarSet, unitVarSet)
import System.Environment (getArgs)
import System.Exit (die)

-- | Supply checked definitions and simulated unknowns for the observations.
data OpenInputs = OpenInputs
  { definitions :: VarEnv CoreExpr
  , unsolved :: VarSet
  }

-- | Adapt the pure test inputs to the production demand provider.
openWhnf :: Int -> OpenInputs -> CoreExpr -> Either EvalFailure Whnf
openWhnf fuel inputs expr =
  runIdentity (openWhnfM (fromIntegral fuel) (Identity . lookupVarEnv (definitions inputs))
                        (unsolved inputs) expr)

-- | Load the actual library Id and its typed body from its interface.
lookupIdentity :: Ghc (Maybe (Id, CoreExpr))
lookupIdentity = do
  flags <- getSessionDynFlags
  void (setSessionDynFlags (gopt_unset flags Opt_IgnoreInterfacePragmas))
  setContext [IIDecl (simpleImportDecl (mkModuleName "Prelude"))]
  names <- parseName "Prelude.id"
  case names of
    name :| [] -> do
      thing <- lookupName name
      pure $ do
        AnId fun <- thing
        unfolding <- maybeUnfoldingTemplate (realIdUnfolding fun)
        pure (fun, unfolding)
    _ -> pure Nothing

main :: IO ()
main = do
  args <- getArgs
  case args of
    [libdir] -> do
      identity <- runGhc (Just libdir) lookupIdentity
      case identity of
        Just (fun, unfolding) -> controls fun unfolding
        Nothing -> die "Prelude.id did not have a Core unfolding"
    _ -> die "usage: open-whnf GHC_LIBDIR"

-- | Check imported beta, lexical scope, neutral application, and case demand.
controls :: Id -> CoreExpr -> IO ()
controls identity unfolding = do
  let inputs = OpenInputs (reachableDefinitions identity unfolding) emptyVarSet
      rigid = local "rigid" 1 boolTy
      other = local "other" 2 boolTy
      idCall = App (App (Var identity) (Type boolTy)) (Var rigid)
      x = local "x" 3 boolTy
      xShadow = local "x" 4 boolTy
      ignored = local "ignored" 5 boolTy
      f = local "f" 6 (mkVisFunTyManyU boolTy boolTy)
      loop = local "loop" 7 boolTy
      bottom = Let (Rec [(loop, Var loop)]) (Var loop)
      scoped = App (Lam x (Let (NonRec f (Lam ignored (Var x)))
                           (App (Lam xShadow (App (Var f) (Var xShadow))) (Var other))))
                   (Var rigid)
      unused = App (App (Lam x (Lam ignored (Var x))) (Var rigid)) bottom
      neutralApp = App (Var f) bottom
      scrut = local "scrut" 8 charPrimTy
      caseBinder = local "caseBinder" 9 charPrimTy
      strictCase = Case (Var scrut) caseBinder boolTy [Alt DEFAULT [] (Var rigid)]
      available = inputs {unsolved = unitVarSet scrut}
      solved = available {definitions = extendVarEnv (definitions available) scrut (Lit (mkLitChar 'a'))}

  skolemControls inputs identity
  expect "imported Prelude.id with a rigid argument"
    (isRigid rigid idCall (openWhnf 32 inputs idCall))
  expect "nested closure under same-spelling binders"
    (isRigid rigid scoped (openWhnf 32 inputs scoped))
  expect "projection does not demand its divergent operand"
    (isRigid rigid unused (openWhnf 32 inputs unused))
  expect "divergent operand itself runs out of fuel"
    (case openWhnf 32 inputs bottom of Left Exhausted -> True; _ -> False)
  expect "neutral application retains a suspended operand"
    (case openWhnf 32 inputs neutralApp of
      Right (Neutral (Applied (Rigid headVar) _)) -> headVar == f
      _ -> False)
  expect "rigid case preserves its strict scrutinee and alternatives"
    (isBlocked (Rigid scrut) caseBinder rigid (openWhnf 32 inputs strictCase))
  expect "unsolved case records a resumable unknown"
    (isBlocked (Unsolved scrut) caseBinder rigid (openWhnf 32 available strictCase))
  expect "supplying the missing definition allows the case to reduce"
    (isRigid rigid strictCase (openWhnf 32 solved strictCase))

-- | Collect checked interface templates reachable from the selected function.
-- A template can refer to another exported Id, including an optimizer alias.
reachableDefinitions :: Id -> CoreExpr -> VarEnv CoreExpr
reachableDefinitions root template
  | eqType (exprType template) (varType root) =
      go (32 :: Int) (unitVarSet root) (extendVarEnv emptyVarEnv root template)
        (exprSomeFreeVarsList isGlobalId template)
  | otherwise = emptyVarEnv
  where
    go fuel seen found (v:rest)
      | fuel > 0
      , not (v `elemVarSet` seen) =
          let visited = extendVarSet seen v
          in case maybeUnfoldingTemplate (realIdUnfolding v) of
            Just rhs | eqType (exprType rhs) (varType v) ->
              go (fuel - 1) visited (extendVarEnv found v rhs)
                (exprSomeFreeVarsList isGlobalId rhs ++ rest)
            _ -> go (fuel - 1) visited found rest
      | fuel > 0 = go fuel seen found rest
    go _ _ found _ = found

-- | Observe the identities of distinct symbolic inputs at the skolem's kind.
skolemControls :: OpenInputs -> Id -> IO ()
skolemControls inputs identity = do
  skolemUnique <- uniqFromTag LocalTag
  operandUnique <- uniqFromTag LocalTag
  differentUnique <- uniqFromTag LocalTag
  let name = mkSysTvName skolemUnique (fsLit "a")
      skolem = mkTcTyVar name liftedTypeKind vanillaSkolemTvUnk
      a = mkTyVarTy skolem
      operand = mkSysLocal (fsLit "a") operandUnique manyDataConTy (tyVarKind skolem)
      different = mkSysLocal (fsLit "a") differentUnique manyDataConTy (tyVarKind skolem)
      instantiated = App (Var identity) (Type (tyVarKind skolem))
      call = App instantiated (Var operand)
      wrongCall = App instantiated (Var different)
  expect "symbolic term is typed at the kind of the source skolem"
    (eqType (typeKind a) liftedTypeKind &&
     eqType (exprType (Var operand)) (typeKind a) &&
     eqType (exprType instantiated) (mkVisFunTyManyU (typeKind a) (typeKind a)) &&
     eqType (exprType call) (typeKind a))
  expect "Prelude.id returns the symbolic operand at the skolem's kind"
    (isRigid operand call (openWhnf 32 inputs call))
  let otherResult = openWhnf 32 inputs wrongCall
  expect "a distinct neutral keeps its identity at the same spelling and kind"
    (isRigid different wrongCall otherResult && not (isRigid operand wrongCall otherResult))

-- | Check the residual variable identity and its observed type.
isRigid :: Id -> CoreExpr -> Either EvalFailure Whnf -> Bool
isRigid expected original (Right (Neutral (Rigid actual))) =
  expected == actual && eqType (varType actual) (exprType original)
isRigid _ _ _ = False

-- | Check a neutral case still has its binder and branch body.
isBlocked :: Neutral -> Id -> Id -> Either EvalFailure Whnf -> Bool
isBlocked expected binder rhs
          (Right (Neutral (CaseBlocked scrut (CaseContinuation _ actual ty [Alt DEFAULT [] (Var body)])))) =
  sameHead expected scrut && binder == actual && rhs == body && eqType ty boolTy
  where
    sameHead (Rigid x) (Rigid y) = x == y
    sameHead (Unsolved x) (Unsolved y) = x == y
    sameHead _ _ = False
isBlocked _ _ _ _ = False

-- | Make an actual Core binder with an explicit representation.
local :: String -> Word -> Type -> Id
local spelling serial = mkSysLocal (fsLit spelling) (mkUnique LocalTag (fromIntegral serial)) manyDataConTy

-- | Report one acceptance check.
expect :: String -> Bool -> IO ()
expect check passed = do
  unless passed $ die ("failed " ++ check)
  putStrLn ("passed " ++ check)
