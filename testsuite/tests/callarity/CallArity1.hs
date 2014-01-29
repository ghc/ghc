{-# LANGUAGE TupleSections #-}
import CoreSyn
import CoreUtils
import Id
import Type
import MkCore
import CallArity (callArityRHS)
import MkId
import SysTools
import DynFlags
import ErrUtils
import Outputable
import TysWiredIn
import Literal
import GHC
import Control.Monad
import Control.Monad.IO.Class
import System.Environment( getArgs )
import VarSet
import PprCore
import Unique
import CoreLint
import FastString

-- Build IDs. use mkTemplateLocal, more predictable than proper uniques
go, go2, x, d, n, y, z, scrut :: Id
[go, go2, x,d, n, y, z, scrut, f] = mkTestIds
    (words "go go2 x d n y z scrut f")
    [ mkFunTys [intTy, intTy] intTy
    , mkFunTys [intTy, intTy] intTy
    , intTy
    , mkFunTys [intTy] intTy
    , mkFunTys [intTy] intTy
    , intTy
    , intTy
    , boolTy
    , mkFunTys [intTy, intTy] intTy -- protoypical external function
    ]

exprs :: [(String, CoreExpr)]
exprs =
  [ ("go2",) $
     mkRFun go [x]
        (mkLet d (mkACase (Var go `mkVarApps` [x])
                          (mkLams [y] $ Var y)
                  ) $ mkLams [z] $ Var d `mkVarApps` [x]) $
        go `mkLApps` [0, 0]
  , ("nested_go2",) $
     mkRFun go [x]
        (mkLet n (mkACase (Var go `mkVarApps` [x])
                          (mkLams [y] $ Var y))  $
            mkACase (Var n) $
                mkFun go2 [y]
                    (mkLet d
                        (mkACase (Var go `mkVarApps` [x])
                                 (mkLams [y] $ Var y) ) $
                        mkLams [z] $ Var d `mkVarApps` [x] )$
                    Var go2 `mkApps` [mkLit 1] ) $
        go `mkLApps` [0, 0]
  , ("d0",) $
     mkRFun go [x]
        (mkLet d (mkACase (Var go `mkVarApps` [x])
                          (mkLams [y] $ Var y)
                  ) $ mkLams [z] $ Var f `mkApps` [ Var d `mkVarApps` [x],  Var d `mkVarApps` [x] ]) $
        go `mkLApps` [0, 0]
  , ("go2 (in case crut)",) $
     mkRFun go [x]
        (mkLet d (mkACase (Var go `mkVarApps` [x])
                          (mkLams [y] $ Var y)
                  ) $ mkLams [z] $ Var d `mkVarApps` [x]) $
        Case (go `mkLApps` [0, 0]) z intTy
            [(DEFAULT, [], Var f `mkVarApps` [z,z])]
  , ("go2 (in function call)",) $
     mkRFun go [x]
        (mkLet d (mkACase (Var go `mkVarApps` [x])
                          (mkLams [y] $ Var y)
                  ) $ mkLams [z] $ Var d `mkVarApps` [x]) $
        f `mkLApps` [0] `mkApps` [go `mkLApps` [0, 0]]
  , ("go2 (using surrounding interesting let; 'go 2' would be good!)",) $
     mkLet n (f `mkLApps` [0]) $
         mkRFun go [x]
            (mkLet d (mkACase (Var go `mkVarApps` [x])
                              (mkLams [y] $ Var y)
                      ) $ mkLams [z] $ Var d `mkVarApps` [x]) $
            Var f `mkApps` [n `mkLApps` [0],  go `mkLApps` [0, 0]]
  , ("go2 (using surrounding boring let)",) $
     mkLet z (mkLit 0) $
         mkRFun go [x]
            (mkLet d (mkACase (Var go `mkVarApps` [x])
                              (mkLams [y] $ Var y)
                      ) $ mkLams [z] $ Var d `mkVarApps` [x]) $
            Var f `mkApps` [Var z,  go `mkLApps` [0, 0]]
  , ("two recursions (both arity 1 would be good!)",) $
     mkRLet n (mkACase (mkLams [y] $ mkLit 0) (Var n)) $
     mkRLet d (mkACase (mkLams [y] $ mkLit 0) (Var d)) $
         Var n `mkApps` [d `mkLApps` [0]]
  , ("two recursions (semantically like the previous case)",) $
     mkRLet n (mkACase (mkLams [y] $ mkLit 0) (Var n)) $
     mkRLet d (mkACase (mkLams [y] $ n `mkLApps` [0]) (Var d)) $
         d `mkLApps` [0]
  ]

main = do
    [libdir] <- getArgs
    runGhc (Just libdir) $ do
        getSessionDynFlags >>= setSessionDynFlags . flip gopt_set Opt_SuppressUniques
        dflags <- getSessionDynFlags
        liftIO $ forM_ exprs $ \(n,e) -> do
            case lintExpr [f,scrut] e of
                Just msg -> putMsg dflags (msg $$ text "in" <+> text n)
                Nothing -> return ()
            putMsg dflags (text n <> char ':')
            -- liftIO $ putMsg dflags (ppr e)
            let e' = callArityRHS e
            let bndrs = varSetElems (allBoundIds e')
            -- liftIO $ putMsg dflags (ppr e')
            forM_ bndrs $ \v -> putMsg dflags $ nest 4 $ ppr v <+> ppr (idCallArity v)

-- Utilities
mkLApps :: Id -> [Integer] -> CoreExpr
mkLApps v = mkApps (Var v) . map mkLit

mkACase = mkIfThenElse (Var scrut)

mkTestId :: Int -> String -> Type -> Id
mkTestId i s ty = mkSysLocal (mkFastString s) (mkBuiltinUnique i) ty

mkTestIds :: [String] -> [Type] -> [Id]
mkTestIds ns tys = zipWith3 mkTestId [0..] ns tys

mkLet :: Id -> CoreExpr -> CoreExpr -> CoreExpr
mkLet v rhs body = Let (NonRec v rhs) body

mkRLet :: Id -> CoreExpr -> CoreExpr -> CoreExpr
mkRLet v rhs body = Let (Rec [(v, rhs)]) body

mkFun :: Id -> [Id] -> CoreExpr -> CoreExpr -> CoreExpr
mkFun v xs rhs body = mkLet v (mkLams xs rhs) body

mkRFun :: Id -> [Id] -> CoreExpr -> CoreExpr -> CoreExpr
mkRFun v xs rhs body = mkRLet v (mkLams xs rhs) body

mkLit :: Integer -> CoreExpr
mkLit i = Lit (mkLitInteger i intTy)

-- Collects all let-bound IDs
allBoundIds :: CoreExpr -> VarSet
allBoundIds (Let (NonRec v rhs) body) = allBoundIds rhs `unionVarSet` allBoundIds body `extendVarSet` v
allBoundIds (Let (Rec binds) body) =
    allBoundIds body `unionVarSet` unionVarSets
        [ allBoundIds rhs `extendVarSet` v | (v, rhs) <- binds ]
allBoundIds (App e1 e2) = allBoundIds e1 `unionVarSet` allBoundIds e2
allBoundIds (Case scrut _ _ alts) =
    allBoundIds scrut `unionVarSet` unionVarSets
        [ allBoundIds e | (_, _ , e) <- alts ]
allBoundIds (Lam _ e)  = allBoundIds e
allBoundIds (Tick _ e) = allBoundIds e
allBoundIds (Cast e _) = allBoundIds e
allBoundIds _ = emptyVarSet

