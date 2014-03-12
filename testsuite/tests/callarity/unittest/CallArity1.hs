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
  , ("d0 (go 2 would be bad)",) $
     mkRFun go [x]
        (mkLet d (mkACase (Var go `mkVarApps` [x])
                          (mkLams [y] $ Var y)
                  ) $
            mkLams [z] $ Var f `mkApps` [ Var d `mkVarApps` [x],  Var d `mkVarApps` [x] ]) $
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
  , ("go2 (using surrounding interesting let)",) $
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
  , ("two calls, one from let and from body (d 1 would be bad)",) $
     mkLet  d (mkACase (mkLams [y] $ mkLit 0) (mkLams [y] $ mkLit 0)) $
     mkFun go [x,y] (mkVarApps (Var d) [x]) $
     mkApps (Var d) [mkLApps go [1,2]]
  , ("two recursions",) $
     mkRLet n (mkACase (mkLams [y] $ mkLit 0) (Var n)) $
     mkRLet d (mkACase (mkLams [y] $ mkLit 0) (Var d)) $
         Var n `mkApps` [d `mkLApps` [0]]
  , ("two recursions (semantically like the previous case)",) $
     mkRLet n (mkACase (mkLams [y] $ mkLit 0) (Var n)) $
     mkRLet d (mkACase (mkLams [y] $ n `mkLApps` [0]) (Var d)) $
         d `mkLApps` [0]
  , ("two thunks, one called multiple times (both arity 1 would be bad!)",) $
     mkLet n (mkACase (mkLams [y] $ mkLit 0) (f `mkLApps` [0])) $
     mkLet d (mkACase (mkLams [y] $ mkLit 0) (f `mkLApps` [0])) $
         Var n `mkApps` [Var d `mkApps` [Var d `mkApps` [mkLit 0]]]
  , ("two thunks (recursive), one called multiple times (both arity 1 would be bad!)",) $
     mkRLet n (mkACase (mkLams [y] $ mkLit 0) (Var n)) $
     mkRLet d (mkACase (mkLams [y] $ mkLit 0) (Var d)) $
         Var n `mkApps` [Var d `mkApps` [Var d `mkApps` [mkLit 0]]]
  , ("two functions, not thunks",) $
     mkLet go  (mkLams [x] (mkACase (mkLams [y] $ mkLit 0) (Var f `mkVarApps` [x]))) $
     mkLet go2 (mkLams [x] (mkACase (mkLams [y] $ mkLit 0) (Var f `mkVarApps` [x]))) $
         Var go `mkApps` [go2 `mkLApps` [0,1], mkLit 0]
  , ("a thunk, called multiple times via a forking recursion (d 1 would be bad!)",) $
     mkLet  d   (mkACase (mkLams [y] $ mkLit 0) (f `mkLApps` [0])) $
     mkRLet go2 (mkLams [x] (mkACase (Var go2 `mkApps` [Var go2 `mkApps` [mkLit 0, mkLit 0]]) (Var d))) $
         go2 `mkLApps` [0,1]
  , ("a function, one called multiple times via a forking recursion",) $
     mkLet go   (mkLams [x] (mkACase (mkLams [y] $ mkLit 0) (Var f `mkVarApps` [x]))) $
     mkRLet go2 (mkLams [x] (mkACase (Var go2 `mkApps` [Var go2 `mkApps` [mkLit 0, mkLit 0]]) (go `mkLApps` [0]))) $
         go2 `mkLApps` [0,1]
  , ("two functions (recursive)",) $
     mkRLet go  (mkLams [x] (mkACase (mkLams [y] $ mkLit 0) (Var go `mkVarApps` [x]))) $
     mkRLet go2 (mkLams [x] (mkACase (mkLams [y] $ mkLit 0) (Var go2 `mkVarApps` [x]))) $
         Var go `mkApps` [go2 `mkLApps` [0,1], mkLit 0]
  , ("mutual recursion (thunks), called mutiple times (both arity 1 would be bad!)",) $
     Let (Rec [ (n, mkACase (mkLams [y] $ mkLit 0) (Var d))
              , (d, mkACase (mkLams [y] $ mkLit 0) (Var n))]) $
         Var n `mkApps` [Var d `mkApps` [Var d `mkApps` [mkLit 0]]]
  , ("mutual recursion (functions), but no thunks",) $
     Let (Rec [ (go,  mkLams [x] (mkACase (mkLams [y] $ mkLit 0) (Var go2 `mkVarApps` [x])))
              , (go2, mkLams [x] (mkACase (mkLams [y] $ mkLit 0) (Var go `mkVarApps` [x])))]) $
         Var go `mkApps` [go2 `mkLApps` [0,1], mkLit 0]
  , ("mutual recursion (functions), one boring (d 1 would be bad)",) $
     mkLet d (f `mkLApps` [0]) $
         Let (Rec [ (go,  mkLams [x, y] (Var d `mkApps` [go2 `mkLApps` [1,2]]))
                  , (go2, mkLams [x] (mkACase (mkLams [y] $ mkLit 0) (Var go `mkVarApps` [x])))]) $
             Var d `mkApps` [go2 `mkLApps` [0,1]]
  , ("a thunk (non-function-type), called twice, still calls once",) $
    mkLet d (f `mkLApps` [0]) $
        mkLet x (d `mkLApps` [1]) $
            Var f `mkVarApps` [x, x]
  , ("a thunk (function type), called multiple times, still calls once",) $
    mkLet d (f `mkLApps` [0]) $
        mkLet n (Var f `mkApps` [d `mkLApps` [1]]) $
            mkLams [x] $ Var n `mkVarApps` [x]
  , ("a thunk (non-function-type), in mutual recursion, still calls once (d 1 would be good)",) $
    mkLet d (f `mkLApps` [0]) $
        Let (Rec [ (x, Var d `mkApps` [go `mkLApps` [1,2]])
                 , (go, mkLams [x] $ mkACase (mkLams [z] $ Var x) (Var go `mkVarApps` [x]) ) ]) $
            Var go `mkApps` [mkLit 0, go `mkLApps` [0,1]]
  , ("a thunk (function type), in mutual recursion, still calls once (d 1 would be good)",) $
    mkLet d (f `mkLApps` [0]) $
        Let (Rec [ (n, Var go `mkApps` [d `mkLApps` [1]])
                 , (go, mkLams [x] $ mkACase (Var n) (Var go `mkApps` [Var n `mkVarApps` [x]]) ) ]) $
            Var go `mkApps` [mkLit 0, go `mkLApps` [0,1]]
  , ("a thunk (function type), in mutual recursion, still calls once, d part of mutual recursion (d 1 would be good)",) $
    Let (Rec [ (d, Var f `mkApps` [n `mkLApps` [1]])
             , (n, Var go `mkApps` [d `mkLApps` [1]])
             , (go, mkLams [x] $ mkACase (Var n) (Var go `mkApps` [Var n `mkVarApps` [x]]) ) ]) $
        Var go `mkApps` [mkLit 0, go `mkLApps` [0,1]]
  , ("a thunk (non-function-type) co-calls with the body (d 1 would be bad)",) $
    mkLet d (f `mkLApps` [0]) $
        mkLet x (d `mkLApps` [1]) $
            Var d `mkVarApps` [x]
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

