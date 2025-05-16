{-# LANGUAGE TupleSections, ExplicitNamespaces #-}
import GHC.Core
import GHC.Core.Utils
import GHC.Types.Id
import GHC.Core.Type
import GHC.Core.Multiplicity ( data ManyTy )
import GHC.Core.Make
import GHC.Core.Opt.CallArity (callArityRHS)
import GHC.Types.Id.Make
import GHC.SysTools
import GHC.Driver.Config.Core.Lint
import GHC.Driver.Session
import GHC.Utils.Error
import GHC.Utils.Outputable as Outputable
import GHC.Builtin.Types
import GHC.Builtin.Uniques
import GHC.Types.Literal
import GHC
import Control.Monad
import Control.Monad.IO.Class
import System.Environment( getArgs )
import GHC.Types.Var.Set
import GHC.Core.Ppr
import GHC.Types.Unique
import GHC.Types.Unique.Set
import GHC.Core.Lint
import GHC.Data.FastString

-- Build IDs. use mkTemplateLocal, more predictable than proper uniques
go, go2, x, d, n, y, z, scrutf, scruta :: Id
[go, go2, x,d, n, y, z, scrutf, scruta, f] = mkTestIds
    (words "go go2 x d n y z scrutf scruta f")
    [ mkVisFunTysMany [intTy, intTy] intTy
    , mkVisFunTysMany [intTy, intTy] intTy
    , intTy
    , mkVisFunTysMany [intTy] intTy
    , mkVisFunTysMany [intTy] intTy
    , intTy
    , intTy
    , mkVisFunTysMany [boolTy] boolTy
    , boolTy
    , mkVisFunTysMany [intTy, intTy] intTy -- protoypical external function
    ]

exprs :: [(String, CoreExpr)]
exprs =
  [ ("go2",) $
     mkRFun go [x]
        (mkLetNonRec d (mkACase (Var go `mkVarApps` [x])
                          (mkLams [y] $ Var y)
                  ) $ mkLams [z] $ Var d `mkVarApps` [x]) $
        go `mkLApps` [0, 0]
  , ("nested_go2",) $
     mkRFun go [x]
        (mkLetNonRec n (mkACase (Var go `mkVarApps` [x])
                          (mkLams [y] $ Var y))  $
            mkACase (Var n) $
                mkFun go2 [y]
                    (mkLetNonRec d
                        (mkACase (Var go `mkVarApps` [x])
                                 (mkLams [y] $ Var y) ) $
                        mkLams [z] $ Var d `mkVarApps` [x] )$
                    Var go2 `mkApps` [mkLit 1] ) $
        go `mkLApps` [0, 0]
  , ("d0 (go 2 would be bad)",) $
     mkRFun go [x]
        (mkLetNonRec d (mkACase (Var go `mkVarApps` [x])
                          (mkLams [y] $ Var y)
                  ) $
            mkLams [z] $ Var f `mkApps` [ Var d `mkVarApps` [x],  Var d `mkVarApps` [x] ]) $
        go `mkLApps` [0, 0]
  , ("go2 (in case crut)",) $
     mkRFun go [x]
        (mkLetNonRec d (mkACase (Var go `mkVarApps` [x])
                          (mkLams [y] $ Var y)
                  ) $ mkLams [z] $ Var d `mkVarApps` [x]) $
        Case (go `mkLApps` [0, 0]) z intTy
            [Alt DEFAULT [] (Var f `mkVarApps` [z,z])]
  , ("go2 (in function call)",) $
     mkRFun go [x]
        (mkLetNonRec d (mkACase (Var go `mkVarApps` [x])
                          (mkLams [y] $ Var y)
                  ) $ mkLams [z] $ Var d `mkVarApps` [x]) $
        f `mkLApps` [0] `mkApps` [go `mkLApps` [0, 0]]
  , ("go2 (using surrounding interesting let)",) $
     mkLetNonRec n (f `mkLApps` [0]) $
         mkRFun go [x]
            (mkLetNonRec d (mkACase (Var go `mkVarApps` [x])
                              (mkLams [y] $ Var y)
                      ) $ mkLams [z] $ Var d `mkVarApps` [x]) $
            Var f `mkApps` [n `mkLApps` [0],  go `mkLApps` [0, 0]]
  , ("go2 (using surrounding boring let)",) $
     mkLetNonRec z (mkLit 0) $
         mkRFun go [x]
            (mkLetNonRec d (mkACase (Var go `mkVarApps` [x])
                              (mkLams [y] $ Var y)
                      ) $ mkLams [z] $ Var d `mkVarApps` [x]) $
            Var f `mkApps` [Var z,  go `mkLApps` [0, 0]]
  , ("two calls, one from let and from body (d 1 would be bad)",) $
     mkLetNonRec  d (mkACase (mkLams [y] $ mkLit 0) (mkLams [y] $ mkLit 0)) $
     mkFun go [x,y] (mkVarApps (Var d) [x]) $
     mkApps (Var d) [mkLApps go [1,2]]
  , ("a thunk in a recursion (d 1 would be bad)",) $
     mkRLet n (mkACase (mkLams [y] $ mkLit 0) (Var n)) $
     mkRLet d (mkACase (mkLams [y] $ mkLit 0) (Var d)) $
         Var n `mkApps` [d `mkLApps` [0]]
  , ("two thunks, one called multiple times (both arity 1 would be bad!)",) $
     mkLetNonRec n (mkACase (mkLams [y] $ mkLit 0) (f `mkLApps` [0])) $
     mkLetNonRec d (mkACase (mkLams [y] $ mkLit 0) (f `mkLApps` [0])) $
         Var n `mkApps` [Var d `mkApps` [Var d `mkApps` [mkLit 0]]]
  , ("two functions, not thunks",) $
     mkLetNonRec go  (mkLams [x] (mkACase (mkLams [y] $ mkLit 0) (Var f `mkVarApps` [x]))) $
     mkLetNonRec go2 (mkLams [x] (mkACase (mkLams [y] $ mkLit 0) (Var f `mkVarApps` [x]))) $
         Var go `mkApps` [go2 `mkLApps` [0,1], mkLit 0]
  , ("a thunk, called multiple times via a forking recursion (d 1 would be bad!)",) $
     mkLetNonRec  d   (mkACase (mkLams [y] $ mkLit 0) (f `mkLApps` [0])) $
     mkRLet go2 (mkLams [x] (mkACase (Var go2 `mkApps` [Var go2 `mkApps` [mkLit 0, mkLit 0]]) (Var d))) $
         go2 `mkLApps` [0,1]
  , ("a function, one called multiple times via a forking recursion",) $
     mkLetNonRec go   (mkLams [x] (mkACase (mkLams [y] $ mkLit 0) (Var f `mkVarApps` [x]))) $
     mkRLet go2 (mkLams [x] (mkACase (Var go2 `mkApps` [Var go2 `mkApps` [mkLit 0, mkLit 0]]) (go `mkLApps` [0]))) $
         go2 `mkLApps` [0,1]
  , ("two functions (recursive)",) $
     mkRLet go  (mkLams [x] (mkACase (mkLams [y] $ mkLit 0) (Var go `mkVarApps` [x]))) $
     mkRLet go2 (mkLams [x] (mkACase (mkLams [y] $ mkLit 0) (Var go2 `mkVarApps` [x]))) $
         Var go `mkApps` [go2 `mkLApps` [0,1], mkLit 0]
  , ("mutual recursion (thunks), called multiple times (both arity 1 would be bad!)",) $
     Let (Rec [ (n, mkACase (mkLams [y] $ mkLit 0) (Var d))
              , (d, mkACase (mkLams [y] $ mkLit 0) (Var n))]) $
         Var n `mkApps` [Var d `mkApps` [Var d `mkApps` [mkLit 0]]]
  , ("mutual recursion (functions), but no thunks",) $
     Let (Rec [ (go,  mkLams [x] (mkACase (mkLams [y] $ mkLit 0) (Var go2 `mkVarApps` [x])))
              , (go2, mkLams [x] (mkACase (mkLams [y] $ mkLit 0) (Var go `mkVarApps` [x])))]) $
         Var go `mkApps` [go2 `mkLApps` [0,1], mkLit 0]
  , ("mutual recursion (functions), one boring (d 1 would be bad)",) $
     mkLetNonRec d (f `mkLApps` [0]) $
         Let (Rec [ (go,  mkLams [x, y] (Var d `mkApps` [go2 `mkLApps` [1,2]]))
                  , (go2, mkLams [x] (mkACase (mkLams [y] $ mkLit 0) (Var go `mkVarApps` [x])))]) $
             Var d `mkApps` [go2 `mkLApps` [0,1]]
  , ("a thunk (non-function-type), called twice, still calls once",) $
    mkLetNonRec d (f `mkLApps` [0]) $
        mkLetNonRec x (d `mkLApps` [1]) $
            Var f `mkVarApps` [x, x]
  , ("a thunk (function type), called multiple times, still calls once",) $
    mkLetNonRec d (f `mkLApps` [0]) $
        mkLetNonRec n (Var f `mkApps` [d `mkLApps` [1]]) $
            mkLams [x] $ Var n `mkVarApps` [x]
  , ("a thunk (non-function-type), in mutual recursion, still calls once (d 1 would be good)",) $
    mkLetNonRec d (f `mkLApps` [0]) $
        Let (Rec [ (x, Var d `mkApps` [go `mkLApps` [1,2]])
                 , (go, mkLams [x] $ mkACase (mkLams [z] $ Var x) (Var go `mkVarApps` [x]) ) ]) $
            Var go `mkApps` [mkLit 0, go `mkLApps` [0,1]]
  , ("a thunk (non-function-type), in mutual recursion, causes many calls (d 1 would be bad)",) $
    mkLetNonRec d (f `mkLApps` [0]) $
        Let (Rec [ (x, Var go `mkApps` [go `mkLApps` [1,2], go `mkLApps` [1,2]])
                 , (go, mkLams [x] $ mkACase (Var d) (Var go `mkVarApps` [x]) ) ]) $
            Var go `mkApps` [mkLit 0, go `mkLApps` [0,1]]
  , ("a thunk (function type), in mutual recursion, still calls once (d 1 would be good)",) $
    mkLetNonRec d (f `mkLApps` [0]) $
        Let (Rec [ (n, Var go `mkApps` [d `mkLApps` [1]])
                 , (go, mkLams [x] $ mkACase (Var n) (Var go `mkApps` [Var n `mkVarApps` [x]]) ) ]) $
            Var go `mkApps` [mkLit 0, go `mkLApps` [0,1]]
  , ("a thunk (non-function-type) co-calls with the body (d 1 would be bad)",) $
    mkLetNonRec d (f `mkLApps` [0]) $
        mkLetNonRec x (d `mkLApps` [1]) $
            Var d `mkVarApps` [x]
  ]

main = do
    [libdir] <- getArgs
    runGhc (Just libdir) $ do
        getSessionDynFlags >>= setSessionDynFlags . flip gopt_set Opt_SuppressUniques
        dflags <- getSessionDynFlags
        logger <- getLogger
        liftIO $ forM_ exprs $ \(n,e) -> do
            case lintExpr (initLintConfig dflags [f,scrutf,scruta]) e of
                Just errs -> putMsg logger (pprMessageBag errs $$ text "in" <+> text n)
                Nothing -> return ()
            putMsg logger (text n Outputable.<> char ':')
            -- liftIO $ putMsg logger (ppr e)
            let e' = callArityRHS e
            let bndrs = nonDetEltsUniqSet (allBoundIds e')
              -- It should be OK to use nonDetEltsUniqSet here, if it becomes a
              -- problem we should use DVarSet
            -- liftIO $ putMsg logger (ppr e')
            forM_ bndrs $ \v -> putMsg logger $ nest 4 $ ppr v <+> ppr (idCallArity v)

-- Utilities
mkLApps :: Id -> [Integer] -> CoreExpr
mkLApps v = mkApps (Var v) . map mkLit

mkACase = mkIfThenElse (mkVarApps (Var scrutf) [scruta])

mkTestId :: Int -> String -> Type -> Id
mkTestId i s ty = mkSysLocal (mkFastString s) (mkBuiltinUnique i) ManyTy ty

mkTestIds :: [String] -> [Type] -> [Id]
mkTestIds ns tys = zipWith3 mkTestId [0..] ns tys

mkRLet :: Id -> CoreExpr -> CoreExpr -> CoreExpr
mkRLet v rhs body = mkLetRec [(v, rhs)] body

mkFun :: Id -> [Id] -> CoreExpr -> CoreExpr -> CoreExpr
mkFun v xs rhs body = mkLetNonRec v (mkLams xs rhs) body

mkRFun :: Id -> [Id] -> CoreExpr -> CoreExpr -> CoreExpr
mkRFun v xs rhs body = mkRLet v (mkLams xs rhs) body

mkLit :: Integer -> CoreExpr
mkLit i = mkUncheckedIntExpr i

-- Collects all let-bound IDs
allBoundIds :: CoreExpr -> VarSet
allBoundIds (Let (NonRec v rhs) body) = allBoundIds rhs `unionVarSet` allBoundIds body `extendVarSet` v
allBoundIds (Let (Rec binds) body) =
    allBoundIds body `unionVarSet` unionVarSets
        [ allBoundIds rhs `extendVarSet` v | (v, rhs) <- binds ]
allBoundIds (App e1 e2) = allBoundIds e1 `unionVarSet` allBoundIds e2
allBoundIds (Case scrut _ _ alts) =
    allBoundIds scrut `unionVarSet` unionVarSets
        [ allBoundIds e | Alt _ _ e <- alts ]
allBoundIds (Lam _ e)  = allBoundIds e
allBoundIds (Tick _ e) = allBoundIds e
allBoundIds (Cast e _) = allBoundIds e
allBoundIds _ = emptyVarSet

