module TH_repPatSig_asserts where

import Language.Haskell.TH

assertFoo :: Q [Dec] -> Q [Dec]
assertFoo decsQ = do
  decs <- decsQ
  case decs of
    [ SigD _ (AppT (AppT ArrowT (ConT t1)) (ConT t2)),
      FunD _ [Clause [SigP (VarP _) (ConT t3)] (NormalB (VarE _)) []] ]
      | t1 == ''Int && t2 == ''Int && t3 == ''Int -> return []
    _  -> do reportError $ "Unexpected quote contents: " ++ show decs
             return []

assertCon :: Q Exp -> Q [Dec]
assertCon expQ = do
  exp <- expQ
  case exp of
    LamE [SigP (VarP _) (AppT (AppT ArrowT (AppT (AppT (ConT eitherT)
                                                       (ConT charT1))
                                                 (ConT intT1)))
                              (AppT (AppT (TupleT 2) (ConT charT2))
                                    (ConT intT2)))]
         (VarE _)
      | eitherT == ''Either &&
        charT1 == ''Char &&
        charT2 == ''Char &&
        intT1 == ''Int &&
        intT2 == ''Int -> return []
    _ -> do reportError $ "Unexpected quote contents: " ++ show exp
            return []

assertVar :: Q Exp -> Q [Dec]
assertVar expQ = do
  exp <- expQ
  case exp of
    LamE [SigP (VarP x) (AppT (ConT _) (VarT a))]
         (CaseE (VarE x1) [Match (ConP _ [VarP y])
                                 (NormalB (SigE (VarE y1) (VarT a1))) []])
      | x1 == x &&
        y1 == y &&
        a1 == a -> return []
    _ -> do reportError $ "Unexpected quote contents: " ++ show exp
            return []
