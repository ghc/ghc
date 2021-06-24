-- | Generate a generate statement for the builtin function "fst"
genFst :: BuiltinBuilder
genFst = genNoInsts genFst'
genFst' :: (Either CoreSyn.CoreBndr AST.VHDLName) -> CoreSyn.CoreBndr -> [(Either CoreSyn.CoreExpr AST.Expr, Type.Type)] -> TranslatorSession [AST.ConcSm]
genFst' res f args@[(arg,argType)] = do {
  ; arg_htype <- MonadState.lift tsType $ mkHType "\nGenerate.genFst: Invalid argument type" argType
  ; [AST.PrimName argExpr] <- argsToVHDLExprs [arg]
  ; let {
        ; labels      = getFieldLabels arg_htype 0
        ; argexprA    = vhdlNameToVHDLExpr $ mkSelectedName argExpr (labels!!0)
        ; assign      = mkUncondAssign res argexprA
        } ;
    -- Return the generate functions
  ; return [assign]
  }
