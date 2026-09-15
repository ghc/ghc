module T27331b_Plugin ( plugin ) where

import GHC.Plugins
import GHC.Core.Make.Box ( mkBox, mkUnbox )
import GHC.Types.InlinePragma ( neverInlinePragma )

plugin :: Plugin
plugin = defaultPlugin
  { installCoreToDos = \ _ todos -> return (CoreDoPluginPass "T27331b" pass : todos)
  , pluginRecompile  = purePlugin
  }

-- | Add the binding
--
-- > boxed = box (# 0#, ..., 99# #)
--
-- and replace the right-hand side of 'roundtrip' with
--
-- > case unbox boxed of (# x_0, ..., x_99 #) -> [I# x_0, ..., I# x_99]
pass :: ModGuts -> CoreM ModGuts
pass guts = do
  platform <- targetPlatform <$> getDynFlags
  let lits   = [ mkLitInt platform i | i <- [0 .. 99] ]
      tup    = mkCoreUnboxedTuple (map Lit lits)
      tup_ty = exprType tup
      rep    = getRuntimeRep tup_ty
  boxed_rhs <- mkBox rep tup_ty tup
  -- 'boxed' is NOINLINE, so that the boxing and unboxing don't cancel out
  -- before code generation.
  boxed     <- (`setInlinePragma` neverInlinePragma) <$>
                 mkSysLocalM (fsLit "boxed") ManyTy (exprType boxed_rhs)
  unboxed   <- mkUnbox rep tup_ty (Var boxed)
  xs        <- mapM (mkSysLocalM (fsLit "x") ManyTy . literalType) lits
  let roundtrip_rhs =
        mkWildCase unboxed (unrestricted tup_ty) (mkListTy intTy)
          [ Alt (DataAlt (tupleDataCon Unboxed (length xs))) xs
              (mkListExpr intTy [ mkCoreConApps intDataCon [Var x] | x <- xs ]) ]
      replace (b, rhs)
        | getOccString b == "roundtrip" = (b, roundtrip_rhs)
        | otherwise                     = (b, rhs)
  return $
    guts
      { mg_binds =
        [ Rec
          $ (boxed, boxed_rhs)
          : map replace (flattenBinds (mg_binds guts))
        ]
      }
