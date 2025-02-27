-- -----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 1993-2004
--
--
-- -----------------------------------------------------------------------------

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.Cmm.GenericOpt
   ( cmmToCmm
   )
where

import GHC.Prelude hiding (head)
import GHC.Platform
import GHC.CmmToAsm.PIC
import GHC.CmmToAsm.Config
import GHC.CmmToAsm.Types
import GHC.Cmm.BlockId
import GHC.Cmm
import GHC.Cmm.Utils
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Opt           ( cmmMachOpFold )
import GHC.Cmm.CLabel
import GHC.Data.FastString
import GHC.Unit
import Control.Monad.Trans.Reader
import GHC.Utils.Monad.State.Strict as Strict

-- -----------------------------------------------------------------------------
-- Generic Cmm optimiser

{-
Here we do:

  (a) Constant folding
  (c) Position independent code and dynamic linking
        (i)  introduce the appropriate indirections
             and position independent refs
        (ii) compile a list of imported symbols
  (d) Some arch-specific optimizations

(a) will be moving to the new Hoopl pipeline, however, (c) and
(d) are only needed by the native backend and will continue to live
here.

Ideas for other things we could do (put these in Hoopl please!):

  - shortcut jumps-to-jumps
  - simple CSE: if an expr is assigned to a temp, then replace later occs of
    that expr with the temp, until the expr is no longer valid (can push through
    temp assignments, and certain assigns to mem...)
-}

cmmToCmm :: NCGConfig -> RawCmmDecl -> (RawCmmDecl, [CLabel])
cmmToCmm _ top@(CmmData _ _) = (top, [])
cmmToCmm config (CmmProc info lbl live graph)
    = runCmmOpt config $
      do blocks' <- mapM cmmBlockConFold (toBlockList graph)
         return $ CmmProc info lbl live (ofBlockList (g_entry graph) blocks')

type OptMResult a = (# a, [CLabel] #)

pattern OptMResult :: a -> b -> (# a, b #)
pattern OptMResult x y = (# x, y #)
{-# COMPLETE OptMResult #-}

newtype CmmOptM a = CmmOptM (NCGConfig -> [CLabel] -> OptMResult a)
    deriving (Functor, Applicative, Monad) via (ReaderT NCGConfig (Strict.State [CLabel]))

instance CmmMakeDynamicReferenceM CmmOptM where
    addImport = addImportCmmOpt

addImportCmmOpt :: CLabel -> CmmOptM ()
addImportCmmOpt lbl = CmmOptM $ \_ imports -> OptMResult () (lbl:imports)

getCmmOptConfig :: CmmOptM NCGConfig
getCmmOptConfig = CmmOptM $ \config imports -> OptMResult config imports

runCmmOpt :: NCGConfig -> CmmOptM a -> (a, [CLabel])
runCmmOpt config (CmmOptM f) =
  case f config [] of
    OptMResult result imports -> (result, imports)

cmmBlockConFold :: CmmBlock -> CmmOptM CmmBlock
cmmBlockConFold block = do
  let (entry, middle, last) = blockSplit block
      stmts = blockToList middle
  stmts' <- mapM cmmStmtConFold stmts
  last' <- cmmStmtConFold last
  return $ blockJoin entry (blockFromList stmts') last'

-- This does three optimizations, but they're very quick to check, so we don't
-- bother turning them off even when the Hoopl code is active.  Since
-- this is on the old Cmm representation, we can't reuse the code either:
--  * reg = reg      --> nop
--  * if 0 then jump --> nop
--  * if 1 then jump --> jump
-- We might be tempted to skip this step entirely of not Opt_PIC, but
-- there is some PowerPC code for the non-PIC case, which would also
-- have to be separated.
cmmStmtConFold :: CmmNode e x -> CmmOptM (CmmNode e x)
cmmStmtConFold stmt
   = case stmt of
        CmmAssign reg src
           -> do src' <- cmmExprConFold DataReference src
                 return $ case src' of
                   CmmReg reg' | reg == reg' -> CmmComment (fsLit "nop")
                   new_src -> CmmAssign reg new_src

        CmmStore addr src align
           -> do addr' <- cmmExprConFold DataReference addr
                 src'  <- cmmExprConFold DataReference src
                 return $ CmmStore addr' src' align

        CmmCall { cml_target = addr }
           -> do addr' <- cmmExprConFold JumpReference addr
                 return $ stmt { cml_target = addr' }

        CmmUnsafeForeignCall target regs args
           -> do target' <- case target of
                              ForeignTarget e conv -> do
                                e' <- cmmExprConFold CallReference e
                                return $ ForeignTarget e' conv
                              PrimTarget _ ->
                                return target
                 args' <- mapM (cmmExprConFold DataReference) args
                 return $ CmmUnsafeForeignCall target' regs args'

        CmmCondBranch test true false likely
           -> do test' <- cmmExprConFold DataReference test
                 return $ case test' of
                   CmmLit (CmmInt 0 _) -> CmmBranch false
                   CmmLit (CmmInt _ _) -> CmmBranch true
                   _other -> CmmCondBranch test' true false likely

        CmmSwitch expr ids
           -> do expr' <- cmmExprConFold DataReference expr
                 return $ CmmSwitch expr' ids

        other
           -> return other

cmmExprConFold :: ReferenceKind -> CmmExpr -> CmmOptM CmmExpr
cmmExprConFold referenceKind expr = do
    config <- getCmmOptConfig

    let expr' = if not (ncgDoConstantFolding config)
                    then expr
                    else cmmExprCon config expr

    cmmExprNative referenceKind expr'

cmmExprCon :: NCGConfig -> CmmExpr -> CmmExpr
cmmExprCon config (CmmLoad addr rep align) = CmmLoad (cmmExprCon config addr) rep align
cmmExprCon config (CmmMachOp mop args)
    = cmmMachOpFold (ncgPlatform config) mop (fmap (cmmExprCon config) args)
cmmExprCon _ other = other

-- handles both PIC and non-PIC cases... a very strange mixture
-- of things to do.
cmmExprNative :: ReferenceKind -> CmmExpr -> CmmOptM CmmExpr
cmmExprNative referenceKind expr = do
     config <- getCmmOptConfig
     let platform = ncgPlatform config
         arch = platformArch platform
     case expr of
        CmmLoad addr rep align
          -> do addr' <- cmmExprNative DataReference addr
                return $ CmmLoad addr' rep align

        CmmMachOp mop args
          -> do args' <- mapM (cmmExprNative DataReference) args
                return $ CmmMachOp mop args'

        CmmLit (CmmBlock id)
          -> cmmExprNative referenceKind (CmmLit (CmmLabel (infoTblLbl id)))
          -- we must convert block Ids to CLabels here, because we
          -- might have to do the PIC transformation.  Hence we must
          -- not modify BlockIds beyond this point.

        CmmLit (CmmLabel lbl)
          -> cmmMakeDynamicReference config referenceKind lbl
        CmmLit (CmmLabelOff lbl off)
          -> do dynRef <- cmmMakeDynamicReference config referenceKind lbl
                -- need to optimize here, since it's late
                return $ cmmMachOpFold platform (MO_Add (wordWidth platform)) (
                  TupleG2
                    dynRef
                    (CmmLit $ CmmInt (fromIntegral off) (wordWidth platform))
                  )

        -- On powerpc (non-PIC), it's easier to jump directly to a label than
        -- to use the register table, so we replace these registers
        -- with the corresponding labels:
        CmmReg (CmmGlobal (GlobalRegUse EagerBlackholeInfo _))
          | arch == ArchPPC && not (ncgPIC config)
          -> cmmExprNative referenceKind $
             CmmLit (CmmLabel (mkCmmCodeLabel rtsUnitId (fsLit "__stg_EAGER_BLACKHOLE_info")))
        CmmReg (CmmGlobal (GlobalRegUse GCEnter1 _))
          | arch == ArchPPC && not (ncgPIC config)
          -> cmmExprNative referenceKind $
             CmmLit (CmmLabel (mkCmmCodeLabel rtsUnitId (fsLit "__stg_gc_enter_1")))
        CmmReg (CmmGlobal (GlobalRegUse GCFun _))
          | arch == ArchPPC && not (ncgPIC config)
          -> cmmExprNative referenceKind $
             CmmLit (CmmLabel (mkCmmCodeLabel rtsUnitId (fsLit "__stg_gc_fun")))

        other
           -> return other
