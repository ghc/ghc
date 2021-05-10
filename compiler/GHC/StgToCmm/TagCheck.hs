{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
--
-- Code generator utilities; mostly monadic
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module GHC.StgToCmm.TagCheck
  ( emitTagAssertion, emitArgTagCheck, checkArg, whenCheckTags,
    checkArgStatic, checkFunctionArgTags,checkConArgsStatic,checkConArgsDyn) where

#include "ClosureTypes.h"

import GHC.Prelude

import GHC.StgToCmm.Env
import GHC.StgToCmm.Monad
import GHC.StgToCmm.Utils
import GHC.Cmm
import GHC.Cmm.BlockId
import GHC.Cmm.Graph as CmmGraph

import GHC.Core.Type
import GHC.Types.Id
import GHC.Utils.Misc
import GHC.Utils.Outputable

import GHC.Core.DataCon
import Control.Monad
import GHC.StgToCmm.Types
import GHC.Utils.Panic (pprPanic)
import GHC.Utils.Panic.Plain (panic)
import GHC.Stg.Syntax
import GHC.StgToCmm.Closure
import GHC.Types.RepType (dataConRuntimeRepStrictness)
import GHC.Types.Basic
import GHC.Data.FastString (mkFastString)
import GHC.Cmm.Info (cmmGetClosureType)
import GHC.Cmm.Utils (mkWordCLit)

-- | Check all arguments marked as already tagged for a function
-- are tagged by inserting runtime checks.
checkFunctionArgTags :: SDoc -> Id -> [Id] -> FCode ()
checkFunctionArgTags msg f args = whenCheckTags $ do
  onJust (return ()) (idCbvMarks_maybe f) $ \marks -> do
    -- Only check args marked as strict, and only lifted ones.
    let cbv_args = filter (isLiftedRuntimeRep . idType) $ filterByList (map isMarkedCbv marks) args
    -- Get their (cmm) address
    arg_infos <- mapM getCgIdInfo cbv_args
    let arg_cmms = map idInfoToAmode arg_infos
    mapM_ (emitTagAssertion (showPprUnsafe msg))  (arg_cmms)

-- | Check all required-tagged arguments of a constructor are tagged *at compile time*.
checkConArgsStatic :: SDoc -> DataCon -> [StgArg] -> FCode ()
checkConArgsStatic msg con args = whenCheckTags $ do
  let marks = dataConRuntimeRepStrictness con
  zipWithM_ (checkArgStatic msg) marks args

-- Check all required arguments of a constructor are tagged.
-- Possible by emitting checks at runtime.
checkConArgsDyn :: SDoc -> DataCon -> [StgArg] -> FCode ()
checkConArgsDyn msg con args = whenCheckTags $ do
  let marks = dataConRuntimeRepStrictness con
  zipWithM_ (checkArg msg) (map cbvFromStrictMark marks) args

whenCheckTags :: FCode () -> FCode ()
whenCheckTags act = do
  check_tags <- stgToCmmDoTagCheck <$> getStgToCmmConfig
  when check_tags act

-- | Call barf if we failed to predict a tag correctly.
-- This is immensly useful when debugging issues in tag inference
-- as it will result in a program abort when we encounter an invalid
-- call/heap object, rather than leaving it be and segfaulting arbitrary
-- or producing invalid results.
-- We check if either:
-- * A tag is present
-- * Or the object is a PAP (for which zero is the proper tag)
emitTagAssertion :: String -> CmmExpr -> FCode ()
emitTagAssertion onWhat fun = do
  { platform <- getPlatform
  ; lret <- newBlockId
  ; lno_tag <- newBlockId
  ; lbarf <- newBlockId
  -- Check for presence of any tag.
  ; emit $ mkCbranch (cmmIsTagged platform fun)
                     lret lno_tag (Just True)
  -- If there is no tag check if we are dealing with a PAP
  ; emitLabel lno_tag
  ; emitComment (mkFastString "closereTypeCheck")
  ; align_check <- stgToCmmAlignCheck <$> getStgToCmmConfig
  ; profile <- getProfile
  ; let closure_ty = cmmGetClosureType profile align_check fun
  ; ty_reg <- newTemp (bWord platform)
  ; emitAssign (CmmLocal ty_reg) closure_ty
  ; emit $ mkCbranch (cmmEqWord platform
                        (CmmReg $ CmmLocal ty_reg)
                        (CmmLit $ mkWordCLit platform PAP))
                      lret lbarf (Just True)

  ; emitLabel lbarf
  ; emitBarf ("Tag inference failed on:" ++ onWhat)
  ; emitLabel lret
  }

emitArgTagCheck :: SDoc -> [CbvMark] -> [Id] -> FCode ()
emitArgTagCheck info marks args = whenCheckTags $ do
  mod <- getModuleName
  let cbv_args = filter (isLiftedRuntimeRep . idType) $ filterByList (map isMarkedCbv marks) args
  arg_infos <- mapM getCgIdInfo cbv_args
  let arg_cmms = map idInfoToAmode arg_infos
      mk_msg arg = showPprUnsafe (text "Untagged arg:" <> (ppr mod) <> char ':' <> info <+> ppr arg)
  zipWithM_ emitTagAssertion (map mk_msg args) (arg_cmms)

taggedCgInfo :: CgIdInfo -> Bool
taggedCgInfo cg_info
  = case lf of
      LFCon {} -> True
      LFReEntrant {} -> True
      LFUnlifted {} -> True
      LFThunk {} -> False
      LFUnknown {} -> False
      LFLetNoEscape -> panic "Let no escape binding passed to top level con"
  where
    lf = cg_lf cg_info

-- Check that one argument is properly tagged.
checkArg :: SDoc -> CbvMark -> StgArg -> FCode ()
checkArg _ NotMarkedCbv _ = return ()
checkArg msg MarkedCbv arg = whenCheckTags $
  case arg of
    StgLitArg _ -> return ()
    StgVarArg v -> do
      info <- getCgIdInfo v
      if taggedCgInfo info
          then return ()
          else case (cg_loc info) of
            CmmLoc loc -> emitTagAssertion (showPprUnsafe msg) loc
            LneLoc {} -> panic "LNE-arg"

-- Check that argument is properly tagged.
checkArgStatic :: SDoc -> StrictnessMark  -> StgArg -> FCode ()
checkArgStatic _   NotMarkedStrict _ = return ()
checkArgStatic msg MarkedStrict arg = whenCheckTags $
  case arg of
    StgLitArg _ -> return ()
    StgVarArg v -> do
      info <- getCgIdInfo v
      if taggedCgInfo info
          then return ()
          else pprPanic "Arg not tagged as expectd" (ppr msg <+> ppr arg)


