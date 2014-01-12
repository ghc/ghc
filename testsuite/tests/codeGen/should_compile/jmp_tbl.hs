{-# LANGUAGE NamedFieldPuns #-}

{-
This funny module was reduced from a failing build of stage2 using
the new code generator and the linear register allocator, with this bug:

"inplace/bin/ghc-stage1" -fPIC -dynamic  -H32m -O -Wall -H64m -O0    -package-name ghc-7.1.20110414 -hide-all-packages -i -icompiler/basicTypes -icompiler/cmm -icompiler/codeGen -icompiler/coreSyn -icompiler/deSugar -icompiler/ghci -icompiler/hsSyn -icompiler/iface -icompiler/llvmGen -icompiler/main -icompiler/nativeGen -icompiler/parser -icompiler/prelude -icompiler/profiling -icompiler/rename -icompiler/simplCore -icompiler/simplStg -icompiler/specialise -icompiler/stgSyn -icompiler/stranal -icompiler/typecheck -icompiler/types -icompiler/utils -icompiler/vectorise -icompiler/stage2/build -icompiler/stage2/build/autogen -Icompiler/stage2/build -Icompiler/stage2/build/autogen -Icompiler/../libffi/build/include -Icompiler/stage2 -Icompiler/../libraries/base/cbits -Icompiler/../libraries/base/include -Icompiler/. -Icompiler/parser -Icompiler/utils   -optP-DGHCI -optP-include -optPcompiler/stage2/build/autogen/cabal_macros.h -package Cabal-1.11.0 -package array-0.3.0.2 -package base-4.3.1.0 -package bin-package-db-0.0.0.0 -package bytestring-0.9.1.10 -package containers-0.4.0.0 -package directory-1.1.0.0 -package filepath-1.2.0.0 -package hoopl-3.8.7.0 -package hpc-0.5.0.6 -package old-time-1.0.0.6 -package process-1.0.1.4 -package template-haskell-2.5.0.0 -package unix-2.4.1.0  -Wall -fno-warn-name-shadowing -fno-warn-orphans -XHaskell98 -XNondecreasingIndentation -XCPP -XMagicHash -XUnboxedTuples -XPatternGuards -XForeignFunctionInterface -XEmptyDataDecls -XTypeSynonymInstances -XMultiParamTypeClasses -XFlexibleInstances -XRank2Types -XScopedTypeVariables -XDeriveDataTypeable -DGHCI_TABLES_NEXT_TO_CODE -DSTAGE=2 -O2 -O -DGHC_DEFAULT_NEW_CODEGEN -no-user-package-db -rtsopts     -odir compiler/stage2/build -hidir compiler/stage2/build -stubdir compiler/stage2/build -hisuf dyn_hi -osuf  dyn_o -hcsuf dyn_hc -c compiler/main/DriverPipeline.hs -o compiler/stage2/build/DriverPipeline.dyn_o  -fforce-recomp -dno-debug-output -fno-warn-unused-binds

ghc-stage1: panic! (the 'impossible' happened)
  (GHC version 7.1.20110414 for x86_64-unknown-linux):
        Cannot patch JMP_TBL

This panic only appears to show up on x86-64 and with -fPIC.  I wasn't
able to get the produced optimized C-- to crash the linear register
allocator.  To see the bug, you need some extra patches for the new code
generator, in particular, this set (which can be acquired from the
jmp_tbl_bug tag at <https://github.com/ezyang/ghc>):

    commit 7b275c93df7944f0a9b51034cf1f64e3e70582a5
    Author: Edward Z. Yang <ezyang@mit.edu>
    Date:   Thu Apr 14 21:20:21 2011 +0100

        Give manifestSP better information about the actual SP location.

        This patch fixes silliness where the SP pointer is continually
        bumped up and down.

        Signed-off-by: Edward Z. Yang <ezyang@mit.edu>

    commit 5b5add4246d3997670ae995f7d2a028db92fff95
    Author: Edward Z. Yang <ezyang@mit.edu>
    Date:   Wed Apr 13 11:16:36 2011 +0100

        Generalized assignment rewriting pass.

        This assignment rewriting pass subsumes the previous reload
        sinking pass, and also performs basic inlining.

        Signed-off-by: Edward Z. Yang <ezyang@mit.edu>

The ostensible cause is that the linear register allocator is getting
really unlucky and needs to insert a fixup block after precisely one
jump in a jump table, because the block it jumps to was processed
already.  As you can see, actually getting the linear register allocator
into this funk is /very/ difficult.

-}

module DriverPipeline (compileFile) where

import Control.Exception

import Control.Applicative (Applicative(..))
import Control.Monad (liftM, ap)

data Phase
        = Unlit ()
        | Ccpp
        | Cc
        | Cobjc
        | HCc
        | SplitAs
        | As
        | LlvmOpt
        | LlvmLlc
        | LlvmMangle
        | MergeStub
        | StopLn
  deriving (Show)

data PipeState = PipeState {
       stop_phase   :: Phase,
       src_basename :: String,
       output_spec  :: (),
       hsc_env      :: Maybe String,
       maybe_loc    :: Maybe String
  }

newtype CompPipeline a = P { unP :: PipeState -> IO (PipeState, a) }

instance Functor CompPipeline where
    fmap = liftM

instance Applicative CompPipeline where
    pure = return
    (<*>) = ap

instance Monad CompPipeline where
  return a = P $ \state -> return (state, a)
  P m >>= k = P $ \state -> do (state',a) <- m state
                               unP (k a) state'

eqPhase :: Phase -> Phase -> Bool
eqPhase (Unlit _)   (Unlit _)   = True
eqPhase Ccpp        Ccpp        = True
eqPhase Cc          Cc          = True
eqPhase HCc         HCc         = True
eqPhase SplitAs     SplitAs     = True
eqPhase As          As          = True
eqPhase LlvmOpt	    LlvmOpt 	= True
eqPhase LlvmLlc	    LlvmLlc 	= True
eqPhase LlvmMangle  LlvmMangle 	= True
eqPhase MergeStub   MergeStub   = True
eqPhase StopLn      StopLn      = True
eqPhase _           _           = False

compileFile start_phase state = do
  unP (pipeLoop start_phase) state
  getOutputFilename undefined undefined undefined undefined undefined undefined

pipeLoop phase = do
  dflags@PipeState{stop_phase} <- getPipeState
  io $ evaluate (phase `eqPhase` stop_phase)
  runPhase phase dflags
  pipeLoop phase

getOutputFilename :: Phase -> () -> String -> Maybe String -> Phase -> Maybe String -> IO String
getOutputFilename p o b md p' ml
   | p' `eqPhase` p, () <- o = undefined
   | Just l <- ml = return l
   | Just d <- md = return $ d ++ b
   | otherwise    = undefined

runPhase p _ | p `eqPhase` Cc || p `eqPhase` Ccpp || p `eqPhase` HCc || p `eqPhase` Cobjc = undefined
runPhase LlvmMangle _ = undefined
runPhase SplitAs _ = undefined
runPhase LlvmOpt _ = undefined
runPhase LlvmLlc dflags = phaseOutputFilename >> io (evaluate dflags) >> return undefined
runPhase MergeStub _ = phaseOutputFilename >> undefined
runPhase other _ = io (evaluate (show other)) >> undefined

phaseOutputFilename :: CompPipeline ()
phaseOutputFilename = do
  PipeState{stop_phase, src_basename, output_spec, maybe_loc, hsc_env} <- getPipeState
  io $ getOutputFilename stop_phase output_spec src_basename hsc_env StopLn maybe_loc

getPipeState = P $ \state -> return (state, state)
io m = P $ \state -> do a <- m; return (state, ())
