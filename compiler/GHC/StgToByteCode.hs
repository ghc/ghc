{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleContexts           #-}

--
--  (c) The University of Glasgow 2002-2006
--

-- | GHC.StgToByteCode: Generate bytecode from STG
module GHC.StgToByteCode ( UnlinkedBCO, byteCodeGen) where

import GHC.Prelude

import GHC.Driver.DynFlags
import GHC.Driver.Env

import GHC.ByteCode.Instr
import GHC.ByteCode.Asm
import GHC.ByteCode.Types

import GHC.Cmm.CallConv
import GHC.Cmm.Expr
import GHC.Cmm.Reg ( GlobalArgRegs(..) )
import GHC.Cmm.Node
import GHC.Cmm.Utils

import GHC.Platform
import GHC.Platform.Profile

import GHC.Runtime.Interpreter
import GHCi.FFI
import GHCi.RemoteTypes
import GHC.Types.Basic
import GHC.Utils.Outputable
import GHC.Types.Name
import GHC.Types.Id
import GHC.Types.ForeignCall
import GHC.Core
import GHC.Types.Literal
import GHC.Builtin.PrimOps
import GHC.Builtin.PrimOps.Ids (primOpId)
import GHC.Core.Type
import GHC.Core.Predicate( tyCoVarsOfTypesWellScoped )
import GHC.Core.TyCo.Compare (eqType)
import GHC.Types.RepType
import GHC.Core.DataCon
import GHC.Core.TyCon
import GHC.Utils.Misc
import GHC.Utils.Logger
import GHC.Types.Var.Set
import GHC.Builtin.Types.Prim
import GHC.Core.TyCo.Ppr ( pprType )
import GHC.Utils.Error
import GHC.Builtin.Uniques
import GHC.Data.FastString
import GHC.Utils.Panic
import GHC.Utils.Exception (evaluate)
import GHC.CmmToAsm.Config (platformWordWidth)
import GHC.StgToCmm.Closure ( NonVoid(..), fromNonVoid, idPrimRepU,
                              addIdReps, addArgReps,
                              assertNonVoidIds, assertNonVoidStgArgs )
import GHC.StgToCmm.Layout
import GHC.Runtime.Heap.Layout hiding (WordOff, ByteOff, wordsToBytes)
import GHC.Data.Bitmap
import GHC.Data.FlatBag as FlatBag
import GHC.Data.OrdList
import GHC.Data.Maybe
import GHC.Types.Tickish
import GHC.Types.SptEntry

import Data.List ( genericReplicate, intersperse
                 , partition, scanl', sortBy, zip4, zip6 )
import Foreign hiding (shiftL, shiftR)
import Control.Monad
import Data.Char

import GHC.Unit.Module
import qualified GHC.Unit.Home.Graph as HUG

import Data.Array
import Data.Coerce (coerce)
#if MIN_VERSION_rts(1,0,3)
import qualified Data.ByteString.Char8 as BS
#endif
import Data.Map (Map)
import Data.IntMap (IntMap)
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified GHC.Data.FiniteMap as Map
import Data.Ord
import Data.Either ( partitionEithers )

import GHC.Stg.Syntax
import qualified Data.IntSet as IntSet
import GHC.CoreToIface

-- -----------------------------------------------------------------------------
-- Generating byte code for a complete module

byteCodeGen :: HscEnv
            -> Module
            -> [CgStgTopBinding]
            -> [TyCon]
            -> Maybe ModBreaks
            -> [SptEntry]
            -> IO CompiledByteCode
byteCodeGen hsc_env this_mod binds tycs mb_modBreaks spt_entries
   = withTiming logger
                (text "GHC.StgToByteCode"<+>brackets (ppr this_mod))
                (const ()) $ do
        -- Split top-level binds into strings and others.
        -- See Note [Generating code for top-level string literal bindings].
        let (strings, lifted_binds) = partitionEithers $ do  -- list monad
                bnd <- binds
                case bnd of
                  StgTopLifted bnd      -> [Right bnd]
                  StgTopStringLit b str -> [Left (getName b, str)]
            flattenBind (StgNonRec b e) = [(b,e)]
            flattenBind (StgRec bs)     = bs

        (BcM_State{..}, proto_bcos) <-
           runBc hsc_env this_mod mb_modBreaks $ do
             let flattened_binds = concatMap flattenBind (reverse lifted_binds)
             FlatBag.fromList (fromIntegral $ length flattened_binds) <$> mapM schemeTopBind flattened_binds

        putDumpFileMaybe logger Opt_D_dump_BCOs
           "Proto-BCOs" FormatByteCode
           (vcat (intersperse (char ' ') (map ppr $ elemsFlatBag proto_bcos)))

        let mod_breaks = case modBreaks of
             Nothing -> Nothing
             Just mb -> Just mb{ modBreaks_breakInfo = breakInfo }
        cbc <- assembleBCOs profile proto_bcos tycs strings mod_breaks spt_entries

        -- Squash space leaks in the CompiledByteCode.  This is really
        -- important, because when loading a set of modules into GHCi
        -- we don't touch the CompiledByteCode until the end when we
        -- do linking.  Forcing out the thunks here reduces space
        -- usage by more than 50% when loading a large number of
        -- modules.
        evaluate (seqCompiledByteCode cbc)

        return cbc

  where dflags  = hsc_dflags hsc_env
        logger  = hsc_logger hsc_env
        profile = targetProfile dflags

{- Note [Generating code for top-level string literal bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As described in Note [Compilation plan for top-level string literals]
in GHC.Core, the core-to-core optimizer can introduce top-level Addr#
bindings to represent string literals. The creates two challenges for
the bytecode compiler: (1) compiling the bindings themselves, and
(2) compiling references to such bindings. Here is a summary on how
we deal with them:

  1. Top-level string literal bindings are separated from the rest of
     the module. Memory is not allocated until bytecode link-time, the
     bc_strs field of the CompiledByteCode result records [(Name, ByteString)]
     directly.

  2. When we encounter a reference to a top-level string literal, we
     generate a PUSH_ADDR pseudo-instruction, which is assembled to
     a PUSH_UBX instruction with a BCONPtrAddr argument.

  3. The loader accumulates string literal bindings from loaded
     bytecode in the addr_env field of the LinkerEnv.

  4. The BCO linker resolves BCONPtrAddr references by searching both
     the addr_env (to find literals defined in bytecode) and the native
     symbol table (to find literals defined in native code).

This strategy works alright, but it does have one significant problem:
we never free the memory that we allocate for the top-level strings.
In theory, we could explicitly free it when BCOs are unloaded, but
this comes with its own complications; see #22400 for why. For now,
we just accept the leak, but it would nice to find something better. -}

-- -----------------------------------------------------------------------------
-- Compilation schema for the bytecode generator

type BCInstrList = OrdList BCInstr

wordsToBytes :: Platform -> WordOff -> ByteOff
wordsToBytes platform = fromIntegral . (* platformWordSizeInBytes platform) . fromIntegral

-- Used when we know we have a whole number of words
bytesToWords :: Platform -> ByteOff -> WordOff
bytesToWords platform (ByteOff bytes) =
    let (q, r) = bytes `quotRem` (platformWordSizeInBytes platform)
    in if r == 0
           then fromIntegral q
           else pprPanic "GHC.StgToByteCode.bytesToWords"
                         (text "bytes=" <> ppr bytes)

wordSize :: Platform -> ByteOff
wordSize platform = ByteOff (platformWordSizeInBytes platform)

type Sequel = ByteOff -- back off to this depth before ENTER

type StackDepth = ByteOff

-- | Maps Ids to their stack depth. This allows us to avoid having to mess with
-- it after each push/pop.
type BCEnv = Map Id StackDepth -- To find vars on the stack

{-
ppBCEnv :: BCEnv -> SDoc
ppBCEnv p
   = text "begin-env"
     $$ nest 4 (vcat (map pp_one (sortBy cmp_snd (Map.toList p))))
     $$ text "end-env"
     where
        pp_one (var, ByteOff offset) = int offset <> colon <+> ppr var <+> ppr (bcIdArgReps var)
        cmp_snd x y = compare (snd x) (snd y)
-}

-- Create a BCO and do a spot of peephole optimisation on the insns
-- at the same time.
mkProtoBCO
   ::
    Platform
   -> Maybe Module
        -- ^ Just cur_mod <=> label with @BCO_NAME@ instruction
        -- see Note [BCO_NAME]
   -> Name
   -> BCInstrList
   -> Either  [CgStgAlt] (CgStgRhs)
                -- ^ original expression; for debugging only
   -> Int       -- ^ arity
   -> WordOff   -- ^ bitmap size
   -> [StgWord] -- ^ bitmap
   -> Bool      -- ^ True <=> it's a case continuation, rather than a function
                -- See also Note [Case continuation BCOs].
   -> ProtoBCO Name
mkProtoBCO platform _add_bco_name nm instrs_ordlist origin arity bitmap_size bitmap is_ret
   = ProtoBCO {
        protoBCOName = nm,
        protoBCOInstrs = maybe_add_bco_name $ maybe_add_stack_check peep_d,
        protoBCOBitmap = bitmap,
        protoBCOBitmapSize = fromIntegral bitmap_size,
        protoBCOArity = arity,
        protoBCOExpr = origin
      }
     where
#if MIN_VERSION_rts(1,0,3)
        maybe_add_bco_name instrs
          | Just cur_mod <- _add_bco_name =
              let str = BS.pack $ showSDocOneLine defaultSDocContext (pprFullNameWithUnique cur_mod nm)
              in BCO_NAME str : instrs
#endif
        maybe_add_bco_name instrs = instrs

        -- Overestimate the stack usage (in words) of this BCO,
        -- and if >= iNTERP_STACK_CHECK_THRESH, add an explicit
        -- stack check.  (The interpreter always does a stack check
        -- for iNTERP_STACK_CHECK_THRESH words at the start of each
        -- BCO anyway, so we only need to add an explicit one in the
        -- (hopefully rare) cases when the (overestimated) stack use
        -- exceeds iNTERP_STACK_CHECK_THRESH.
        maybe_add_stack_check instrs
           | is_ret && stack_usage < fromIntegral (pc_AP_STACK_SPLIM (platformConstants platform)) = instrs
                -- don't do stack checks at return points,
                -- everything is aggregated up to the top BCO
                -- (which must be a function).
                -- That is, unless the stack usage is >= AP_STACK_SPLIM,
                -- see bug #1466.
           | stack_usage >= fromIntegral iNTERP_STACK_CHECK_THRESH
           = STKCHECK stack_usage : instrs
           | otherwise
           = instrs     -- the supposedly common case

        -- We assume that this sum doesn't wrap
        stack_usage = sum (map bciStackUse peep_d)

        -- Merge local pushes
        peep_d = peep (fromOL instrs_ordlist)

        peep (PUSH_L off1 : PUSH_L off2 : PUSH_L off3 : rest)
           = PUSH_LLL off1 (off2-1) (off3-2) : peep rest
        peep (PUSH_L off1 : PUSH_L off2 : rest)
           = PUSH_LL off1 (off2-1) : peep rest
        peep (i:rest)
           = i : peep rest
        peep []
           = []

argBits :: Platform -> [ArgRep] -> [Bool]
argBits _        [] = []
argBits platform (rep : args)
  | isFollowableArg rep  = False : argBits platform args
  | otherwise = replicate (argRepSizeW platform rep) True ++ argBits platform args

-- -----------------------------------------------------------------------------
-- schemeTopBind

-- Compile code for the right-hand side of a top-level binding

schemeTopBind :: (Id, CgStgRhs) -> BcM (ProtoBCO Name)
schemeTopBind (id, rhs)
  | Just data_con <- isDataConWorkId_maybe id,
    isNullaryRepDataCon data_con = do
    platform <- profilePlatform <$> getProfile
    add_bco_name <- shouldAddBcoName
        -- Special case for the worker of a nullary data con.
        -- It'll look like this:        Nil = /\a -> Nil a
        -- If we feed it into schemeR, we'll get
        --      Nil = Nil
        -- because mkConAppCode treats nullary constructor applications
        -- by just re-using the single top-level definition.  So
        -- for the worker itself, we must allocate it directly.
    -- ioToBc (putStrLn $ "top level BCO")
    pure (mkProtoBCO platform add_bco_name
                       (getName id) (toOL [PACK data_con 0, RETURN P])
                       (Right rhs) 0 0 [{-no bitmap-}] False{-not alts-})

  | otherwise
  = schemeR [{- No free variables -}] (getName id, rhs)


-- -----------------------------------------------------------------------------
-- schemeR

-- Compile code for a right-hand side, to give a BCO that,
-- when executed with the free variables and arguments on top of the stack,
-- will return with a pointer to the result on top of the stack, after
-- removing the free variables and arguments.
--
-- Park the resulting BCO in the monad.  Also requires the
-- name of the variable to which this value was bound,
-- so as to give the resulting BCO a name.
--
-- The resulting ProtoBCO expects the free variables and the function arguments
-- to be in the stack frame directly before it.
schemeR :: [Id]                 -- Free vars of the RHS, ordered as they
                                -- will appear in the thunk.  Empty for
                                -- top-level things, which have no free vars.
        -> (Name, CgStgRhs)
        -> BcM (ProtoBCO Name)
schemeR fvs (nm, rhs)
   = schemeR_wrk fvs nm rhs (collect rhs)

-- If an expression is a lambda, return the
-- list of arguments to the lambda (in R-to-L order) and the
-- underlying expression

collect :: CgStgRhs -> ([Var], CgStgExpr)
collect (StgRhsClosure _ _ _ args body _) = (args, body)
collect (StgRhsCon _cc dc cnum _ticks args _typ) = ([], StgConApp dc cnum args [])

schemeR_wrk
    :: [Id]
    -> Name
    -> CgStgRhs            -- expression e, for debugging only
    -> ([Var], CgStgExpr)  -- result of collect on e
    -> BcM (ProtoBCO Name)
schemeR_wrk fvs nm original_body (args, body)
   = do
     add_bco_name <- shouldAddBcoName
     profile <- getProfile
     let
         platform  = profilePlatform profile
         all_args  = reverse args ++ fvs
         arity     = length all_args
         -- all_args are the args in reverse order.  We're compiling a function
         -- \fv1..fvn x1..xn -> e
         -- i.e. the fvs come first

         -- Stack arguments always take a whole number of words, we never pack
         -- them unlike constructor fields.
         szsb_args = map (wordsToBytes platform . idSizeW platform) all_args
         sum_szsb_args  = sum szsb_args
         -- Make a stack offset for each argument or free var -- they should
         -- appear contiguous in the stack, in order.
         p_init    = Map.fromList (zip all_args (mkStackOffsets 0 szsb_args))

         -- make the arg bitmap
         bits = argBits platform (reverse (map (idArgRep platform) all_args))
         bitmap_size = strictGenericLength bits
         bitmap = mkBitmap platform bits
     body_code <- schemeER_wrk sum_szsb_args p_init body

     pure (mkProtoBCO platform add_bco_name nm body_code (Right original_body)
                 arity bitmap_size bitmap False{-not alts-})

-- | Introduce break instructions for ticked expressions.
-- If no breakpoint information is available, the instruction is omitted.
schemeER_wrk :: StackDepth -> BCEnv -> CgStgExpr -> BcM BCInstrList
schemeER_wrk d p (StgTick (Breakpoint tick_ty tick_no fvs tick_mod) rhs) = do
  code <- schemeE d 0 p rhs
  hsc_env <- getHscEnv
  current_mod <- getCurrentModule
  mb_current_mod_breaks <- getCurrentModBreaks
  case mb_current_mod_breaks of
    -- if we're not generating ModBreaks for this module for some reason, we
    -- can't store breakpoint occurrence information.
    Nothing -> pure code
    Just current_mod_breaks -> break_info hsc_env tick_mod current_mod mb_current_mod_breaks >>= \case
      Nothing -> pure code
      Just ModBreaks {modBreaks_flags = breaks, modBreaks_module = tick_mod_ptr, modBreaks_module_unitid = tick_mod_id_ptr, modBreaks_ccs = cc_arr} -> do
        platform <- profilePlatform <$> getProfile
        let idOffSets = getVarOffSets platform d p fvs
            ty_vars   = tyCoVarsOfTypesWellScoped (tick_ty:map idType fvs)
            toWord :: Maybe (Id, WordOff) -> Maybe (Id, Word)
            toWord = fmap (\(i, wo) -> (i, fromIntegral wo))
            breakInfo  = dehydrateCgBreakInfo ty_vars (map toWord idOffSets) tick_ty

        let info_mod_ptr = modBreaks_module current_mod_breaks
            info_mod_id_ptr = modBreaks_module_unitid current_mod_breaks
        infox <- newBreakInfo breakInfo

        let cc | Just interp <- hsc_interp hsc_env
              , interpreterProfiled interp
              = cc_arr ! tick_no
              | otherwise = toRemotePtr nullPtr

        let -- cast that checks that round-tripping through Word16 doesn't change the value
            toW16 x = let r = fromIntegral x :: Word16
                      in if fromIntegral r == x
                        then r
                        else pprPanic "schemeER_wrk: breakpoint tick/info index too large!" (ppr x)
            breakInstr = BRK_FUN breaks tick_mod_ptr tick_mod_id_ptr (toW16 tick_no) info_mod_ptr info_mod_id_ptr (toW16 infox) cc
        return $ breakInstr `consOL` code
schemeER_wrk d p rhs = schemeE d 0 p rhs

-- | Determine the GHCi-allocated 'BreakArray' and module pointer for the module
-- from which the breakpoint originates.
-- These are stored in 'ModBreaks' as remote pointers in order to allow the BCOs
-- to refer to pointers in GHCi's address space.
-- They are initialized in 'GHC.HsToCore.Breakpoints.mkModBreaks', called by
-- 'GHC.HsToCore.deSugar'.
--
-- Breakpoints might be disabled because we're in TH, because
-- @-fno-break-points@ was specified, or because a module was reloaded without
-- reinitializing 'ModBreaks'.
--
-- If the module stored in the breakpoint is the currently processed module, use
-- the 'ModBreaks' from the state.
-- If that is 'Nothing', consider breakpoints to be disabled and skip the
-- instruction.
--
-- If the breakpoint is inlined from another module, look it up in the HUG (home unit graph).
-- If the module doesn't exist there, or if the 'ModBreaks' value is
-- uninitialized, skip the instruction (i.e. return Nothing).
break_info ::
  HscEnv ->
  Module ->
  Module ->
  Maybe ModBreaks ->
  BcM (Maybe ModBreaks)
break_info hsc_env mod current_mod current_mod_breaks
  | mod == current_mod
  = pure current_mod_breaks
  | otherwise
  = ioToBc (HUG.lookupHugByModule mod (hsc_HUG hsc_env)) >>= \case
      Just hp -> pure $ getModBreaks hp
      Nothing -> pure Nothing

getVarOffSets :: Platform -> StackDepth -> BCEnv -> [Id] -> [Maybe (Id, WordOff)]
getVarOffSets platform depth env = map getOffSet
  where
    getOffSet id = case lookupBCEnv_maybe id env of
        Nothing     -> Nothing
        Just offset ->
            -- michalt: I'm not entirely sure why we need the stack
            -- adjustment by 2 here. I initially thought that there's
            -- something off with getIdValFromApStack (the only user of this
            -- value), but it looks ok to me. My current hypothesis is that
            -- this "adjustment" is needed due to stack manipulation for
            -- BRK_FUN in Interpreter.c In any case, this is used only when
            -- we trigger a breakpoint.
            let !var_depth_ws = bytesToWords platform (depth - offset) + 2
            in Just (id, var_depth_ws)

fvsToEnv :: BCEnv -> CgStgRhs -> [Id]
-- Takes the free variables of a right-hand side, and
-- delivers an ordered list of the local variables that will
-- be captured in the thunk for the RHS
-- The BCEnv argument tells which variables are in the local
-- environment: these are the ones that should be captured
--
-- The code that constructs the thunk, and the code that executes
-- it, have to agree about this layout

fvsToEnv p rhs =  [v | v <- dVarSetElems $ freeVarsOfRhs rhs,
                       v `Map.member` p]

-- -----------------------------------------------------------------------------
-- schemeE

-- Returning an unlifted value.
-- Heave it on the stack, SLIDE, and RETURN.
returnUnliftedAtom
    :: StackDepth
    -> Sequel
    -> BCEnv
    -> StgArg
    -> BcM BCInstrList
returnUnliftedAtom d s p e = do
    let reps = stgArgRep e
    (push, szb) <- pushAtom d p e
    ret <- returnUnliftedReps d s szb reps
    return (push `appOL` ret)

-- return an unlifted value from the top of the stack
returnUnliftedReps
    :: StackDepth
    -> Sequel
    -> ByteOff    -- size of the thing we're returning
    -> [PrimRep]  -- representations
    -> BcM BCInstrList
returnUnliftedReps d s szb reps = do
    profile <- getProfile
    let platform = profilePlatform profile
    ret <- case reps of
             -- use RETURN for nullary/unary representations
             []    -> return (unitOL $ RETURN V)
             [rep] -> return (unitOL $ RETURN (toArgRep platform rep))
             -- otherwise use RETURN_TUPLE with a tuple descriptor
             nv_reps -> do
               let (call_info, args_offsets) = layoutNativeCall profile NativeTupleReturn 0 id nv_reps
                   tuple_bco = tupleBCO platform call_info args_offsets
               return $ PUSH_UBX (mkNativeCallInfoLit platform call_info) 1 `consOL`
                        PUSH_BCO tuple_bco `consOL`
                        unitOL RETURN_TUPLE
    return ( mkSlideB platform szb (d - s) -- clear to sequel
             `appOL` ret)                 -- go

-- construct and return an unboxed tuple
returnUnboxedTuple
    :: StackDepth
    -> Sequel
    -> BCEnv
    -> [StgArg]
    -> BcM BCInstrList
returnUnboxedTuple d s p es = do
    profile <- getProfile
    let platform = profilePlatform profile
        (call_info, tuple_components) = layoutNativeCall profile
                                                         NativeTupleReturn
                                                         d
                                                         stgArgRepU
                                                         es
        go _   pushes [] = return (reverse pushes)
        go !dd pushes ((a, off):cs) = do (push, szb) <- pushAtom dd p a
                                         massert (off == dd + szb)
                                         go (dd + szb) (push:pushes) cs
    pushes <- go d [] tuple_components

    ret <- returnUnliftedReps d
                              s
                              (wordsToBytes platform $ nativeCallSize call_info)
                              (map stgArgRepU es)
    return (mconcat pushes `appOL` ret)

-- Compile code to apply the given expression to the remaining args
-- on the stack, returning a HNF.
schemeE :: StackDepth -> Sequel -> BCEnv -> CgStgExpr -> BcM BCInstrList
schemeE d s p (StgLit lit) = returnUnliftedAtom d s p (StgLitArg lit)
schemeE d s p (StgApp x [])
   | isUnliftedType (idType x) = returnUnliftedAtom d s p (StgVarArg x)
-- Delegate tail-calls to schemeT.
schemeE d s p e@(StgApp {}) = schemeT d s p e
schemeE d s p e@(StgConApp {}) = schemeT d s p e
schemeE d s p e@(StgOpApp {}) = schemeT d s p e
schemeE d s p (StgLetNoEscape xlet bnd body)
   = schemeE d s p (StgLet xlet bnd body)
schemeE d s p (StgLet _xlet
                      (StgNonRec x (StgRhsCon _cc data_con _cnum _ticks args _typ))
                      body)
   = do -- Special case for a non-recursive let whose RHS is a
        -- saturated constructor application.
        -- Just allocate the constructor and carry on
        alloc_code <- mkConAppCode d s p data_con args
        platform <- targetPlatform <$> getDynFlags
        let !d2 = d + wordSize platform
        body_code <- schemeE d2 s (Map.insert x d2 p) body
        return (alloc_code `appOL` body_code)
-- General case for let.  Generates correct, if inefficient, code in
-- all situations.
schemeE d s p (StgLet _ext binds body) = do
     platform <- targetPlatform <$> getDynFlags
     let (xs,rhss) = case binds of StgNonRec x rhs  -> ([x],[rhs])
                                   StgRec xs_n_rhss -> unzip xs_n_rhss
         n_binds = strictGenericLength xs

         fvss  = map (fvsToEnv p') rhss

         -- Sizes of free vars
         size_w = idSizeW platform
         sizes = map (\rhs_fvs -> sum (map size_w rhs_fvs)) fvss

         -- the arity of each rhs
         arities = map (strictGenericLength . fst . collect) rhss

         -- This p', d' defn is safe because all the items being pushed
         -- are ptrs, so all have size 1 word.  d' and p' reflect the stack
         -- after the closures have been allocated in the heap (but not
         -- filled in), and pointers to them parked on the stack.
         offsets = mkStackOffsets d (genericReplicate n_binds (wordSize platform))
         p' = Map.insertList (zipEqual xs offsets) p
         d' = d + wordsToBytes platform n_binds

         -- ToDo: don't build thunks for things with no free variables
         build_thunk
             :: StackDepth
             -> [Id]
             -> WordOff
             -> ProtoBCO Name
             -> WordOff
             -> HalfWord
             -> BcM BCInstrList
         build_thunk _ [] size bco off arity
            = return (PUSH_BCO bco `consOL` unitOL (mkap (off+size) (fromIntegral size)))
           where
                mkap | arity == 0 = MKAP
                     | otherwise  = MKPAP
         build_thunk dd (fv:fvs) size bco off arity = do
              (push_code, pushed_szb) <- pushAtom dd p' (StgVarArg fv)
              more_push_code <-
                  build_thunk (dd + pushed_szb) fvs size bco off arity
              return (push_code `appOL` more_push_code)

         alloc_code = toOL (zipWith mkAlloc sizes arities)
           where mkAlloc sz 0
                    | is_tick     = ALLOC_AP_NOUPD (fromIntegral sz)
                    | otherwise   = ALLOC_AP (fromIntegral sz)
                 mkAlloc sz arity = ALLOC_PAP arity (fromIntegral sz)

         is_tick = case binds of
                     StgNonRec id _ -> occNameFS (getOccName id) == tickFS
                     _other -> False

         compile_bind d' fvs x (rhs::CgStgRhs) size arity off = do
                bco <- schemeR fvs (getName x,rhs)
                build_thunk d' fvs size bco off arity

         compile_binds =
            [ compile_bind d' fvs x rhs size arity n
            | (fvs, x, rhs, size, arity, n) <-
                zip6 fvss xs rhss sizes arities [n_binds, n_binds-1 .. 1]
            ]
     body_code <- schemeE d' s p' body
     thunk_codes <- sequence compile_binds
     return (alloc_code `appOL` concatOL thunk_codes `appOL` body_code)

schemeE _d _s _p (StgTick (Breakpoint _ bp_id _ _) _rhs)
   = panic ("schemeE: Breakpoint without let binding: " ++
            show bp_id ++
            " forgot to run bcPrep?")

-- ignore other kinds of tick
schemeE d s p (StgTick _ rhs) = schemeE d s p rhs

-- no alts: scrut is guaranteed to diverge
schemeE d s p (StgCase scrut _ _ []) = schemeE d s p scrut

schemeE d s p (StgCase scrut bndr _ alts)
   = doCase d s p scrut bndr alts


{-
   Ticked Expressions
   ------------------

  The idea is that the "breakpoint<n,fvs> E" is really just an annotation on
  the code. When we find such a thing, we pull out the useful information,
  and then compile the code as if it was just the expression E.
-}

-- Compile code to do a tail call.  Specifically, push the fn,
-- slide the on-stack app back down to the sequel depth,
-- and enter.  Four cases:
--
-- 0.  (Nasty hack).
--     An application "GHC.Prim.tagToEnum# <type> unboxed-int".
--     The int will be on the stack.  Generate a code sequence
--     to convert it to the relevant constructor, SLIDE and ENTER.
--
-- 1.  The fn denotes a ccall.  Defer to generateCCall.
--
-- 2.  An unboxed tuple: push the components on the top of
--     the stack and return.
--
-- 3.  Application of a constructor, by defn saturated.
--     Split the args into ptrs and non-ptrs, and push the nonptrs,
--     then the ptrs, and then do PACK and RETURN.
--
-- 4.  Otherwise, it must be a function call.  Push the args
--     right to left, SLIDE and ENTER.

schemeT :: StackDepth   -- Stack depth
        -> Sequel       -- Sequel depth
        -> BCEnv        -- stack env
        -> CgStgExpr
        -> BcM BCInstrList

   -- Case 0
schemeT d s p app
   | Just (arg, constr_names) <- maybe_is_tagToEnum_call app
   = implement_tagToId d s p arg constr_names

   -- Case 1
schemeT d s p (StgOpApp (StgFCallOp (CCall ccall_spec) _ty) args result_ty)
   = if isSupportedCConv ccall_spec
      then generateCCall d s p ccall_spec result_ty args
      else unsupportedCConvException

schemeT d s p (StgOpApp (StgPrimOp op) args _ty) = do
  profile <- getProfile
  let platform = profilePlatform profile
  case doPrimOp platform op d s p args of
    -- Can we do this right in the interpreter?
    Just prim_code -> prim_code
    -- Otherwise we have to do a call to the primop wrapper instead :(
    _         -> doTailCall d s p (primOpId op) (reverse args)

schemeT d s p (StgOpApp (StgPrimCallOp (PrimCall label unit)) args result_ty)
   = generatePrimCall d s p label (Just unit) result_ty args

schemeT d s p (StgConApp con _cn args _tys)
   -- Case 2: Unboxed tuple
   | isUnboxedTupleDataCon con || isUnboxedSumDataCon con
   = returnUnboxedTuple d s p args

   -- Case 3: Ordinary data constructor
   | otherwise
   = do alloc_con <- mkConAppCode d s p con args
        platform <- profilePlatform <$> getProfile
        return (alloc_con         `appOL`
                mkSlideW 1 (bytesToWords platform $ d - s) `snocOL` RETURN P)

   -- Case 4: Tail call of function
schemeT d s p (StgApp fn args)
   = doTailCall d s p fn (reverse args)

schemeT _ _ _ e = pprPanic "GHC.StgToByteCode.schemeT"
                           (pprStgExpr shortStgPprOpts e)

-- -----------------------------------------------------------------------------
-- Generate code to build a constructor application,
-- leaving it on top of the stack

mkConAppCode
    :: StackDepth
    -> Sequel
    -> BCEnv
    -> DataCon                  -- The data constructor
    -> [StgArg]                 -- Args, in *reverse* order
    -> BcM BCInstrList
mkConAppCode orig_d _ p con args = app_code
  where
    app_code = do
        profile <- getProfile
        let platform = profilePlatform profile

            non_voids =
                addArgReps (assertNonVoidStgArgs args)
            (_, _, args_offsets) =
                mkVirtHeapOffsetsWithPadding profile StdHeader non_voids

            do_pushery !d (arg : args) = do
                (push, arg_bytes) <- case arg of
                    (Padding l _) -> return $! pushPadding (ByteOff l)
                    (FieldOff a _) -> pushConstrAtom d p (fromNonVoid a)
                more_push_code <- do_pushery (d + arg_bytes) args
                return (push `appOL` more_push_code)
            do_pushery !d [] = do
                let !n_arg_words = bytesToWords platform (d - orig_d)
                return (unitOL (PACK con n_arg_words))

        -- Push on the stack in the reverse order.
        do_pushery orig_d (reverse args_offsets)

-- -----------------------------------------------------------------------------
-- Generate code for a tail-call

doTailCall
    :: StackDepth
    -> Sequel
    -> BCEnv
    -> Id
    -> [StgArg]
    -> BcM BCInstrList
doTailCall init_d s p fn args = do
   platform <- profilePlatform <$> getProfile

   -- Do tail call only after
   do_pushes init_d args (map (atomRep platform) args)

  where
  do_pushes !d [] reps = do
        assert (null reps) return ()
        (push_fn, sz) <- pushAtom d p (StgVarArg fn)
        platform <- profilePlatform <$> getProfile
        assert (sz == wordSize platform) return ()
        let slide = mkSlideB platform (d - init_d + wordSize platform) (init_d - s)
        return (push_fn `appOL` (slide `appOL` unitOL ENTER))
  do_pushes !d args reps = do
      let (push_apply, n, rest_of_reps) = findPushSeq reps
          (these_args, rest_of_args) = splitAt n args
      (next_d, push_code) <- push_seq d these_args
      platform <- profilePlatform <$> getProfile
      instrs <- do_pushes (next_d + wordSize platform) rest_of_args rest_of_reps
      --                          ^^^ for the PUSH_APPLY_ instruction
      return (push_code `appOL` (push_apply `consOL` instrs))

  push_seq d [] = return (d, nilOL)
  push_seq d (arg:args) = do
    (push_code, sz) <- pushAtom d p arg
    (final_d, more_push_code) <- push_seq (d + sz) args
    return (final_d, push_code `appOL` more_push_code)

doPrimOp  :: Platform
          -> PrimOp
          -> StackDepth
          -> Sequel
          -> BCEnv
          -> [StgArg]
          -> Maybe (BcM BCInstrList)
doPrimOp platform op init_d s p args =
  case op of
    IntAddOp -> sizedPrimOp OP_ADD
    Int64AddOp -> only64bit $ sizedPrimOp OP_ADD
    Int32AddOp -> sizedPrimOp OP_ADD
    Int16AddOp -> sizedPrimOp OP_ADD
    Int8AddOp -> sizedPrimOp OP_ADD
    WordAddOp -> sizedPrimOp OP_ADD
    Word64AddOp -> only64bit $ sizedPrimOp OP_ADD
    Word32AddOp -> sizedPrimOp OP_ADD
    Word16AddOp -> sizedPrimOp OP_ADD
    Word8AddOp -> sizedPrimOp OP_ADD
    AddrAddOp -> sizedPrimOp OP_ADD

    IntMulOp -> sizedPrimOp OP_MUL
    Int64MulOp -> only64bit $ sizedPrimOp OP_MUL
    Int32MulOp -> sizedPrimOp OP_MUL
    Int16MulOp -> sizedPrimOp OP_MUL
    Int8MulOp -> sizedPrimOp OP_MUL
    WordMulOp -> sizedPrimOp OP_MUL
    Word64MulOp -> only64bit $ sizedPrimOp OP_MUL
    Word32MulOp -> sizedPrimOp OP_MUL
    Word16MulOp -> sizedPrimOp OP_MUL
    Word8MulOp -> sizedPrimOp OP_MUL

    IntSubOp -> sizedPrimOp OP_SUB
    WordSubOp -> sizedPrimOp OP_SUB
    Int64SubOp -> only64bit $ sizedPrimOp OP_SUB
    Int32SubOp -> sizedPrimOp OP_SUB
    Int16SubOp -> sizedPrimOp OP_SUB
    Int8SubOp -> sizedPrimOp OP_SUB
    Word64SubOp -> only64bit $ sizedPrimOp OP_SUB
    Word32SubOp -> sizedPrimOp OP_SUB
    Word16SubOp -> sizedPrimOp OP_SUB
    Word8SubOp -> sizedPrimOp OP_SUB
    AddrSubOp -> sizedPrimOp OP_SUB

    IntAndOp -> sizedPrimOp OP_AND
    WordAndOp -> sizedPrimOp OP_AND
    Word64AndOp -> only64bit $ sizedPrimOp OP_AND
    Word32AndOp -> sizedPrimOp OP_AND
    Word16AndOp -> sizedPrimOp OP_AND
    Word8AndOp -> sizedPrimOp OP_AND

    IntNotOp -> sizedPrimOp OP_NOT
    WordNotOp -> sizedPrimOp OP_NOT
    Word64NotOp -> only64bit $ sizedPrimOp OP_NOT
    Word32NotOp -> sizedPrimOp OP_NOT
    Word16NotOp -> sizedPrimOp OP_NOT
    Word8NotOp -> sizedPrimOp OP_NOT

    IntXorOp -> sizedPrimOp OP_XOR
    WordXorOp -> sizedPrimOp OP_XOR
    Word64XorOp -> only64bit $ sizedPrimOp OP_XOR
    Word32XorOp -> sizedPrimOp OP_XOR
    Word16XorOp -> sizedPrimOp OP_XOR
    Word8XorOp -> sizedPrimOp OP_XOR

    IntOrOp -> sizedPrimOp OP_OR
    WordOrOp -> sizedPrimOp OP_OR
    Word64OrOp -> only64bit $ sizedPrimOp OP_OR
    Word32OrOp -> sizedPrimOp OP_OR
    Word16OrOp -> sizedPrimOp OP_OR
    Word8OrOp -> sizedPrimOp OP_OR

    WordSllOp   -> sizedPrimOp OP_SHL
    Word64SllOp -> only64bit $ sizedPrimOp OP_SHL -- check 32bit platform
    Word32SllOp -> sizedPrimOp OP_SHL
    Word16SllOp -> sizedPrimOp OP_SHL
    Word8SllOp -> sizedPrimOp OP_SHL
    IntSllOp    -> sizedPrimOp OP_SHL
    Int64SllOp  -> only64bit $ sizedPrimOp OP_SHL
    Int32SllOp  -> sizedPrimOp OP_SHL
    Int16SllOp  -> sizedPrimOp OP_SHL
    Int8SllOp  -> sizedPrimOp OP_SHL

    WordSrlOp   -> sizedPrimOp OP_LSR
    Word64SrlOp -> only64bit $ sizedPrimOp OP_LSR
    Word32SrlOp -> sizedPrimOp OP_LSR
    Word16SrlOp -> sizedPrimOp OP_LSR
    Word8SrlOp -> sizedPrimOp OP_LSR
    IntSrlOp    -> sizedPrimOp OP_LSR
    Int64SrlOp  -> only64bit $ sizedPrimOp OP_LSR -- check 32bit platform
    Int32SrlOp  -> sizedPrimOp OP_LSR
    Int16SrlOp  -> sizedPrimOp OP_LSR
    Int8SrlOp  -> sizedPrimOp OP_LSR

    IntSraOp -> sizedPrimOp OP_ASR
    Int64SraOp -> only64bit $ sizedPrimOp OP_ASR -- check 32bit platform
    Int32SraOp -> sizedPrimOp OP_ASR
    Int16SraOp -> sizedPrimOp OP_ASR
    Int8SraOp -> sizedPrimOp OP_ASR


    IntNeOp -> sizedPrimOp OP_NEQ
    Int64NeOp -> only64bit $ sizedPrimOp OP_NEQ
    Int32NeOp -> sizedPrimOp OP_NEQ
    Int16NeOp -> sizedPrimOp OP_NEQ
    Int8NeOp -> sizedPrimOp OP_NEQ
    WordNeOp -> sizedPrimOp OP_NEQ
    Word64NeOp -> only64bit $ sizedPrimOp OP_NEQ
    Word32NeOp -> sizedPrimOp OP_NEQ
    Word16NeOp -> sizedPrimOp OP_NEQ
    Word8NeOp -> sizedPrimOp OP_NEQ
    AddrNeOp -> sizedPrimOp OP_NEQ

    IntEqOp -> sizedPrimOp OP_EQ
    Int64EqOp -> only64bit $ sizedPrimOp OP_EQ
    Int32EqOp -> sizedPrimOp OP_EQ
    Int16EqOp -> sizedPrimOp OP_EQ
    Int8EqOp -> sizedPrimOp OP_EQ
    WordEqOp -> sizedPrimOp OP_EQ
    Word64EqOp -> only64bit $ sizedPrimOp OP_EQ
    Word32EqOp -> sizedPrimOp OP_EQ
    Word16EqOp -> sizedPrimOp OP_EQ
    Word8EqOp -> sizedPrimOp OP_EQ
    AddrEqOp -> sizedPrimOp OP_EQ
    CharEqOp -> sizedPrimOp OP_EQ

    IntLtOp -> sizedPrimOp OP_S_LT
    Int64LtOp -> only64bit $ sizedPrimOp OP_S_LT
    Int32LtOp -> sizedPrimOp OP_S_LT
    Int16LtOp -> sizedPrimOp OP_S_LT
    Int8LtOp -> sizedPrimOp OP_S_LT
    WordLtOp -> sizedPrimOp OP_U_LT
    Word64LtOp -> only64bit $ sizedPrimOp OP_U_LT
    Word32LtOp -> sizedPrimOp OP_U_LT
    Word16LtOp -> sizedPrimOp OP_U_LT
    Word8LtOp -> sizedPrimOp OP_U_LT
    AddrLtOp -> sizedPrimOp OP_U_LT
    CharLtOp -> sizedPrimOp OP_U_LT

    IntGeOp -> sizedPrimOp OP_S_GE
    Int64GeOp -> only64bit $ sizedPrimOp OP_S_GE
    Int32GeOp -> sizedPrimOp OP_S_GE
    Int16GeOp -> sizedPrimOp OP_S_GE
    Int8GeOp -> sizedPrimOp OP_S_GE
    WordGeOp -> sizedPrimOp OP_U_GE
    Word64GeOp -> only64bit $ sizedPrimOp OP_U_GE
    Word32GeOp -> sizedPrimOp OP_U_GE
    Word16GeOp -> sizedPrimOp OP_U_GE
    Word8GeOp -> sizedPrimOp OP_U_GE
    AddrGeOp -> sizedPrimOp OP_U_GE
    CharGeOp -> sizedPrimOp OP_U_GE

    IntGtOp -> sizedPrimOp OP_S_GT
    Int64GtOp -> only64bit $ sizedPrimOp OP_S_GT
    Int32GtOp -> sizedPrimOp OP_S_GT
    Int16GtOp -> sizedPrimOp OP_S_GT
    Int8GtOp -> sizedPrimOp OP_S_GT
    WordGtOp -> sizedPrimOp OP_U_GT
    Word64GtOp -> only64bit $ sizedPrimOp OP_U_GT
    Word32GtOp -> sizedPrimOp OP_U_GT
    Word16GtOp -> sizedPrimOp OP_U_GT
    Word8GtOp -> sizedPrimOp OP_U_GT
    AddrGtOp -> sizedPrimOp OP_U_GT
    CharGtOp -> sizedPrimOp OP_U_GT

    IntLeOp -> sizedPrimOp OP_S_LE
    Int64LeOp -> only64bit $ sizedPrimOp OP_S_LE
    Int32LeOp -> sizedPrimOp OP_S_LE
    Int16LeOp -> sizedPrimOp OP_S_LE
    Int8LeOp -> sizedPrimOp OP_S_LE
    WordLeOp -> sizedPrimOp OP_U_LE
    Word64LeOp -> only64bit $ sizedPrimOp OP_U_LE
    Word32LeOp -> sizedPrimOp OP_U_LE
    Word16LeOp -> sizedPrimOp OP_U_LE
    Word8LeOp -> sizedPrimOp OP_U_LE
    AddrLeOp -> sizedPrimOp OP_U_LE
    CharLeOp -> sizedPrimOp OP_U_LE

    IntNegOp -> sizedPrimOp OP_NEG
    Int64NegOp -> only64bit $ sizedPrimOp OP_NEG
    Int32NegOp -> sizedPrimOp OP_NEG
    Int16NegOp -> sizedPrimOp OP_NEG
    Int8NegOp -> sizedPrimOp OP_NEG

    IntToWordOp     -> mk_conv (platformWordWidth platform)
    WordToIntOp     -> mk_conv (platformWordWidth platform)
    Int8ToWord8Op   -> mk_conv W8
    Word8ToInt8Op   -> mk_conv W8
    Int16ToWord16Op -> mk_conv W16
    Word16ToInt16Op -> mk_conv W16
    Int32ToWord32Op -> mk_conv W32
    Word32ToInt32Op -> mk_conv W32
    Int64ToWord64Op -> only64bit $ mk_conv W64
    Word64ToInt64Op -> only64bit $ mk_conv W64
    IntToAddrOp     -> mk_conv (platformWordWidth platform)
    AddrToIntOp     -> mk_conv (platformWordWidth platform)
    ChrOp           -> mk_conv (platformWordWidth platform)   -- Int# and Char# are rep'd the same
    OrdOp           -> mk_conv (platformWordWidth platform)

    -- Memory primops, expand the ghci-mem-primops test if you add more.
    IndexOffAddrOp_Word8 ->  primOpWithRep (OP_INDEX_ADDR W8) W8
    IndexOffAddrOp_Word16 -> primOpWithRep (OP_INDEX_ADDR W16) W16
    IndexOffAddrOp_Word32 -> primOpWithRep (OP_INDEX_ADDR W32) W32
    IndexOffAddrOp_Word64 -> only64bit $ primOpWithRep (OP_INDEX_ADDR W64) W64

    _ -> Nothing
  where
    only64bit = if platformWordWidth platform == W64 then id else const Nothing
    primArg1Width :: StgArg -> Width
    primArg1Width arg
      | rep <- (stgArgRepU arg)
      = case rep of
        AddrRep -> platformWordWidth platform
        IntRep -> platformWordWidth platform
        WordRep -> platformWordWidth platform

        Int64Rep -> W64
        Word64Rep -> W64

        Int32Rep -> W32
        Word32Rep -> W32

        Int16Rep -> W16
        Word16Rep -> W16

        Int8Rep -> W8
        Word8Rep -> W8

        FloatRep -> unexpectedRep
        DoubleRep -> unexpectedRep

        BoxedRep{} -> unexpectedRep
        VecRep{} -> unexpectedRep
      where
        unexpectedRep = panic "doPrimOp: Unexpected argument rep"


    -- TODO: The slides for the result need to be two words on 32bit for 64bit ops.
    mkNReturn width
      | W64 <- width = RETURN L -- L works for 64 bit on any platform
      | otherwise = RETURN N -- <64bit width, fits in word on all platforms

    mkSlideWords width = if platformWordWidth platform < width then 2 else 1

    -- Push args, execute primop, slide, return_N
    -- Decides width of operation based on first argument.
    sizedPrimOp op_inst = Just $ do
      let width = primArg1Width (head args)
      prim_code <- mkPrimOpCode init_d s p (op_inst width) $ args
      let slide = mkSlideW (mkSlideWords width) (bytesToWords platform $ init_d - s) `snocOL` mkNReturn width
      return $ prim_code `appOL` slide

    -- primOpWithRep op w => operation @op@ resulting in result @w@ wide.
    primOpWithRep :: BCInstr -> Width -> Maybe (BcM (OrdList BCInstr))
    primOpWithRep op_inst result_width = Just $ do
      prim_code <- mkPrimOpCode init_d s p op_inst $ args
      let slide = mkSlideW (mkSlideWords result_width) (bytesToWords platform $ init_d - s) `snocOL` mkNReturn result_width
      return $ prim_code `appOL` slide

    -- Coerce the argument, requires them to be the same size
    mk_conv :: Width -> Maybe (BcM (OrdList BCInstr))
    mk_conv target_width = Just $ do
      let width = primArg1Width (head args)
      massert (width == target_width)
      (push_code, _bytes) <- pushAtom init_d p (head args)
      let slide = mkSlideW (mkSlideWords target_width) (bytesToWords platform $ init_d - s) `snocOL` mkNReturn target_width
      return $ push_code `appOL` slide

-- Push the arguments on the stack and emit the given instruction
-- Pushes at least one word per non void arg.
mkPrimOpCode
    :: StackDepth
    -> Sequel
    -> BCEnv
    -> BCInstr                  -- The operator
    -> [StgArg]                 -- Args, in *reverse* order (must be fully applied)
    -> BcM BCInstrList
mkPrimOpCode orig_d _ p op_inst args = app_code
  where
    app_code = do
        profile <- getProfile
        let _platform = profilePlatform profile

            do_pushery :: StackDepth -> [StgArg] -> BcM BCInstrList
            do_pushery !d (arg : args) = do
                (push,arg_bytes) <- pushAtom d p arg
                more_push_code <- do_pushery (d + arg_bytes) args
                return (push `appOL` more_push_code)
            do_pushery !_d [] = do
                return (unitOL op_inst)

        -- Push on the stack in the reverse order.
        do_pushery orig_d (reverse args)

-- v. similar to CgStackery.findMatch, ToDo: merge
findPushSeq :: [ArgRep] -> (BCInstr, Int, [ArgRep])
findPushSeq (P: P: P: P: P: P: rest)
  = (PUSH_APPLY_PPPPPP, 6, rest)
findPushSeq (P: P: P: P: P: rest)
  = (PUSH_APPLY_PPPPP, 5, rest)
findPushSeq (P: P: P: P: rest)
  = (PUSH_APPLY_PPPP, 4, rest)
findPushSeq (P: P: P: rest)
  = (PUSH_APPLY_PPP, 3, rest)
findPushSeq (P: P: rest)
  = (PUSH_APPLY_PP, 2, rest)
findPushSeq (P: rest)
  = (PUSH_APPLY_P, 1, rest)
findPushSeq (V: rest)
  = (PUSH_APPLY_V, 1, rest)
findPushSeq (N: rest)
  = (PUSH_APPLY_N, 1, rest)
findPushSeq (F: rest)
  = (PUSH_APPLY_F, 1, rest)
findPushSeq (D: rest)
  = (PUSH_APPLY_D, 1, rest)
findPushSeq (L: rest)
  = (PUSH_APPLY_L, 1, rest)
findPushSeq argReps
  | any (`elem` [V16, V32, V64]) argReps
  = sorry "SIMD vector operations are not available in GHCi"
findPushSeq _
  = panic "GHC.StgToByteCode.findPushSeq"

-- -----------------------------------------------------------------------------
-- Case expressions

-- | Generate ByteCode for a case expression.
--
-- Note that case BCOs may be "nested" within other parent BCOs and refer to
-- its parent's variables (as in the 'BCEnv' contains variables from parent
-- frames). For more details about the interaction between case BCOs and their
-- parent frames see Note [Case continuation BCOs].
doCase
    :: StackDepth
    -> Sequel
    -> BCEnv
    -> CgStgExpr
    -> Id
    -> [CgStgAlt]
    -> BcM BCInstrList
doCase d s p scrut bndr alts
  = do
     profile <- getProfile
     hsc_env <- getHscEnv
     let
        platform = profilePlatform profile

        -- Are we dealing with an unboxed tuple with a tuple return frame?
        --
        -- 'Simple' tuples with at most one non-void component,
        -- like (# Word# #) or (# Int#, State# RealWorld #) do not have a
        -- tuple return frame. This is because (# foo #) and (# foo, Void# #)
        -- have the same runtime rep. We have more efficient specialized
        -- return frames for the situations with one non-void element.

        non_void_arg_reps = typeArgReps platform bndr_ty
        ubx_tuple_frame =
          (isUnboxedTupleType bndr_ty || isUnboxedSumType bndr_ty) &&
          length non_void_arg_reps > 1

        profiling
          | Just interp <- hsc_interp hsc_env
          = interpreterProfiled interp
          | otherwise = False

        -- Top of stack is the return itbl, as usual.
        -- underneath it is the pointer to the alt_code BCO.
        -- When an alt is entered, it assumes the returned value is
        -- on top of the itbl; see Note [Return convention for non-tuple values]
        -- for details.
        ret_frame_size_b :: StackDepth
        ret_frame_size_b | ubx_tuple_frame =
                             (if profiling then 5 else 4) * wordSize platform
                         | otherwise = 2 * wordSize platform

        -- The stack space used to save/restore the CCCS when profiling
        save_ccs_size_b | profiling &&
                          not ubx_tuple_frame = 2 * wordSize platform
                        | otherwise = 0

        -- The size of the return frame info table pointer if one exists
        unlifted_itbl_size_b :: StackDepth
        unlifted_itbl_size_b | ubx_tuple_frame = wordSize platform
                             | otherwise       = 0

        (bndr_size, call_info, args_offsets)
           | ubx_tuple_frame =
               let bndr_reps = typePrimRep (idType bndr)
                   (call_info, args_offsets) =
                       layoutNativeCall profile NativeTupleReturn 0 id bndr_reps
               in ( wordsToBytes platform (nativeCallSize call_info)
                  , call_info
                  , args_offsets
                  )
           | otherwise = ( wordsToBytes platform (idSizeW platform bndr)
                         , voidTupleReturnInfo
                         , []
                         )

        -- depth of stack after the return value has been pushed
        d_bndr =
            d + ret_frame_size_b + bndr_size

        -- depth of stack after the extra info table for an unlifted return
        -- has been pushed, if any.  This is the stack depth at the
        -- continuation.
        d_alts = d + ret_frame_size_b + bndr_size + unlifted_itbl_size_b

        -- Env in which to compile the alts, not including
        -- any vars bound by the alts themselves
        p_alts = Map.insert bndr d_bndr p

        bndr_ty = idType bndr
        isAlgCase = isAlgType bndr_ty

        -- given an alt, return a discr and code for it.
        codeAlt :: CgStgAlt -> BcM (Discr, BCInstrList)
        codeAlt GenStgAlt{alt_con=DEFAULT,alt_bndrs=_,alt_rhs=rhs}
           = do rhs_code <- schemeE d_alts s p_alts rhs
                return (NoDiscr, rhs_code)

        codeAlt alt@GenStgAlt{alt_con=_, alt_bndrs=bndrs, alt_rhs=rhs}
           -- primitive or nullary constructor alt: no need to UNPACK
           | null real_bndrs = do
                rhs_code <- schemeE d_alts s p_alts rhs
                return (my_discr alt, rhs_code)
           | isUnboxedTupleType bndr_ty || isUnboxedSumType bndr_ty =
             let bndr_ty = idPrimRepU . fromNonVoid
                 tuple_start = d_bndr
                 (call_info, args_offsets) =
                   layoutNativeCall profile
                                    NativeTupleReturn
                                    0
                                    bndr_ty
                                    (assertNonVoidIds bndrs)

                 stack_bot = d_alts

                 p' = Map.insertList
                        [ (arg, tuple_start -
                                wordsToBytes platform (nativeCallSize call_info) +
                                offset)
                        | (NonVoid arg, offset) <- args_offsets]
                        p_alts
             in do
               rhs_code <- schemeE stack_bot s p' rhs
               return (NoDiscr, rhs_code)
           -- algebraic alt with some binders
           | otherwise =
             let (tot_wds, _ptrs_wds, args_offsets) =
                     mkVirtHeapOffsets profile NoHeader
                         (addIdReps (assertNonVoidIds real_bndrs))
                 size = WordOff tot_wds

                 stack_bot = d_alts + wordsToBytes platform size

                 -- convert offsets from Sp into offsets into the virtual stack
                 p' = Map.insertList
                        [ (arg, stack_bot - ByteOff offset)
                        | (NonVoid arg, offset) <- args_offsets ]
                        p_alts

             in do
             massert isAlgCase
             rhs_code <- schemeE stack_bot s p' rhs
             return (my_discr alt,
                     unitOL (UNPACK size) `appOL` rhs_code)
           where
             real_bndrs = filterOut isTyVar bndrs

        my_discr alt = case alt_con alt of
            DEFAULT    -> NoDiscr {-shouldn't really happen-}
            DataAlt dc
              | isUnboxedTupleDataCon dc || isUnboxedSumDataCon dc
              -> NoDiscr
              | otherwise
              -> DiscrP (fromIntegral (dataConTag dc - fIRST_TAG))
            LitAlt l -> case l of
              LitNumber LitNumInt i    -> DiscrI (fromInteger i)
              LitNumber LitNumInt8 i   -> DiscrI8 (fromInteger i)
              LitNumber LitNumInt16 i  -> DiscrI16 (fromInteger i)
              LitNumber LitNumInt32 i  -> DiscrI32 (fromInteger i)
              LitNumber LitNumInt64 i  -> DiscrI64 (fromInteger i)
              LitNumber LitNumWord w   -> DiscrW (fromInteger w)
              LitNumber LitNumWord8 w  -> DiscrW8 (fromInteger w)
              LitNumber LitNumWord16 w -> DiscrW16 (fromInteger w)
              LitNumber LitNumWord32 w -> DiscrW32 (fromInteger w)
              LitNumber LitNumWord64 w -> DiscrW64 (fromInteger w)
              LitNumber LitNumBigNat _ -> unsupported
              LitFloat r               -> DiscrF (fromRational r)
              LitDouble r              -> DiscrD (fromRational r)
              LitChar i                -> DiscrI (ord i)
              LitString {}             -> unsupported
              LitRubbish {}            -> unsupported
              LitNullAddr {}           -> unsupported
              LitLabel {}              -> unsupported
              where
                  unsupported = pprPanic "schemeE(StgCase).my_discr:" (ppr l)

        maybe_ncons
           | not isAlgCase = Nothing
           | otherwise
           = case [dc | DataAlt dc <- alt_con <$> alts] of
                []     -> Nothing
                (dc:_) -> Just (tyConFamilySize (dataConTyCon dc))

        -- the bitmap is relative to stack depth d, i.e. before the
        -- BCO, info table and return value are pushed on.
        -- This bit of code is v. similar to buildLivenessMask in CgBindery,
        -- except that here we build the bitmap from the known bindings of
        -- things that are pointers, whereas in CgBindery the code builds the
        -- bitmap from the free slots and unboxed bindings.
        -- (ToDo: merge?)
        --
        -- NOTE [7/12/2006] bug #1013, testcase ghci/should_run/ghci002.
        -- The bitmap must cover the portion of the stack up to the sequel only.
        -- Previously we were building a bitmap for the whole depth (d), but we
        -- really want a bitmap up to depth (d-s).  This affects compilation of
        -- case-of-case expressions, which is the only time we can be compiling a
        -- case expression with s /= 0.

        -- unboxed tuples get two more words, the second is a pointer (tuple_bco)
        (extra_pointers, extra_slots)
           | ubx_tuple_frame && profiling = ([1], 3) -- call_info, tuple_BCO, CCCS
           | ubx_tuple_frame              = ([1], 2) -- call_info, tuple_BCO
           | otherwise                    = ([], 0)

        bitmap_size :: WordOff
        bitmap_size = fromIntegral extra_slots +
                      bytesToWords platform (d - s)

        bitmap_size' :: Int
        bitmap_size' = fromIntegral bitmap_size


        pointers =
          extra_pointers ++
          filter (< bitmap_size') (map (+extra_slots) rel_slots)
          where
          -- NB: unboxed tuple cases bind the scrut binder to the same offset
          -- as one of the alt binders, so we have to remove any duplicates here:
          -- 'toAscList' takes care of sorting the result, which was previously done after the application of 'filter'.
          rel_slots = IntSet.toAscList $ IntSet.fromList $ Map.elems $ Map.mapMaybeWithKey spread p
          spread id offset | isUnboxedTupleType (idType id) ||
                             isUnboxedSumType (idType id) = Nothing
                           | isFollowableArg (idArgRep platform id) = Just (fromIntegral rel_offset)
                           | otherwise                      = Nothing
                where rel_offset = bytesToWords platform (d - offset)

        bitmap = intsToReverseBitmap platform bitmap_size' pointers

     alt_stuff <- mapM codeAlt alts
     alt_final0 <- mkMultiBranch maybe_ncons alt_stuff

     let alt_final1
           | ubx_tuple_frame    = SLIDE 0 2 `consOL` alt_final0
           | otherwise          = alt_final0
         alt_final
           | gopt Opt_InsertBreakpoints (hsc_dflags hsc_env)
                                -- See Note [Debugger: BRK_ALTS]
                                = BRK_ALTS False `consOL` alt_final1
           | otherwise          = alt_final1

     add_bco_name <- shouldAddBcoName
     let
         alt_bco_name = getName bndr
         alt_bco = mkProtoBCO platform add_bco_name alt_bco_name alt_final (Left alts)
                       0{-no arity-} bitmap_size bitmap True{-is alts-}
     scrut_code <- schemeE (d + ret_frame_size_b + save_ccs_size_b)
                           (d + ret_frame_size_b + save_ccs_size_b)
                           p scrut
     if ubx_tuple_frame
       then do let tuple_bco = tupleBCO platform call_info args_offsets
               return (PUSH_ALTS_TUPLE alt_bco call_info tuple_bco
                       `consOL` scrut_code)
       else let scrut_rep = case non_void_arg_reps of
                  []    -> V
                  [rep] -> rep
                  _     -> panic "schemeE(StgCase).push_alts"
            in return (PUSH_ALTS alt_bco scrut_rep `consOL` scrut_code)

{-
Note [Debugger: BRK_ALTS]
~~~~~~~~~~~~~~~~~~~~~~~~~
As described in Note [Debugger: Step-out] in rts/Interpreter.c, to implement
the stepping-out debugger feature we traverse the stack at runtime, identify
the first continuation BCO, and explicitly enable that BCO's breakpoint thus
ensuring that we stop exactly when we return to the continuation.

However, case continuation BCOs (produced by PUSH_ALTS and which merely compute
which case alternative BCO to enter next) contain no user-facing breakpoint
ticks (BRK_FUN). While we could in principle add breakpoints in case continuation
BCOs, there are a few reasons why this is not an attractive option:

  1) It's not useful to a user stepping through the program to always have a
  breakpoint after the scrutinee is evaluated but before the case alternative
  is selected. The source span associated with such a breakpoint would also be
  slightly awkward to choose.

  2) It's not easy to add a breakpoint tick before the case alternatives because in
  essentially all internal representations they are given as a list of Alts
  rather than an expression.

To provide the debugger a way to break in a case continuation
despite the BCOs' lack of BRK_FUNs, we introduce an alternative
type of breakpoint, represented by the BRK_ALTS instruction,
at the start of every case continuation BCO. For instance,

    case x of
      0# -> ...
      _  -> ...

will produce a continuation of the form (N.B. the below bytecode
is simplified):

    PUSH_ALTS P
      BRK_ALTS 0
      TESTEQ_I 0 lblA
      PUSH_BCO
        BRK_FUN 0
        -- body of 0# alternative
      ENTER

      lblA:
      PUSH_BCO
        BRK_FUN 1
        -- body of wildcard alternative
      ENTER

When enabled (by its single boolean operand), the BRK_ALTS instruction causes
the program to break at the next encountered breakpoint (implemented
by setting the TSO's TSO_STOP_NEXT_BREAKPOINT flag). Since the case
continuation BCO will ultimately jump to one of the alternatives (each of
which having its own BRK_FUN) we are guaranteed to stop in the taken alternative.

It's important that BRK_ALTS (just like BRK_FUN) is the first instruction of
the BCO, since that's where the debugger will look to enable it at runtime.

KNOWN ISSUES:
-------------
This implementation of BRK_ALTS that modifies the first argument of the
bytecode to enable it does not allow multi-threaded debugging because the BCO
object is shared across threads and enabling the breakpoint in one will enable
it in all other threads too. This will have to change to support multi-threads
debugging.

The progress towards multi-threaded debugging is tracked by #26064
-}

-- -----------------------------------------------------------------------------
-- Deal with tuples

-- The native calling convention uses registers for tuples, but in the
-- bytecode interpreter, all values live on the stack.

{- Note [GHCi and native call registers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The GHCi bytecode interpreter does not have access to the STG registers
that the native calling convention uses for passing arguments. It uses
helper stack frames to move values between the stack and registers.

If only a single register needs to be moved, GHCi uses a specific stack
frame. For example stg_ctoi_R1p saves a heap pointer value from STG register
R1 and stg_ctoi_D1 saves a double precision floating point value from D1.
In the other direction, helpers stg_ret_p and stg_ret_d move a value from
the stack to the R1 and D1 registers, respectively.

When GHCi needs to move more than one register it cannot use a specific
helper frame. It would simply be impossible to create a helper for all
possible combinations of register values. Instead, there are generic helper
stack frames that use a call_info word that describes the active registers
and the number of stack words used by the arguments of a call.

These helper stack frames are currently:

    - stg_ret_t:    return a tuple to the continuation at the top of
                        the stack
    - stg_ctoi_t:   convert a tuple return value to be used in
                        bytecode
    - stg_primcall: call a function


The call_info word contains a bitmap of the active registers
for the call and and a stack offset. The layout is as follows:

  - bit 0-23:  Bitmap of active registers for the call, the
               order corresponds to the list returned by
               allArgRegsCover.
               For example if bit 0 (the least significant bit) is set, the
               first register in the allArgRegsCover
               list is active. Bit 1 for the
               second register in the list and so on.

  - bit 24-31: Unsigned byte indicating the stack offset
               of the continuation in words. For tuple returns
               this is the number of words returned on the
               stack. For primcalls this field is unused, since
               we don't jump to a continuation.

The upper 32 bits on 64 bit platforms are currently unused.

If a register is smaller than a word on the stack (for example a
single precision float on a 64 bit system), then the stack slot
is padded to a whole word.

  Example:

    If a tuple is returned in three registers and an additional two
    words on the stack, then three bits in the register bitmap
    (bits 0-23) would be set. And bit 24-31 would be
    00000010 (two in binary).

    The values on the stack before a call to POP_ARG_REGS would
    be as follows:

      ...
      continuation
      stack_arg_1
      stack_arg_2
      register_arg_3
      register_arg_2
      register_arg_1 <- Sp

    A call to POP_ARG_REGS(call_info) would move register_arg_1
    to the register corresponding to the lowest set bit in the
    call_info word. register_arg_2 would be moved to the register
    corresponding to the second lowest set bit, and so on.

    After POP_ARG_REGS(call_info), the stack pointer Sp points
    to the topmost stack argument, so the stack looks as follows:

      ...
      continuation
      stack_arg_1
      stack_arg_2 <- Sp

    At this point all the arguments are in place and we are ready
    to jump to the continuation, the location (offset from Sp) of
    which is found by inspecting the value of bits 24-31. In this
    case the offset is two words.

On x86_64, the double precision (Dn) and single precision
floating (Fn) point registers overlap, e.g. D1 uses the same
physical register as F1. On this platform, the list returned
by allArgRegsCover contains only entries for the double
precision registers. If an argument is passed in register
Fn, the bit corresponding to Dn should be set.

Note: if anything changes in how registers for native calls overlap,
         make sure to also update GHC.StgToByteCode.layoutNativeCall
-}

layoutNativeCall :: Profile
                 -> NativeCallType
                 -> ByteOff
                 -> (a -> PrimRep)
                 -> [a]
                 -> ( NativeCallInfo      -- See Note [GHCi TupleInfo]
                    , [(a, ByteOff)] -- argument, offset on stack
                    )
layoutNativeCall profile call_type start_off arg_rep reps =
  let platform = profilePlatform profile
      arg_ty = primRepCmmType platform . arg_rep
      (orig_stk_bytes, pos) = assignArgumentsPos profile
                                                 0
                                                 NativeReturn
                                                 arg_ty
                                                 reps

      -- keep the stack parameters in the same place
      orig_stk_params = [(x, fromIntegral off) | (x, StackParam off) <- pos]

      -- sort the register parameters by register and add them to the stack
      regs_order :: Map.Map GlobalReg Int
      regs_order = Map.fromList $ zip (allArgRegsCover platform SCALAR_ARG_REGS) [0..]

      reg_order :: GlobalReg -> (Int, GlobalReg)
      reg_order reg | Just n <- Map.lookup reg regs_order = (n, reg)
      -- if we don't have a position for a FloatReg then they must be passed
      -- in the equivalent DoubleReg
      reg_order (FloatReg n) = reg_order (DoubleReg n)
      -- one-tuples can be passed in other registers, but then we don't need
      -- to care about the order
      reg_order reg          = (0, reg)

      (regs, reg_params)
          = unzip $ sortBy (comparing fst)
                           [(reg_order reg, x) | (x, RegisterParam reg) <- pos]

      (new_stk_bytes, new_stk_params) = assignStack platform
                                                    orig_stk_bytes
                                                    arg_ty
                                                    reg_params

      regs_set = mkRegSet (map snd regs)

      get_byte_off (x, StackParam y) = (x, fromIntegral y)
      get_byte_off _                 =
          panic "GHC.StgToByteCode.layoutTuple get_byte_off"

  in ( NativeCallInfo
         { nativeCallType           = call_type
         , nativeCallSize           = bytesToWords platform (ByteOff new_stk_bytes)
         , nativeCallRegs           = regs_set
         , nativeCallStackSpillSize = bytesToWords platform
                                               (ByteOff orig_stk_bytes)
         }
     , sortBy (comparing snd) $
              map (\(x, o) -> (x, o + start_off))
                  (orig_stk_params ++ map get_byte_off new_stk_params)
     )

{- Note [Return convention for non-tuple values]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The RETURN and ENTER instructions are used to return values. RETURN directly
returns the value at the top of the stack while ENTER evaluates it first (so
RETURN is only used when the result is already known to be evaluated), but the
end result is the same: control returns to the enclosing stack frame with the
result at the top of the stack.

The PUSH_ALTS instruction pushes a two-word stack frame that receives a single
lifted value. Its payload is a BCO that is executed when control returns, with
the stack set up as if a RETURN instruction had just been executed: the returned
value is at the top of the stack, and beneath it is the two-word frame being
returned to. It is the continuation BCO’s job to pop its own frame off the
stack, so the simplest possible continuation consists of two instructions:

    SLIDE 1 2   -- pop the return frame off the stack, keeping the returned value
    RETURN P    -- return the returned value to our caller

RETURN and PUSH_ALTS are not really instructions but are in fact representation-
polymorphic *families* of instructions indexed by ArgRep. ENTER, however, is a
single real instruction, since it is only used to return lifted values, which
are always pointers.

The RETURN, ENTER, and PUSH_ALTS instructions are only used when the returned
value has nullary or unary representation. Returning/receiving an unboxed
tuple (or, indirectly, an unboxed sum, since unboxed sums have been desugared to
unboxed tuples by Unarise) containing two or more results uses the special
RETURN_TUPLE/PUSH_ALTS_TUPLE instructions, which use a different return
convention. See Note [unboxed tuple bytecodes and tuple_BCO] for details.

Note [unboxed tuple bytecodes and tuple_BCO]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  We have the bytecode instructions RETURN_TUPLE and PUSH_ALTS_TUPLE to
  return and receive arbitrary unboxed tuples, respectively. These
  instructions use the helper data tuple_BCO and call_info.

  The helper data is used to convert tuples between GHCs native calling
  convention (object code), which uses stack and registers, and the bytecode
  calling convention, which only uses the stack. See Note [GHCi TupleInfo]
  for more details.


  Returning a tuple
  =================

  Bytecode that returns a tuple first pushes all the tuple fields followed
  by the appropriate call_info and tuple_BCO onto the stack. It then
  executes the RETURN_TUPLE instruction, which causes the interpreter
  to push stg_ret_t_info to the top of the stack. The stack (growing down)
  then looks as follows:

      ...
      next_frame
      tuple_field_1
      tuple_field_2
      ...
      tuple_field_n
      call_info
      tuple_BCO
      stg_ret_t_info <- Sp

  If next_frame is bytecode, the interpreter will start executing it. If
  it's object code, the interpreter jumps back to the scheduler, which in
  turn jumps to stg_ret_t. stg_ret_t converts the tuple to the native
  calling convention using the description in call_info, and then jumps
  to next_frame.


  Receiving a tuple
  =================

  Bytecode that receives a tuple uses the PUSH_ALTS_TUPLE instruction to
  push a continuation, followed by jumping to the code that produces the
  tuple. The PUSH_ALTS_TUPLE instuction contains three pieces of data:

     * cont_BCO: the continuation that receives the tuple
     * call_info: see below
     * tuple_BCO: see below

  The interpreter pushes these onto the stack when the PUSH_ALTS_TUPLE
  instruction is executed, followed by stg_ctoi_tN_info, with N depending
  on the number of stack words used by the tuple in the GHC native calling
  convention. N is derived from call_info.

  For example if we expect a tuple with three words on the stack, the stack
  looks as follows after PUSH_ALTS_TUPLE:

      ...
      next_frame
      cont_free_var_1
      cont_free_var_2
      ...
      cont_free_var_n
      call_info
      tuple_BCO
      cont_BCO
      stg_ctoi_t3_info <- Sp

  If the tuple is returned by object code, stg_ctoi_t3 will deal with
  adjusting the stack pointer and converting the tuple to the bytecode
  calling convention. See Note [GHCi unboxed tuples stack spills] for more
  details.


  The tuple_BCO
  =============

  The tuple_BCO is a helper bytecode object. Its main purpose is describing
  the contents of the stack frame containing the tuple for the storage
  manager. It contains only instructions to immediately return the tuple
  that is already on the stack.


  The call_info word
  ===================

  The call_info word describes the stack and STG register (e.g. R1..R6,
  D1..D6) usage for the tuple. call_info contains enough information to
  convert the tuple between the stack-only bytecode and stack+registers
  GHC native calling conventions.

  See Note [GHCi and native call registers] for more details of how the
  data is packed in a single word.

 -}

tupleBCO :: Platform -> NativeCallInfo -> [(PrimRep, ByteOff)] -> ProtoBCO Name
tupleBCO platform args_info args =
  mkProtoBCO platform Nothing invented_name body_code (Left [])
             0{-no arity-} bitmap_size bitmap False{-not alts-}
  where
    {-
      The tuple BCO is never referred to by name, so we can get away
      with using a fake name here. We will need to change this if we want
      to save some memory by sharing the BCO between places that have
      the same tuple shape
    -}
    invented_name  = mkSystemVarName (mkPseudoUniqueE 0) (fsLit "tuple")

    -- the first word in the frame is the call_info word,
    -- which is not a pointer
    nptrs_prefix = 1
    (bitmap_size, bitmap) = mkStackBitmap platform nptrs_prefix args_info args

    body_code = mkSlideW 0 1          -- pop frame header
                `snocOL` RETURN_TUPLE -- and add it again

primCallBCO :: Platform -> NativeCallInfo -> [(PrimRep, ByteOff)] -> ProtoBCO Name
primCallBCO platform args_info args =
  mkProtoBCO platform Nothing invented_name body_code (Left [])
             0{-no arity-} bitmap_size bitmap False{-not alts-}
  where
    {-
      The primcall BCO is never referred to by name, so we can get away
      with using a fake name here. We will need to change this if we want
      to save some memory by sharing the BCO between places that have
      the same tuple shape
    -}
    invented_name  = mkSystemVarName (mkPseudoUniqueE 0) (fsLit "primcall")

    -- The first two words in the frame (after the BCO) are the call_info word
    -- and the pointer to the Cmm function being called. Neither of these is a
    -- pointer that should be followed by the garbage collector.
    nptrs_prefix = 2
    (bitmap_size, bitmap) = mkStackBitmap platform nptrs_prefix args_info args

    -- if the primcall BCO is ever run it's a bug, since the BCO should only
    -- be pushed immediately before running the PRIMCALL bytecode instruction,
    -- which immediately leaves the interpreter to jump to the stg_primcall_info
    -- Cmm function
    body_code =  unitOL CASEFAIL

-- | Builds a bitmap for a stack layout with a nonpointer prefix followed by
-- some number of arguments.
mkStackBitmap
  :: Platform
  -> WordOff
  -- ^ The number of nonpointer words that prefix the arguments.
  -> NativeCallInfo
  -> [(PrimRep, ByteOff)]
  -- ^ The stack layout of the arguments, where each offset is relative to the
  -- /bottom/ of the stack space they occupy. Their offsets must be word-aligned,
  -- and the list must be sorted in order of ascending offset (i.e. bottom to top).
  -> (WordOff, [StgWord])
mkStackBitmap platform nptrs_prefix args_info args
  = (bitmap_size, bitmap)
  where
    bitmap_size = nptrs_prefix + arg_bottom
    bitmap = intsToReverseBitmap platform (fromIntegral bitmap_size) ptr_offsets

    arg_bottom = nativeCallSize args_info
    ptr_offsets = reverse $ map (fromIntegral . convert_arg_offset)
                $ mapMaybe get_ptr_offset args

    get_ptr_offset :: (PrimRep, ByteOff) -> Maybe ByteOff
    get_ptr_offset (rep, byte_offset)
      | isFollowableArg (toArgRep platform rep) = Just byte_offset
      | otherwise                               = Nothing

    convert_arg_offset :: ByteOff -> WordOff
    convert_arg_offset arg_offset =
      -- The argument offsets are relative to `arg_bottom`, but
      -- `intsToReverseBitmap` expects offsets from the top, so we need to flip
      -- them around.
      nptrs_prefix + (arg_bottom - bytesToWords platform arg_offset)

-- -----------------------------------------------------------------------------
-- Deal with a primitive call to native code.

generatePrimCall
    :: StackDepth
    -> Sequel
    -> BCEnv
    -> CLabelString          -- where to call
    -> Maybe Unit
    -> Type
    -> [StgArg]              -- args (atoms)
    -> BcM BCInstrList
generatePrimCall d s p target _mb_unit _result_ty args
 = do
     profile <- getProfile
     let
         platform = profilePlatform profile

         non_void VoidRep = False
         non_void _       = True

         nv_args :: [StgArg]
         nv_args = filter (non_void . stgArgRep1) args

         (args_info, args_offsets) =
              layoutNativeCall profile
                               NativePrimCall
                               0
                               stgArgRepU
                               nv_args

         prim_args_offsets = mapFst stgArgRepU args_offsets
         shifted_args_offsets = mapSnd (+ d) args_offsets

         push_target = PUSH_UBX (LitLabel target IsFunction) 1
         push_info = PUSH_UBX (mkNativeCallInfoLit platform args_info) 1
         {-
            compute size to move payload (without stg_primcall_info header)

            size of arguments plus three words for:
                - function pointer to the target
                - call_info word
                - BCO to describe the stack frame
          -}
         szb = wordsToBytes platform (nativeCallSize args_info + 3)
         go _   pushes [] = return (reverse pushes)
         go !dd pushes ((a, off):cs) = do (push, szb) <- pushAtom dd p a
                                          massert (off == dd + szb)
                                          go (dd + szb) (push:pushes) cs
     push_args <- go d [] shifted_args_offsets
     let args_bco = primCallBCO platform args_info prim_args_offsets
     return $ mconcat push_args `appOL`
              (push_target `consOL`
               push_info `consOL`
               PUSH_BCO args_bco `consOL`
               (mkSlideB platform szb (d - s) `appOL` unitOL PRIMCALL))

-- -----------------------------------------------------------------------------
-- Deal with a CCall.

-- Taggedly push the args onto the stack R->L,
-- deferencing ForeignObj#s and adjusting addrs to point to
-- payloads in Ptr/Byte arrays.  Then, generate the marshalling
-- (machine) code for the ccall, and create bytecodes to call that and
-- then return in the right way.

generateCCall
    :: StackDepth
    -> Sequel
    -> BCEnv
    -> CCallSpec               -- where to call
    -> Type
    -> [StgArg]              -- args (atoms)
    -> BcM BCInstrList
generateCCall d0 s p (CCallSpec target PrimCallConv _) result_ty args
 | (StaticTarget _ label mb_unit _) <- target
 = generatePrimCall d0 s p label mb_unit result_ty args
 | otherwise
 = panic "GHC.StgToByteCode.generateCCall: primcall convention only supports static targets"
generateCCall d0 s p (CCallSpec target _ safety) result_ty args
 = do
     profile <- getProfile

     let
         args_r_to_l = reverse args
         platform = profilePlatform profile
         -- useful constants
         addr_size_b :: ByteOff
         addr_size_b = wordSize platform

         arrayish_rep_hdr_size :: TyCon -> Maybe Int
         arrayish_rep_hdr_size t
           | t == arrayPrimTyCon || t == mutableArrayPrimTyCon
              = Just (arrPtrsHdrSize profile)
           | t == smallArrayPrimTyCon || t == smallMutableArrayPrimTyCon
              = Just (smallArrPtrsHdrSize profile)
           | t == byteArrayPrimTyCon || t == mutableByteArrayPrimTyCon
              = Just (arrWordsHdrSize profile)
           | otherwise
              = Nothing

         -- Get the args on the stack, with tags and suitably
         -- dereferenced for the CCall.  For each arg, return the
         -- depth to the first word of the bits for that arg, and the
         -- ArgRep of what was actually pushed.

         pargs
             :: ByteOff -> [StgArg] -> BcM [(BCInstrList, PrimOrVoidRep)]
         pargs _ [] = return []
         pargs d (aa@(StgVarArg a):az)
            | Just t      <- tyConAppTyCon_maybe (idType a)
            , Just hdr_sz <- arrayish_rep_hdr_size t
            -- Do magic for Ptr/Byte arrays.  Push a ptr to the array on
            -- the stack but then advance it over the headers, so as to
            -- point to the payload.
            = do rest <- pargs (d + addr_size_b) az
                 (push_fo, _) <- pushAtom d p aa
                 -- The ptr points at the header.  Advance it over the
                 -- header and then pretend this is an Addr#.
                 let code = push_fo `snocOL` SWIZZLE 0 (fromIntegral hdr_sz)
                 return ((code, NVRep AddrRep) : rest)
         pargs d (aa:az) =  do (code_a, sz_a) <- pushAtom d p aa
                               rest <- pargs (d + sz_a) az
                               return ((code_a, stgArgRep1 aa) : rest)

     code_n_reps <- pargs d0 args_r_to_l
     let
         (pushs_arg, a_reps_pushed_r_to_l) = unzip code_n_reps
         a_reps_sizeW = sum (map (repSizeWords platform) a_reps_pushed_r_to_l)

         push_args    = concatOL pushs_arg
         !d_after_args = d0 + wordsToBytes platform a_reps_sizeW
         a_reps_pushed_RAW
            | VoidRep:xs <- a_reps_pushed_r_to_l
            = reverse xs
            | otherwise
            = panic "GHC.StgToByteCode.generateCCall: missing or invalid World token?"

         -- Now: a_reps_pushed_RAW are the reps which are actually on the stack.
         -- push_args is the code to do that.
         -- d_after_args is the stack depth once the args are on.

         -- Get the result rep.
         r_rep = maybe_getCCallReturnRep result_ty
         {-
         Because the Haskell stack grows down, the a_reps refer to
         lowest to highest addresses in that order.  The args for the call
         are on the stack.  Now push an unboxed Addr# indicating
         the C function to call.  Then push a dummy placeholder for the
         result.  Finally, emit a CCALL insn with an offset pointing to the
         Addr# just pushed, and a literal field holding the mallocville
         address of the piece of marshalling code we generate.
         So, just prior to the CCALL insn, the stack looks like this
         (growing down, as usual):

            <arg_n>
            ...
            <arg_1>
            Addr# address_of_C_fn
            <placeholder-for-result#> (must be an unboxed type)

         The interpreter then calls the marshal code mentioned
         in the CCALL insn, passing it (& <placeholder-for-result#>),
         that is, the addr of the topmost word in the stack.
         When this returns, the placeholder will have been
         filled in.  The placeholder is slid down to the sequel
         depth, and we RETURN.

         This arrangement makes it simple to do f-i-dynamic since the Addr#
         value is the first arg anyway.

         The marshalling code is generated specifically for this
         call site, and so knows exactly the (Haskell) stack
         offsets of the args, fn address and placeholder.  It
         copies the args to the C stack, calls the stacked addr,
         and parks the result back in the placeholder.  The interpreter
         calls it as a normal C call, assuming it has a signature
            void marshal_code ( StgWord* ptr_to_top_of_stack )
         -}
         -- resolve static address
         maybe_static_target :: Maybe Literal
         maybe_static_target =
             case target of
                 DynamicTarget -> Nothing
                 StaticTarget _ _ _ False ->
                   panic "generateCCall: unexpected FFI value import"
                 StaticTarget _ target _ True ->
                   Just (LitLabel target IsFunction)

     let
         is_static = isJust maybe_static_target

         -- Get the arg reps, zapping the leading Addr# in the dynamic case
         a_reps --  | trace (showSDoc (ppr a_reps_pushed_RAW)) False = error "???"
                | is_static = a_reps_pushed_RAW
                | _:xs <- a_reps_pushed_RAW = xs
                | otherwise = panic "GHC.StgToByteCode.generateCCall: dyn with no args"

         -- push the Addr#
         (push_Addr, d_after_Addr)
            | Just machlabel <- maybe_static_target
            = (toOL [PUSH_UBX machlabel 1], d_after_args + addr_size_b)
            | otherwise -- is already on the stack
            = (nilOL, d_after_args)

         -- Push the return placeholder.  For a call returning nothing,
         -- this is a V (tag).
         r_sizeW   = repSizeWords platform r_rep
         d_after_r = d_after_Addr + wordsToBytes platform r_sizeW
         push_r = case r_rep of
                    VoidRep -> nilOL
                    NVRep r -> unitOL (PUSH_UBX (mkDummyLiteral platform r) r_sizeW)

         -- generate the marshalling code we're going to call

         -- Offset of the next stack frame down the stack.  The CCALL
         -- instruction needs to describe the chunk of stack containing
         -- the ccall args to the GC, so it needs to know how large it
         -- is.  See comment in Interpreter.c with the CCALL instruction.
         stk_offset   = bytesToWords platform (d_after_r - s)

     -- the only difference in libffi mode is that we prepare a cif
     -- describing the call type by calling libffi, and we attach the
     -- address of this to the CCALL instruction.


     let ffires = primRepToFFIType platform r_rep
         ffiargs = map (primRepToFFIType platform) a_reps

     let
         -- do the call
         do_call      = unitOL (CCALL stk_offset (FFIInfo ffiargs ffires) flags)
           where flags = case safety of
                           PlaySafe          -> 0x0
                           PlayInterruptible -> 0x1
                           PlayRisky         -> 0x2

         -- slide and return
         d_after_r_min_s = bytesToWords platform (d_after_r - s)
         wrapup       = mkSlideW r_sizeW (d_after_r_min_s - r_sizeW)
                        `snocOL` RETURN (toArgRepOrV platform r_rep)
         --trace (show (arg1_offW, args_offW  ,  (map argRepSizeW a_reps) )) $
     return (
         push_args `appOL`
         push_Addr `appOL` push_r `appOL` do_call `appOL` wrapup
         )

primRepToFFIType :: Platform -> PrimOrVoidRep -> FFIType
primRepToFFIType _ VoidRep = FFIVoid
primRepToFFIType platform (NVRep r)
  = case r of
     IntRep      -> signed_word
     WordRep     -> unsigned_word
     Int8Rep     -> FFISInt8
     Word8Rep    -> FFIUInt8
     Int16Rep    -> FFISInt16
     Word16Rep   -> FFIUInt16
     Int32Rep    -> FFISInt32
     Word32Rep   -> FFIUInt32
     Int64Rep    -> FFISInt64
     Word64Rep   -> FFIUInt64
     AddrRep     -> FFIPointer
     FloatRep    -> FFIFloat
     DoubleRep   -> FFIDouble
     BoxedRep _  -> FFIPointer
     VecRep{}    -> pprPanic "primRepToFFIType" (ppr r)
  where
    (signed_word, unsigned_word) = case platformWordSize platform of
       PW4 -> (FFISInt32, FFIUInt32)
       PW8 -> (FFISInt64, FFIUInt64)

-- Make a dummy literal, to be used as a placeholder for FFI return
-- values on the stack.
mkDummyLiteral :: Platform -> PrimRep -> Literal
mkDummyLiteral platform pr
   = case pr of
        IntRep      -> mkLitInt  platform 0
        WordRep     -> mkLitWord platform 0
        Int8Rep     -> mkLitInt8 0
        Word8Rep    -> mkLitWord8 0
        Int16Rep    -> mkLitInt16 0
        Word16Rep   -> mkLitWord16 0
        Int32Rep    -> mkLitInt32 0
        Word32Rep   -> mkLitWord32 0
        Int64Rep    -> mkLitInt64 0
        Word64Rep   -> mkLitWord64 0
        AddrRep     -> LitNullAddr
        DoubleRep   -> LitDouble 0
        FloatRep    -> LitFloat 0
        BoxedRep _  -> LitNullAddr
        VecRep{}    -> pprPanic "mkDummyLiteral" (ppr pr)


-- Convert (eg)
--     GHC.Prim.Char# -> GHC.Prim.State# GHC.Prim.RealWorld
--                   -> (# GHC.Prim.State# GHC.Prim.RealWorld, GHC.Prim.Int# #)
--
-- to  NVRep IntRep
-- and check that an unboxed pair is returned wherein the first arg is V'd.
--
-- Alternatively, for call-targets returning nothing, convert
--
--     GHC.Prim.Char# -> GHC.Prim.State# GHC.Prim.RealWorld
--                   -> (# GHC.Prim.State# GHC.Prim.RealWorld #)
--
-- to  VoidRep

maybe_getCCallReturnRep :: Type -> PrimOrVoidRep
maybe_getCCallReturnRep fn_ty
   = let
       (_a_tys, r_ty) = splitFunTys (dropForAlls fn_ty)
     in
       case typePrimRep r_ty of
         [] -> VoidRep
         [rep] -> NVRep rep

                 -- if it was, it would be impossible to create a
                 -- valid return value placeholder on the stack
         _ -> pprPanic "maybe_getCCallReturn: can't handle:"
                         (pprType fn_ty)

maybe_is_tagToEnum_call :: CgStgExpr -> Maybe (StgArg, [Name])
-- Detect and extract relevant info for the tagToEnum kludge.
maybe_is_tagToEnum_call (StgOpApp (StgPrimOp TagToEnumOp) args t)
  | [v] <- args
  = Just (v, extract_constr_Names t)
  | otherwise
  = pprPanic "StgToByteCode: tagToEnum#"
     $ text "Expected exactly one arg, but actual args are:" <+> ppr args
  where
    extract_constr_Names ty
           | rep_ty <- unwrapType ty
           , Just tyc <- tyConAppTyCon_maybe rep_ty
           , isDataTyCon tyc
           = map (getName . dataConWorkId) (tyConDataCons tyc)
           -- NOTE: use the worker name, not the source name of
           -- the DataCon.  See "GHC.Core.DataCon" for details.
           | otherwise
           = pprPanic "maybe_is_tagToEnum_call.extract_constr_Ids" (ppr ty)
maybe_is_tagToEnum_call _ = Nothing

{- -----------------------------------------------------------------------------
Note [Implementing tagToEnum#]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(implement_tagToId arg names) compiles code which takes an argument
'arg', (call it i), and enters the i'th closure in the supplied list
as a consequence.  The [Name] is a list of the constructors of this
(enumeration) type.

The code we generate is this:
                push arg

                TESTEQ_I 0 L1
                  PUSH_G <lbl for first data con>
                  JMP L_Exit

        L1:     TESTEQ_I 1 L2
                  PUSH_G <lbl for second data con>
                  JMP L_Exit
        ...etc...
        Ln:     TESTEQ_I n L_fail
                  PUSH_G <lbl for last data con>
                  JMP L_Exit

        L_fail: CASEFAIL

        L_exit: SLIDE 1 n
                ENTER
-}


implement_tagToId
    :: StackDepth
    -> Sequel
    -> BCEnv
    -> StgArg
    -> [Name]
    -> BcM BCInstrList
-- See Note [Implementing tagToEnum#]
implement_tagToId d s p arg names
  = assert (notNull names) $
    do (push_arg, arg_bytes) <- pushAtom d p arg
       labels <- getLabelsBc (strictGenericLength names)
       label_fail <- getLabelBc
       label_exit <- getLabelBc
       dflags <- getDynFlags
       let infos = zip4 labels (tail labels ++ [label_fail])
                               [0 ..] names
           platform = targetPlatform dflags
           steps = map (mkStep label_exit) infos
           slide_ws = bytesToWords platform (d - s + arg_bytes)

       return (push_arg
               `appOL` concatOL steps
               `appOL` toOL [ LABEL label_fail, CASEFAIL,
                              LABEL label_exit ]
               `appOL` mkSlideW 1 slide_ws
               `appOL` unitOL ENTER)
  where
        mkStep l_exit (my_label, next_label, n, name_for_n)
           = toOL [LABEL my_label,
                   TESTEQ_I n next_label,
                   PUSH_G name_for_n,
                   JMP l_exit]


-- -----------------------------------------------------------------------------
-- pushAtom

-- Push an atom onto the stack, returning suitable code & number of
-- stack words used.
--
-- The env p must map each variable to the highest- numbered stack
-- slot for it.  For example, if the stack has depth 4 and we
-- tagged-ly push (v :: Int#) on it, the value will be in stack[4],
-- the tag in stack[5], the stack will have depth 6, and p must map v
-- to 5 and not to 4.  Stack locations are numbered from zero, so a
-- depth 6 stack has valid words 0 .. 5.

pushAtom :: StackDepth -> BCEnv -> StgArg -> BcM (BCInstrList, ByteOff)
-- See Note [Empty case alternatives] in GHC.Core
-- and Note [Bottoming expressions] in GHC.Core.Utils:
-- The scrutinee of an empty case evaluates to bottom
pushAtom d p (StgVarArg var)
   | [] <- typePrimRep (idType var)
   = return (nilOL, 0)

   | isFCallId var
   = pprPanic "pushAtom: shouldn't get an FCallId here" (ppr var)

   | Just primop <- isPrimOpId_maybe var
   = do
       platform <- targetPlatform <$> getDynFlags
       return (unitOL (PUSH_PRIMOP primop), wordSize platform)

   | Just d_v <- lookupBCEnv_maybe var p  -- var is a local variable
   = do platform <- targetPlatform <$> getDynFlags

        let !szb = idSizeCon platform var
            with_instr :: (ByteOff -> BCInstr) -> BcM (OrdList BCInstr, ByteOff)
            with_instr instr = do
                let !off_b = d - d_v
                return (unitOL (instr off_b), wordSize platform)

        case szb of
            1 -> with_instr PUSH8_W
            2 -> with_instr PUSH16_W
            4 -> with_instr PUSH32_W
            _ -> do
                let !szw = bytesToWords platform szb
                    !off_w = bytesToWords platform (d - d_v) + szw - 1
                return (toOL (genericReplicate szw (PUSH_L off_w)),
                              wordsToBytes platform szw)
        -- d - d_v           offset from TOS to the first slot of the object
        --
        -- d - d_v + sz - 1  offset from the TOS of the last slot of the object
        --
        -- Having found the last slot, we proceed to copy the right number of
        -- slots on to the top of the stack.

   | otherwise  -- var must be a global variable
   = do platform <- targetPlatform <$> getDynFlags
        let !szb = idSizeCon platform var
        massert (szb == wordSize platform)

        -- PUSH_G doesn't tag constructors. So we use PACK here
        -- if we are dealing with nullary constructor.
        case isDataConWorkId_maybe var of
          Just con -> do
            massert (isNullaryRepDataCon con)
            return (unitOL (PACK con 0), szb)

          Nothing
            -- see Note [Generating code for top-level string literal bindings]
            | idType var `eqType` addrPrimTy ->
              return (unitOL (PUSH_ADDR (getName var)), szb)

            | otherwise -> do
              let varTy = idType var
              massertPpr (definitelyLiftedType varTy) $
                vcat [ text "pushAtom: unhandled unlifted type"
                     , text "var:" <+> ppr var <+> dcolon <+> ppr varTy <> dcolon <+> ppr (typeKind varTy)
                     ]
              return (unitOL (PUSH_G (getName var)), szb)

pushAtom _ _ (StgLitArg lit) = pushLiteral True lit

pushLiteral :: Bool -> Literal -> BcM (BCInstrList, ByteOff)
pushLiteral padded lit =
  do
     platform <- targetPlatform <$> getDynFlags
     let code :: PrimRep -> BcM (BCInstrList, ByteOff)
         code rep =
            -- TODO: It's a bit silly to use up to four instructions to put a single literal on the stack.
            --       Lot's of better ways to do this. Add a instruction to push it as full word.
            --       Store the literal as full word and push it as full word.
            --       Maybe more, but for now this will do.
            case platformByteOrder platform of
              LittleEndian -> return (padding_instr `snocOL` instr, size_bytes + padding_bytes)
              BigEndian -> return (instr `consOL` padding_instr, size_bytes + padding_bytes)
          where
            size_bytes = ByteOff $ primRepSizeB platform rep

            -- Here we handle the non-word-width cases specifically since we
            -- must emit different bytecode for them.

            round_to_words (ByteOff bytes) =
              ByteOff (roundUpToWords platform bytes)

            padding_bytes
                | padded    = round_to_words size_bytes - size_bytes
                | otherwise = 0

            (padding_instr, _) = pushPadding padding_bytes

            instr =
              case size_bytes of
                1  -> PUSH_UBX8 lit
                2  -> PUSH_UBX16 lit
                4  -> PUSH_UBX32 lit
                _  -> PUSH_UBX lit (bytesToWords platform size_bytes)

     case lit of
        LitLabel {}     -> code AddrRep
        LitFloat {}     -> code FloatRep
        LitDouble {}    -> code DoubleRep
        LitChar {}      -> code WordRep
        LitNullAddr     -> code AddrRep
        LitString {}    -> code AddrRep
        LitRubbish _ rep-> case runtimeRepPrimRep (text "pushLiteral") rep of
                             [pr] -> code pr
                             _    -> pprPanic "pushLiteral" (ppr lit)
        LitNumber nt _  -> case nt of
          LitNumInt     -> code IntRep
          LitNumWord    -> code WordRep
          LitNumInt8    -> code Int8Rep
          LitNumWord8   -> code Word8Rep
          LitNumInt16   -> code Int16Rep
          LitNumWord16  -> code Word16Rep
          LitNumInt32   -> code Int32Rep
          LitNumWord32  -> code Word32Rep
          LitNumInt64   -> code Int64Rep
          LitNumWord64  -> code Word64Rep
          -- No LitNumBigNat should be left by the time this is called. CorePrep
          -- should have converted them all to a real core representation.
          LitNumBigNat  -> panic "pushAtom: LitNumBigNat"

-- | Push an atom for constructor (i.e., PACK instruction) onto the stack.
-- This is slightly different to @pushAtom@ due to the fact that we allow
-- packing constructor fields. See also @mkConAppCode@ and @pushPadding@.
pushConstrAtom
    :: StackDepth -> BCEnv -> StgArg -> BcM (BCInstrList, ByteOff)
pushConstrAtom _ _ (StgLitArg lit) = pushLiteral False lit

pushConstrAtom d p va@(StgVarArg v)
    | Just d_v <- lookupBCEnv_maybe v p = do  -- v is a local variable
        platform <- targetPlatform <$> getDynFlags
        let !szb = idSizeCon platform v
            done instr = do
                let !off = d - d_v
                return (unitOL (instr off), szb)
        case szb of
            1 -> done PUSH8
            2 -> done PUSH16
            4 -> done PUSH32
            _ -> pushAtom d p va

pushConstrAtom d p expr = pushAtom d p expr

pushPadding :: ByteOff -> (BCInstrList, ByteOff)
pushPadding (ByteOff n) = go n (nilOL, 0)
  where
    go n acc@(!instrs, !off) = case n of
        0 -> acc
        1 -> (instrs `mappend` unitOL PUSH_PAD8, off + 1)
        2 -> (instrs `mappend` unitOL PUSH_PAD16, off + 2)
        3 -> go 1 (go 2 acc)
        4 -> (instrs `mappend` unitOL PUSH_PAD32, off + 4)
        _ -> go (n - 4) (go 4 acc)

-- -----------------------------------------------------------------------------
-- Given a bunch of alts code and their discrs, do the donkey work
-- of making a multiway branch using a switch tree.
-- What a load of hassle!

mkMultiBranch :: Maybe Int      -- # datacons in tycon, if alg alt
                                -- a hint; generates better code
                                -- Nothing is always safe
              -> [(Discr, BCInstrList)]
              -> BcM BCInstrList
mkMultiBranch maybe_ncons raw_ways = do
     lbl_default <- getLabelBc

     let
         mkTree :: [(Discr, BCInstrList)] -> Discr -> Discr -> BcM BCInstrList
         mkTree [] _range_lo _range_hi = return (unitOL (JMP lbl_default))
             -- shouldn't happen?

         mkTree [val] range_lo range_hi
            | range_lo == range_hi
            = return (snd val)
            | null defaults -- Note [CASEFAIL]
            = do lbl <- getLabelBc
                 return (testEQ (fst val) lbl
                            `consOL` (snd val
                            `appOL`  (LABEL lbl `consOL` unitOL CASEFAIL)))
            | otherwise
            = return (testEQ (fst val) lbl_default `consOL` snd val)

            -- Note [CASEFAIL]
            -- ~~~~~~~~~~~~~~~
            -- It may be that this case has no default
            -- branch, but the alternatives are not exhaustive - this
            -- happens for GADT cases for example, where the types
            -- prove that certain branches are impossible.  We could
            -- just assume that the other cases won't occur, but if
            -- this assumption was wrong (because of a bug in GHC)
            -- then the result would be a segfault.  So instead we
            -- emit an explicit test and a CASEFAIL instruction that
            -- causes the interpreter to barf() if it is ever
            -- executed.

         mkTree vals range_lo range_hi
            = let n = length vals `div` 2
                  (vals_lo, vals_hi) = splitAt n vals
                  v_mid = fst (head vals_hi)
              in do
              label_geq <- getLabelBc
              code_lo <- mkTree vals_lo range_lo (dec v_mid)
              code_hi <- mkTree vals_hi v_mid range_hi
              return (testLT v_mid label_geq
                      `consOL` (code_lo
                      `appOL`   unitOL (LABEL label_geq)
                      `appOL`   code_hi))

         the_default
            = case defaults of
                []         -> nilOL
                [(_, def)] -> LABEL lbl_default `consOL` def
                _          -> panic "mkMultiBranch/the_default"
     instrs <- mkTree notd_ways init_lo init_hi
     return (instrs `appOL` the_default)
  where
         (defaults, not_defaults) = partition (isNoDiscr.fst) raw_ways
         notd_ways = sortBy (comparing fst) not_defaults

         testLT (DiscrI i) fail_label = TESTLT_I i fail_label
         testLT (DiscrI8 i) fail_label = TESTLT_I8 (fromIntegral i) fail_label
         testLT (DiscrI16 i) fail_label = TESTLT_I16 (fromIntegral i) fail_label
         testLT (DiscrI32 i) fail_label = TESTLT_I32 (fromIntegral i) fail_label
         testLT (DiscrI64 i) fail_label = TESTLT_I64 (fromIntegral i) fail_label
         testLT (DiscrW i) fail_label = TESTLT_W i fail_label
         testLT (DiscrW8 i) fail_label = TESTLT_W8 (fromIntegral i) fail_label
         testLT (DiscrW16 i) fail_label = TESTLT_W16 (fromIntegral i) fail_label
         testLT (DiscrW32 i) fail_label = TESTLT_W32 (fromIntegral i) fail_label
         testLT (DiscrW64 i) fail_label = TESTLT_W64 (fromIntegral i) fail_label
         testLT (DiscrF i) fail_label = TESTLT_F i fail_label
         testLT (DiscrD i) fail_label = TESTLT_D i fail_label
         testLT (DiscrP i) fail_label = TESTLT_P i fail_label
         testLT NoDiscr    _          = panic "mkMultiBranch NoDiscr"

         testEQ (DiscrI i) fail_label = TESTEQ_I i fail_label
         testEQ (DiscrI8 i) fail_label = TESTEQ_I8 (fromIntegral i) fail_label
         testEQ (DiscrI16 i) fail_label = TESTEQ_I16 (fromIntegral i) fail_label
         testEQ (DiscrI32 i) fail_label = TESTEQ_I32 (fromIntegral i) fail_label
         testEQ (DiscrI64 i) fail_label = TESTEQ_I64 (fromIntegral i) fail_label
         testEQ (DiscrW i) fail_label = TESTEQ_W i fail_label
         testEQ (DiscrW8 i) fail_label = TESTEQ_W8 (fromIntegral i) fail_label
         testEQ (DiscrW16 i) fail_label = TESTEQ_W16 (fromIntegral i) fail_label
         testEQ (DiscrW32 i) fail_label = TESTEQ_W32 (fromIntegral i) fail_label
         testEQ (DiscrW64 i) fail_label = TESTEQ_W64 (fromIntegral i) fail_label
         testEQ (DiscrF i) fail_label = TESTEQ_F i fail_label
         testEQ (DiscrD i) fail_label = TESTEQ_D i fail_label
         testEQ (DiscrP i) fail_label = TESTEQ_P i fail_label
         testEQ NoDiscr    _          = panic "mkMultiBranch NoDiscr"

         -- None of these will be needed if there are no non-default alts
         (init_lo, init_hi) = case notd_ways of
            [] -> panic "mkMultiBranch: awesome foursome"
            (discr, _):_ -> case discr of
                DiscrI _ -> ( DiscrI minBound,  DiscrI maxBound )
                DiscrI8 _ -> ( DiscrI8 minBound, DiscrI8 maxBound )
                DiscrI16 _ -> ( DiscrI16 minBound, DiscrI16 maxBound )
                DiscrI32 _ -> ( DiscrI32 minBound, DiscrI32 maxBound )
                DiscrI64 _ -> ( DiscrI64 minBound, DiscrI64 maxBound )
                DiscrW _ -> ( DiscrW minBound,  DiscrW maxBound )
                DiscrW8 _ -> ( DiscrW8 minBound, DiscrW8 maxBound )
                DiscrW16 _ -> ( DiscrW16 minBound, DiscrW16 maxBound )
                DiscrW32 _ -> ( DiscrW32 minBound, DiscrW32 maxBound )
                DiscrW64 _ -> ( DiscrW64 minBound, DiscrW64 maxBound )
                DiscrF _ -> ( DiscrF minF,      DiscrF maxF )
                DiscrD _ -> ( DiscrD minD,      DiscrD maxD )
                DiscrP _ -> ( DiscrP algMinBound, DiscrP algMaxBound )
                NoDiscr -> panic "mkMultiBranch NoDiscr"

         (algMinBound, algMaxBound)
            = case maybe_ncons of
                 -- XXX What happens when n == 0?
                 Just n  -> (0, fromIntegral n - 1)
                 Nothing -> (minBound, maxBound)

         isNoDiscr NoDiscr = True
         isNoDiscr _       = False

         dec (DiscrI i) = DiscrI (i-1)
         dec (DiscrW w) = DiscrW (w-1)
         dec (DiscrP i) = DiscrP (i-1)
         dec other      = other         -- not really right, but if you
                -- do cases on floating values, you'll get what you deserve

         -- same snotty comment applies to the following
         minF, maxF :: Float
         minD, maxD :: Double
         minF = -1.0e37
         maxF =  1.0e37
         minD = -1.0e308
         maxD =  1.0e308


-- -----------------------------------------------------------------------------
-- Supporting junk for the compilation schemes

-- Describes case alts
data Discr
   = DiscrI Int
   | DiscrI8 Int8
   | DiscrI16 Int16
   | DiscrI32 Int32
   | DiscrI64 Int64
   | DiscrW Word
   | DiscrW8 Word8
   | DiscrW16 Word16
   | DiscrW32 Word32
   | DiscrW64 Word64
   | DiscrF Float
   | DiscrD Double
   | DiscrP Word16
   | NoDiscr
    deriving (Eq, Ord)

instance Outputable Discr where
   ppr (DiscrI i) = int i
   ppr (DiscrI8 i) = text (show i)
   ppr (DiscrI16 i) = text (show i)
   ppr (DiscrI32 i) = text (show i)
   ppr (DiscrI64 i) = text (show i)
   ppr (DiscrW w) = text (show w)
   ppr (DiscrW8 w) = text (show w)
   ppr (DiscrW16 w) = text (show w)
   ppr (DiscrW32 w) = text (show w)
   ppr (DiscrW64 w) = text (show w)
   ppr (DiscrF f) = text (show f)
   ppr (DiscrD d) = text (show d)
   ppr (DiscrP i) = ppr i
   ppr NoDiscr    = text "DEF"


lookupBCEnv_maybe :: Id -> BCEnv -> Maybe ByteOff
lookupBCEnv_maybe = Map.lookup

idSizeW :: Platform -> Id -> WordOff
idSizeW platform = WordOff . argRepSizeW platform . idArgRep platform

idSizeCon :: Platform -> Id -> ByteOff
idSizeCon platform var
  -- unboxed tuple components are padded to word size
  | isUnboxedTupleType (idType var) ||
    isUnboxedSumType (idType var) =
    wordsToBytes platform .
    WordOff . sum . map (argRepSizeW platform . toArgRep platform) .
    typePrimRep . idType $ var
  | otherwise = ByteOff (primRepSizeB platform (idPrimRepU var))

repSizeWords :: Platform -> PrimOrVoidRep -> WordOff
repSizeWords platform rep = WordOff $ argRepSizeW platform (toArgRepOrV platform rep)

isFollowableArg :: ArgRep -> Bool
isFollowableArg P = True
isFollowableArg _ = False

-- | Indicate if the calling convention is supported
isSupportedCConv :: CCallSpec -> Bool
isSupportedCConv (CCallSpec _ cconv _) = case cconv of
   CCallConv            -> True     -- we explicitly pattern match on every
   StdCallConv          -> False    -- convention to ensure that a warning
   PrimCallConv         -> True     -- is triggered when a new one is added
   JavaScriptCallConv   -> False
   CApiConv             -> True

-- See bug #10462
unsupportedCConvException :: a
unsupportedCConvException = throwGhcException (ProgramError
  ("Error: bytecode compiler can't handle some foreign calling conventions\n"++
   "  Workaround: use -fobject-code, or compile this module to .o separately."))

mkSlideB :: Platform -> ByteOff -> ByteOff -> OrdList BCInstr
mkSlideB platform nb db = mkSlideW n d
  where
    !n = bytesToWords platform nb
    !d = bytesToWords platform db

mkSlideW :: WordOff -> WordOff -> OrdList BCInstr
mkSlideW !n !ws
    | ws == 0
    = nilOL
    | otherwise
    = unitOL (SLIDE n $ fromIntegral ws)



atomRep :: Platform -> StgArg -> ArgRep
atomRep platform e = toArgRepOrV platform (stgArgRep1 e)

-- | Let szsw be the sizes in bytes of some items pushed onto the stack, which
-- has initial depth @original_depth@.  Return the values which the stack
-- environment should map these items to.
mkStackOffsets :: ByteOff -> [ByteOff] -> [ByteOff]
mkStackOffsets original_depth szsb = tail (scanl' (+) original_depth szsb)

typeArgReps :: Platform -> Type -> [ArgRep]
typeArgReps platform = map (toArgRep platform) . typePrimRep

-- -----------------------------------------------------------------------------
-- The bytecode generator's monad

data BcM_State
   = BcM_State
        { bcm_hsc_env :: HscEnv
        , thisModule  :: Module          -- current module (for breakpoints)
        , nextlabel   :: Word32          -- for generating local labels
        , modBreaks   :: Maybe ModBreaks -- info about breakpoints

        , breakInfo   :: IntMap CgBreakInfo -- ^ Info at breakpoint occurrence.
                                            -- Indexed with breakpoint *info* index.
                                            -- See Note [Breakpoint identifiers]
                                            -- in GHC.Types.Breakpoint
        , breakInfoIdx :: !Int              -- ^ Next index for breakInfo array
        }

newtype BcM r = BcM (BcM_State -> IO (BcM_State, r)) deriving (Functor)

ioToBc :: IO a -> BcM a
ioToBc io = BcM $ \st -> do
  x <- io
  return (st, x)

runBc :: HscEnv -> Module -> Maybe ModBreaks
      -> BcM r
      -> IO (BcM_State, r)
runBc hsc_env this_mod modBreaks (BcM m)
   = m (BcM_State hsc_env this_mod 0 modBreaks IntMap.empty 0)

thenBc :: BcM a -> (a -> BcM b) -> BcM b
thenBc (BcM expr) cont = BcM $ \st0 -> do
  (st1, q) <- expr st0
  let BcM k = cont q
  (st2, r) <- k st1
  return (st2, r)

thenBc_ :: BcM a -> BcM b -> BcM b
thenBc_ (BcM expr) (BcM cont) = BcM $ \st0 -> do
  (st1, _) <- expr st0
  (st2, r) <- cont st1
  return (st2, r)

returnBc :: a -> BcM a
returnBc result = BcM $ \st -> (return (st, result))

instance Applicative BcM where
    pure = returnBc
    (<*>) = ap
    (*>) = thenBc_

instance Monad BcM where
  (>>=) = thenBc
  (>>)  = (*>)

instance HasDynFlags BcM where
    getDynFlags = BcM $ \st -> return (st, hsc_dflags (bcm_hsc_env st))

getHscEnv :: BcM HscEnv
getHscEnv = BcM $ \st -> return (st, bcm_hsc_env st)

getProfile :: BcM Profile
getProfile = targetProfile <$> getDynFlags

shouldAddBcoName :: BcM (Maybe Module)
shouldAddBcoName = do
  add <- gopt Opt_AddBcoName <$> getDynFlags
  if add
    then Just <$> getCurrentModule
    else return Nothing

getLabelBc :: BcM LocalLabel
getLabelBc
  = BcM $ \st -> do let nl = nextlabel st
                    when (nl == maxBound) $
                        panic "getLabelBc: Ran out of labels"
                    return (st{nextlabel = nl + 1}, LocalLabel nl)

getLabelsBc :: Word32 -> BcM [LocalLabel]
getLabelsBc n
  = BcM $ \st -> let ctr = nextlabel st
                 in return (st{nextlabel = ctr+n}, coerce [ctr .. ctr+n-1])

newBreakInfo :: CgBreakInfo -> BcM Int
newBreakInfo info = BcM $ \st ->
  let ix = breakInfoIdx st
      st' = st
              { breakInfo = IntMap.insert ix info (breakInfo st)
              , breakInfoIdx = ix + 1
              }
  in return (st', ix)

getCurrentModule :: BcM Module
getCurrentModule = BcM $ \st -> return (st, thisModule st)

getCurrentModBreaks :: BcM (Maybe ModBreaks)
getCurrentModBreaks = BcM $ \st -> return (st, modBreaks st)

tickFS :: FastString
tickFS = fsLit "ticked"
