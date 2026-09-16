{-# LANGUAGE CPP                        #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DerivingVia #-}

--
--  (c) The University of Glasgow 2002-2006
--

-- | GHC.StgToByteCode: Generate bytecode from STG
module GHC.StgToByteCode ( UnlinkedBCO, byteCodeGen ) where

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

import GHCi.FFI
import GHC.Types.Basic
import GHC.Utils.Outputable
import GHC.Types.Name
import GHC.Types.Id
import GHC.Types.ForeignCall
import GHC.Core
import GHC.Types.Literal
import GHC.Types.Literal.Floating
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
import GHC.Builtin.WiredIn.Prim
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
import GHC.Runtime.Interpreter ( interpreterProfiled )
import GHC.Data.Bitmap
import GHC.Data.FlatBag as FlatBag
import GHC.Data.OrdList
import GHC.Data.Maybe
import GHC.Types.Tickish
import GHC.Types.SptEntry
import GHC.ByteCode.Breakpoints
import qualified GHC.HsToCore.Coverage as Coverage

import Data.List ( genericReplicate, intersperse, isSuffixOf
                 , partition, scanl', sortBy, zip4, zip6 )
import Foreign hiding (shiftL, shiftR)
import Control.Monad
import Data.Char

import GHC.Unit.Module

import Data.Coerce (coerce)
#if MIN_VERSION_rts(1,0,3)
import qualified Data.ByteString.Char8 as BS
#endif
import Data.IntMap (IntMap)
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import GHC.Types.Unique.Map (UniqMap)
import qualified GHC.Types.Unique.Map as UniqMap
import Data.Ord
import Data.Either ( partitionEithers )

import GHC.Stg.Syntax
import qualified Data.IntSet as IntSet
import GHC.CoreToIface

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.State  (StateT(..))
import Data.Bifunctor (Bifunctor(..))
import qualified GHC.Data.Strict as Strict

-- -----------------------------------------------------------------------------
-- Generating byte code for a complete module

byteCodeGen :: HscEnv
            -> Module
            -> [CgStgTopBinding]
            -> [TyCon]
            -> Maybe ModBreaks
            -> [SptEntry]
            -> Strict.Maybe ByteCodeHpcInfo
            -> IO CompiledByteCode
byteCodeGen hsc_env this_mod binds tycs mb_modBreaks spt_entries hpc_info
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
            -- See Note [Join points as labels]
            join_verdicts =
              joinPointVerdicts (profilePlatform profile) dflags LoopHasFallback binds

        (proto_bcos, BcM_State{..}) <-
           runBc hsc_env this_mod mb_modBreaks join_verdicts $ do
             let flattened_binds = concatMap flattenBind (reverse lifted_binds)
             FlatBag.fromList (fromIntegral $ length flattened_binds) <$> mapM schemeTopBind flattened_binds

        putDumpFileMaybe logger Opt_D_dump_BCOs
           "Proto-BCOs" FormatByteCode
           (vcat (intersperse (char ' ') (map ppr $ elemsFlatBag proto_bcos)))

        -- See Note [Join points as labels]
        putDumpFileMaybe logger Opt_D_dump_BCOs
           "Join points" FormatText
           (joinPointStats (profilePlatform profile) dflags binds join_verdicts)

        let mod_breaks = case mb_modBreaks of
             Nothing -> Nothing
             Just mb -> Just $ mkInternalModBreaks this_mod breakInfo mb
        cbc <- assembleBCOs profile proto_bcos tycs strings mod_breaks spt_entries hpc_info

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
type BCEnv = UniqMap Id StackDepth -- To find vars on the stack

-- | Identifies a BCO during code generation. See Note [Join points as labels].
newtype BcoId = BcoId Word
  deriving Eq

instance Outputable BcoId where
  ppr (BcoId n) = text "bco" <> ppr n

-- | The stack depth at the definition of a join point compiled as a label. A
-- jump slides its arguments down to this depth.
newtype JoinBaseDepth = JoinBaseDepth StackDepth

-- | A join point compiled as a label. See Note [Join points as labels].
data JoinTarget = JoinTarget
  { jt_label  :: !LocalLabel
  , jt_base   :: !JoinBaseDepth
  , jt_params :: ![Id]
  , jt_sequel :: !Sequel      -- ^ sequel of the let-no-escape
  , jt_bco    :: !BcoId       -- ^ the BCO containing the label
  , jt_slow   :: !(Maybe LocalLabel)
      -- ^ The slow path of a loop, set exactly while the RHS of the join
      -- point itself is compiled, where a jump to the label is a backward
      -- one and so needs a safepoint. 'Nothing' for a forward jump, which
      -- the code before it reached from the entry of the BCO, and for a join
      -- point that is not a loop. See Note [Join points as loops].
  }

-- | A join point compiled as a label in scope.
data JoinBinding
  = JoinLabel !JoinTarget
  -- | The label goes into a continuation BCO whose code is not generated yet
  -- ('PendingJoin'); there must be no jump to it before.
  | JoinLabelPending

-- | The join points compiled as labels in scope
type JoinEnv = UniqMap Id JoinBinding

-- | A join point whose label and RHS go into the continuation BCO of a case
-- nested in its let body. See Note [Join points as labels].
data PendingJoin = PendingJoin
  { pj_id     :: !Id
  , pj_params :: ![Id]
  , pj_rhs    :: !CgStgExpr
  , pj_fvs    :: ![Id]          -- ^ the free variables of the RHS in 'pj_env'
  , pj_env    :: !BCEnv         -- ^ the environment of the definition
  , pj_base   :: !JoinBaseDepth -- ^ the stack depth of the definition
  , pj_sequel :: !Sequel        -- ^ sequel of the let-no-escape
  }

-- | The pending join points in scope, by the binder of the case whose
-- continuation BCO gets their labels.
type PendingJoins = UniqMap Id [PendingJoin]

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
   -> ProtoBCO
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

schemeTopBind :: (Id, CgStgRhs) -> BcM ProtoBCO
schemeTopBind (id, rhs@(StgRhsCon _ dc _ _ args _))
  = do
    profile <- getProfile
    let
      non_voids = addArgReps (assertNonVoidStgArgs args)
      (tot_wds, --  #ptr_wds + #nonptr_wds
       ptr_wds, --  #ptr_wds
       nv_args_w_offsets) =
           -- Compute the runtime ordering for the datacon fields
           -- (Subword-sized fields are laid out contiguously, and padding is
           -- represented as literals of value 0 with the appropriate width)
           mkVirtHeapOffsetsWithPadding profile StdHeader non_voids
      contiguous_args_with_pad =
          litsWithPaddingToLits nv_args_w_offsets

    return ProtoStaticCon
      { protoStaticConName = getName id
      , protoStaticCon     = dc
      , protoStaticConData = [ case a of StgLitArg l -> Left l
                                         StgVarArg i -> Right i
                             | NonVoid a <- contiguous_args_with_pad ]
      , protoStaticConNonPtrsSize = tot_wds - ptr_wds
      , protoStaticConExpr = rhs
      }
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
    -- liftIO (putStrLn $ "top level BCO")
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
        -> BcM ProtoBCO
schemeR = schemeR_entry NoEntryLoop

-- | 'schemeR' for a BCO that may be the fallback closure of a join point
-- compiled as a loop. See Note [Join points as loops].
schemeR_entry :: EntryLoop -> [Id] -> (Name, CgStgRhs) -> BcM ProtoBCO
schemeR_entry entry_loop fvs (nm, rhs@(StgRhsClosure _ _ _ args body _))
   = schemeR_wrk entry_loop fvs nm rhs (args, body)
schemeR_entry entry_loop fvs (nm, rhs@(StgRhsCon _cc dc cnum _ticks args _type))
   -- unlike top-level StgRhsCon, which are static (see schemeTopBind),
   -- non-top-level StgRhsCon are compiled just like StgRhsClosure StgConApp
   = schemeR_wrk entry_loop fvs nm rhs ([], StgConApp dc cnum args [])

-- If an expression is a lambda, return the
-- list of arguments to the lambda (in R-to-L order) and the
-- underlying expression

-- | Whether the BCO being generated is the fallback closure of a join point
-- compiled as a loop, which binds that join point to a label at the entry of
-- the BCO. See Note [Join points as loops].
data EntryLoop = EntryLoop !Id | NoEntryLoop

schemeR_wrk
    :: EntryLoop
    -> [Id]
    -> Name
    -> CgStgRhs            -- expression e, for debugging only
    -> ([Var], CgStgExpr)  -- the args and body of an StgRhsClosure
    -> BcM ProtoBCO
schemeR_wrk entry_loop fvs nm original_body (args, body)
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
         p_init    = UniqMap.listToUniqMap (zip all_args (mkStackOffsets 0 szsb_args))

         -- make the arg bitmap
         bits = argBits platform (reverse (map (idArgRep platform) all_args))
         bitmap_size = strictGenericLength bits
         bitmap = mkBitmap platform bits
     body_code <- withNewBco $ case entry_loop of
       NoEntryLoop -> schemeER_wrk sum_szsb_args p_init body
       EntryLoop j -> schemeEntryLoop j args fvs sum_szsb_args p_init body

     pure (mkProtoBCO platform add_bco_name nm body_code (Right original_body)
                 arity bitmap_size bitmap False{-not alts-})

-- | Introduce break instructions for ticked expressions.
-- If no breakpoint information is available, the instruction is omitted.
schemeER_wrk :: StackDepth -> BCEnv -> CgStgExpr -> BcM BCInstrList
schemeER_wrk d p (StgTick bp@(Breakpoint tick_ty tick_id fvs) rhs) = do
  platform <- profilePlatform <$> getProfile

  -- When we find a tick we update the "last breakpoint location".
  -- We use it when constructing step-out BRK_FUNs in doCase
  -- See Note [Debugger: Stepout internal break locs]
  code <- withBreakTick bp $ schemeE d 0 p rhs

  -- As per Note [Stack layout when entering run_BCO], the breakpoint AP_STACK
  -- as we yield from the interpreter is headed by a stg_apply_interp + BCO to be a valid stack.
  -- Therefore, the var offsets are offset by 2 words
  let idOffSets = map (fmap (second (+2))) $
                  getVarOffSets platform d p fvs
      ty_vars   = tyCoVarsOfTypesWellScoped (tick_ty:map idType fvs)
      toWord :: Maybe (Id, WordOff) -> Maybe (Id, Word)
      toWord = fmap (\(i, wo) -> (i, fromIntegral wo))
      breakInfo = dehydrateCgBreakInfo ty_vars (map toWord idOffSets) tick_ty
                    (Right tick_id)

  mibi <- newBreakInfo breakInfo

  return $ case mibi of
    Nothing  -> code
    Just ibi -> BRK_FUN ibi `consOL` code

schemeER_wrk d p rhs = schemeE d 0 p rhs

-- | The body of the fallback closure of a join point compiled as a loop: a
-- prologue that rebuilds the entry frame, the label, the RHS with the join
-- point bound to that label, and the slow path. The fallback is therefore a
-- loop as well, and does not pay a call per iteration once a safepoint has
-- sent the primary copy here. See Note [Join points as loops].
--
-- The frame has to be rebuilt because a jump slides its arguments down to the
-- base of the label, and the entry frame is not somewhere they can be slid to:
-- the entry stack is the arguments with the free variables on top, as the
-- stored arguments of the PAP, and the free variables must stay -- one of them
-- is the join point that the slow path calls. So the prologue pushes a copy of
-- the free variables and then of the parameters, and one SLIDE drops the whole
-- entry frame from under them, leaving the free variables below the label's
-- base and the parameters above it. The back edge is then the same code as in
-- the primary copy, and nothing of the entry frame is left to be retained.
schemeEntryLoop :: Id -> [Id] -> [Id] -> StackDepth -> BCEnv -> CgStgExpr
                -> BcM BCInstrList
schemeEntryLoop j params fvs d p body = do
   platform <- profilePlatform <$> getProfile
   label <- getLabelBc
   slow <- getLabelBc
   bco <- getCurrentBco
   let fv_szsb    = map (joinParamSize platform) fvs
       param_szsb = map (joinParamSize platform) params
       -- the frame the prologue leaves: the free variables, then the
       -- parameters. The environment is built from scratch, not extended from
       -- the entry one, whose offsets the SLIDE invalidates.
       base   = sum fv_szsb
       p_loop = UniqMap.listToUniqMap
                  (zip (fvs ++ params) (mkStackOffsets 0 (fv_szsb ++ param_szsb)))
       target = JoinTarget { jt_label = label
                           , jt_base = JoinBaseDepth base
                           , jt_params = params
                           -- the sequel of the body of any BCO
                           , jt_sequel = 0
                           , jt_bco = bco
                           , jt_slow = Just slow }
       -- read from the entry environment: this is where the values still are
       copy !dd [] = return (dd, nilOL)
       copy !dd (x : rest) = do
         (push, szb) <- pushAtom dd p (StgVarArg x)
         massertPpr (szb == joinParamSize platform x)
           (text "schemeEntryLoop: size mismatch for" <+> ppr j
             $$ ppr x <+> ppr szb)
         (dd', more) <- copy (dd + szb) rest
         return (dd', push `appOL` more)
   (d_copies, copy_code) <- copy d (fvs ++ params)
   -- The copies are the entry frame again, so they are exactly as big as it
   -- is, and the frame the SLIDE leaves is as deep as the entry frame was.
   massertPpr (d_copies == d + d && base + sum param_szsb == d)
     (text "schemeEntryLoop: prologue of" <+> ppr j <+> text "ends at"
       <+> ppr d_copies <+> text "with base" <+> ppr base
       <+> text "expected" <+> ppr (d + d) <+> text "and entry depth" <+> ppr d)
   body_code <- withJoinPoint j target $ schemeER_wrk d p_loop body
   -- The slow path is an ordinary call of the join point, which enters this
   -- very closure again and so passes the checks of 'run_BCO_fun'. The join
   -- point is a free variable of its own RHS, so it is in the environment.
   -- Reversed: see the calling convention noted at 'doTailCall'.
   slow_code <- doTailCall d 0 p_loop j (reverse (map StgVarArg params))
   massertPpr (endsInControlTransfer body_code)
     (text "schemeEntryLoop: RHS falls through into the slow path of" <+> ppr j)
   return (copy_code `appOL` mkSlideB platform d d
             `appOL` (LABEL label `consOL` body_code)
             `appOL` (LABEL slow `consOL` slow_code))

-- | Get the offset in words into this breakpoint's AP_STACK which contains the matching Id
getVarOffSets :: Platform -> StackDepth -> BCEnv -> [Id] -> [Maybe (Id, WordOff)]
getVarOffSets platform depth env = map getOffSet
  where
    getOffSet id = case lookupBCEnv_maybe id env of
      Nothing     -> Nothing
      Just offset ->
          let !var_depth_ws = bytesToWords platform (depth - offset)
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
                       v `UniqMap.elemUniqMap` p]

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
schemeE d s p e@(StgApp f args) = do
   m_join <- lookupJoinBinding f
   case m_join of
     -- A jump to a join point compiled as a label
     Just (JoinLabel target) -> schemeJump d s p f target args
     Just JoinLabelPending ->
       pprPanic "schemeE: jump to a join point before its label is placed" (ppr f)
     Nothing
       | null args, isUnliftedType (idType f) -> returnUnliftedAtom d s p (StgVarArg f)
       -- Delegate tail-calls to schemeT.
       | otherwise -> schemeT d s p e
schemeE d s p e@(StgConApp {}) = schemeT d s p e
schemeE d s p e@(StgOpApp {}) = schemeT d s p e
schemeE d s p (StgLetNoEscape xlet bnd body) = do
   placement <- case bnd of
     StgNonRec j StgRhsClosure{} -> joinPlacement j
     StgRec [(j, StgRhsClosure{})] -> joinPlacement j
     _ -> pure Nothing
   case bnd of
     StgNonRec j rhs_closure@(StgRhsClosure _ _ _ params rhs _)
       | Just (ContPath []) <- placement
       -> schemeJoinPoint d s p j params rhs body
       -- the innermost continuation of the path gets the label
       | Just (ContPath (k : _)) <- placement
       -> schemeJoinPointInCont d s p j k (fvsToEnv p rhs_closure) params rhs body
     -- A self-recursive join point compiled as a loop. Its label is always in
     -- the BCO of the definition ('JoinRejectLoopInCont' is the rest), so
     -- there is no continuation case here.
     StgRec [(j, rhs_closure@(StgRhsClosure _ _ _ params rhs _))]
       | Just (ContPath []) <- placement
       -> schemeJoinPointLoop d s p j params rhs rhs_closure body
     -- Other join points are compiled like ordinary lets, i.e. as heap
     -- closures. See also Note [Join points and bytecode preparation] in
     -- GHC.Stg.BcPrep.
     _ -> schemeE d s p (StgLet xlet bnd body)
schemeE d s p (StgLet _xlet
                      (StgNonRec x (StgRhsCon _cc data_con _cnum _ticks args _typ))
                      body)
   = do -- Special case for a non-recursive let whose RHS is a
        -- saturated constructor application.
        -- Just allocate the constructor and carry on
        alloc_code <- mkConAppCode d s p data_con args
        platform <- targetPlatform <$> getDynFlags
        let !d2 = d + wordSize platform
        body_code <- schemeE d2 s (UniqMap.addToUniqMap p x d2) body
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
         stgRhsArity (StgRhsClosure _ _ _ args _ _) = strictGenericLength args
         stgRhsArity StgRhsCon{}                    = 0
         arities = map stgRhsArity rhss

         -- This p', d' defn is safe because all the items being pushed
         -- are ptrs, so all have size 1 word.  d' and p' reflect the stack
         -- after the closures have been allocated in the heap (but not
         -- filled in), and pointers to them parked on the stack.
         offsets = mkStackOffsets d (genericReplicate n_binds (wordSize platform))
         p' = UniqMap.addListToUniqMap p $ zipEqual xs offsets
         d' = d + wordsToBytes platform n_binds

         -- ToDo: don't build thunks for things with no free variables
         build_thunk
             :: StackDepth
             -> [Id]
             -> WordOff
             -> ProtoBCO
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

schemeE _d _s _p (StgTick (Breakpoint _ bp_id _) _rhs)
   = pprPanic "schemeE: Breakpoint without let binding:"
        (ppr bp_id <+> text "forgot to run bcPrep?")

schemeE d s p (StgTick (HpcTick mod ix) rhs) = do
   platform <- profilePlatform <$> getProfile
   rhs_code <- schemeE d s p rhs
   pure (unitOL (HPC_TICK (mkHpcTickBoxesLabell platform mod) (fromIntegral ix)) `appOL` rhs_code)

-- ignore other kinds of tick
schemeE d s p (StgTick _ rhs) = schemeE d s p rhs

-- no alts: scrut is guaranteed to diverge
schemeE d s p (StgCase scrut _ _ []) = schemeE d s p scrut

schemeE d s p (StgCase scrut bndr _ alts)
   = doCase d s p scrut bndr alts

-- | Compile @let-no-escape j = \params -> rhs in body@ with @j@ as a label in
-- the current BCO: the code of @body@, followed by the label and the code of
-- @rhs@. See Note [Join points as labels].
schemeJoinPoint :: StackDepth -> Sequel -> BCEnv -> Id -> [Id] -> CgStgExpr
                -> CgStgExpr -> BcM BCInstrList
schemeJoinPoint d s p j params rhs body = do
   platform <- profilePlatform <$> getProfile
   label <- getLabelBc
   bco <- getCurrentBco
   let target = JoinTarget { jt_label = label
                           , jt_base = JoinBaseDepth d
                           , jt_params = params
                           , jt_sequel = s
                           , jt_bco = bco
                           -- not a loop: every jump to this label is forward
                           , jt_slow = Nothing }
       -- A jump pushes the arguments in order, starting at depth d, and
       -- slides them down to d.
       param_szsb = map (joinParamSize platform) params
       p_rhs = UniqMap.addListToUniqMap p (zip params (mkStackOffsets d param_szsb))
       d_rhs = d + sum param_szsb
   body_code <- withJoinPoint j target $ schemeE d s p body
   -- The environment of the definition, not of any jump.
   rhs_code <- schemeE d_rhs s p_rhs rhs
   -- The body must not fall through into the label.
   massertPpr (endsInControlTransfer body_code)
     (text "schemeJoinPoint: body falls through into" <+> ppr j)
   return (body_code `appOL` (LABEL label `consOL` rhs_code))

-- | Compile @let-no-escape rec j = \params -> rhs in body@ as a loop: the
-- fallback closure, the code of @body@ with @j@ as a label in the current BCO,
-- then the label, the code of @rhs@, and the slow path. The closure is built
-- exactly as an ordinary recursive let would build it, and is what the slow
-- path calls. See Note [Join points as loops].
schemeJoinPointLoop :: StackDepth -> Sequel -> BCEnv -> Id -> [Id] -> CgStgExpr
                    -> CgStgRhs -> CgStgExpr -> BcM BCInstrList
schemeJoinPointLoop d s p j params rhs rhs_closure body = do
   platform <- profilePlatform <$> getProfile
   label <- getLabelBc
   slow <- getLabelBc
   bco <- getCurrentBco
   let -- The fallback closure sits on the stack under everything the loop
       -- uses, where an ordinary let would have put it; the loop's base is
       -- above it, so a jump never slides it away.
       !d_clo = d + wordSize platform
       p_clo = UniqMap.addToUniqMap p j d_clo
       -- computed in the environment that has j, so the closure captures
       -- itself: that is what makes the slow path's call reach it
       fvs = fvsToEnv p_clo rhs_closure
       size_w = sum (map (idSizeW platform) fvs)
       arity = strictGenericLength params
       param_szsb = map (joinParamSize platform) params
       p_rhs = UniqMap.addListToUniqMap p_clo
                 (zip params (mkStackOffsets d_clo param_szsb))
       d_rhs = d_clo + sum param_szsb
       target slow_path = JoinTarget { jt_label = label
                                     , jt_base = JoinBaseDepth d_clo
                                     , jt_params = params
                                     , jt_sequel = s
                                     , jt_bco = bco
                                     , jt_slow = slow_path }
       push_fvs !dd [] = return (dd, nilOL)
       push_fvs !dd (fv : rest) = do
         (push, szb) <- pushAtom dd p_clo (StgVarArg fv)
         (dd', more) <- push_fvs (dd + szb) rest
         return (dd', push `appOL` more)
   -- The fallback is the same RHS compiled as a closure, but with a label at
   -- its entry, so that it is a loop too. See 'schemeEntryLoop'.
   fallback <- schemeR_entry (EntryLoop j) fvs (getName j, rhs_closure)
   (_, push_fv_code) <- push_fvs d_clo fvs
   let alloc_code
         | arity == 0 = unitOL (ALLOC_AP (fromIntegral size_w))
         | otherwise  = unitOL (ALLOC_PAP arity (fromIntegral size_w))
       mkap | arity == 0 = MKAP
            | otherwise  = MKPAP
       -- one binding, so the closure is one word under its payload
       fill_code = push_fv_code `snocOL` PUSH_BCO fallback
                     `snocOL` mkap (1 + size_w) (fromIntegral size_w)
   -- Forward jumps from the body, so no safepoint: the body reached them from
   -- the entry of this BCO.
   body_code <- withJoinPoint j (target Nothing) $ schemeE d_clo s p_clo body
   -- Backward jumps from the RHS, which is where the safepoint goes.
   rhs_code <- withJoinPoint j (target (Just slow)) $ schemeE d_rhs s p_rhs rhs
   -- The slow path takes over the stack the back edge built, which is the
   -- loop's base with the parameters on top, and calls the fallback with them.
   -- Reversed: see the calling convention noted at 'doTailCall'.
   slow_code <- doTailCall d_rhs s p_rhs j (reverse (map StgVarArg params))
   massertPpr (endsInControlTransfer body_code)
     (text "schemeJoinPointLoop: body falls through into" <+> ppr j)
   massertPpr (endsInControlTransfer rhs_code)
     (text "schemeJoinPointLoop: RHS falls through into the slow path of"
       <+> ppr j)
   return (alloc_code `appOL` fill_code `appOL` body_code
             `appOL` (LABEL label `consOL` rhs_code)
             `appOL` (LABEL slow `consOL` slow_code))

-- | Compile @let-no-escape j = \params -> rhs in body@ with @j@ as a label in
-- the continuation BCO of the case with binder @k@ in @body@: record the join
-- point for 'doCase', and compile @body@. See Note [Join points as labels].
schemeJoinPointInCont :: StackDepth -> Sequel -> BCEnv -> Id -> Id -> [Id] -> [Id]
                      -> CgStgExpr -> CgStgExpr -> BcM BCInstrList
schemeJoinPointInCont d s p j k fvs params rhs body = do
   let pending = PendingJoin { pj_id = j
                             , pj_params = params
                             , pj_rhs = rhs
                             , pj_fvs = fvs
                             , pj_env = p
                             , pj_base = JoinBaseDepth d
                             , pj_sequel = s }
   body_code <- withPendingJoin k pending $ schemeE d s p body
   placed <- isJoinPlaced j
   unless placed $
     pprPanic "schemeJoinPointInCont: no continuation for the label of"
       (ppr j <+> text "in the case of" <+> ppr k)
   return body_code

-- | Place the labels and RHSs of the pending join points of a continuation
-- BCO: generate the code of the alternatives with the join points in scope,
-- followed by the labels and RHSs, innermost join point first. The stack of
-- the continuation BCO below its frame is the stack of the enclosing BCO,
-- which has the stack of the definition below. See Note [Join points as labels].
placePendingJoins :: BCEnv -> [PendingJoin] -> BcM BCInstrList -> BcM BCInstrList
placePendingJoins _ [] alts_code = alts_code
placePendingJoins p pendings alts_code = do
   platform <- profilePlatform <$> getProfile
   bco <- getCurrentBco
   labels <- mapM (const getLabelBc) pendings
   let target pj label = JoinTarget { jt_label = label
                                    , jt_base = pj_base pj
                                    , jt_params = pj_params pj
                                    , jt_sequel = pj_sequel pj
                                    , jt_bco = bco
                                    -- not a loop: every jump here is forward
                                    , jt_slow = Nothing }
       with_targets act =
         foldr (\(pj, label) -> withJoinPoint (pj_id pj) (target pj label)) act
               (zip pendings labels)
       place_rhs pj label = do
         let JoinBaseDepth base = pj_base pj
             p_def = pj_env pj
             param_szsb = map (joinParamSize platform) (pj_params pj)
             p_rhs = UniqMap.addListToUniqMap p_def
                       (zip (pj_params pj) (mkStackOffsets base param_szsb))
             d_rhs = base + sum param_szsb
         -- The RHS reads its free variables where the definition had them.
         massertPpr (and [ UniqMap.lookupUniqMap p v == UniqMap.lookupUniqMap p_def v
                         | v <- pj_fvs pj ])
           (text "placePendingJoins: free variables moved for" <+> ppr (pj_id pj))
         markJoinPlaced (pj_id pj)
         rhs_code <- schemeE d_rhs (pj_sequel pj) p_rhs (pj_rhs pj)
         return (LABEL label `consOL` rhs_code)
   with_targets $ do
     code <- alts_code
     -- The alternatives must not fall through into the labels.
     massertPpr (endsInControlTransfer code)
       (text "placePendingJoins: alternatives fall through into"
         <+> ppr (map pj_id pendings))
     rhs_codes <- zipWithM place_rhs pendings labels
     return (code `appOL` concatOL rhs_codes)

-- | Compile a jump @j args@ to a join point compiled as a label: push the
-- arguments, slide them down to the join point's base depth, and jump.
-- See Note [Join points as labels].
schemeJump :: StackDepth -> Sequel -> BCEnv -> Id -> JoinTarget -> [StgArg]
           -> BcM BCInstrList
schemeJump d s p j target args = do
   platform <- profilePlatform <$> getProfile
   bco <- getCurrentBco
   let JoinBaseDepth base = jt_base target
       params = jt_params target
   when (bco /= jt_bco target) $
     pprPanic "schemeJump: jump from another BCO"
       (ppr j <+> text "defined in" <+> ppr (jt_bco target)
              <+> text "jumped to from" <+> ppr bco)
   massertPpr (s == jt_sequel target && base <= d)
     (text "schemeJump: bad stack for jump to" <+> ppr j
       $$ text "depth:" <+> ppr d <+> text "base:" <+> ppr base
       $$ text "sequel:" <+> ppr s <+> text "expected:" <+> ppr (jt_sequel target))
   let push_args !dd [] = return (dd, nilOL)
       push_args !dd ((param, arg) : rest) = do
         (push, szb) <- pushAtom dd p arg
         -- the RHS finds the parameter at this size (void ones take none)
         massertPpr (szb == joinParamSize platform param)
           (text "schemeJump: argument size mismatch for" <+> ppr j
             $$ ppr param <+> ppr arg <+> ppr szb)
         (dd', more) <- push_args (dd + szb) rest
         return (dd', push `appOL` more)
   (d_args, push_code) <- push_args d (zipEqual params args)
   -- The slow path of a loop continues from the stack the SLIDE leaves, so
   -- that stack must be exactly the one the label itself expects: the base of
   -- the join point with the parameters on top and nothing else.
   massertPpr (d_args - d == sum (map (joinParamSize platform) params))
     (text "schemeJump: argument block of" <+> ppr j <+> text "is"
       <+> ppr (d_args - d) <+> text "bytes, expected"
       <+> ppr (sum (map (joinParamSize platform) params)))
   let -- The safepoint of a backward jump. A loop stays inside one BCO, so it
       -- passes no heap or context-switch check on its own; without this it
       -- would never yield. See Note [Join points as loops].
       check = case jt_slow target of
                 Nothing   -> nilOL
                 Just slow -> unitOL (YIELD_CHECK slow)
   return (push_code `appOL`
           mkSlideB platform (d_args - d) (d - base) `appOL`
           check `snocOL`
           JMP (jt_label target))

-- | Which kind of closure the RHS of a join point rejected with this verdict is.
joinClosureKind :: Maybe JoinPointVerdict -> ClosureKind
joinClosureKind (Just JoinRejectRecursive{}) = ClosureRecJoin
-- Also the RHS of a recursive join point that stays a closure, so it counts as
-- one: this is not a further class of occurrence, and keeping it here is what
-- makes the old 'in-rec-join' count the sum of the new 'in-rec-join' and
-- 'in-loop-join'.
joinClosureKind (Just JoinRejectLoopPlacedJoin) = ClosureRecJoin
-- Likewise a loop that is not emitted because its label would go into a
-- continuation BCO: it stays the closure a recursive join point is today.
joinClosureKind (Just JoinRejectLoopInCont) = ClosureRecJoin
-- Still a closure, and stays one after loops are emitted: the RHS of a loop is
-- compiled a second time into the fallback closure, so an enclosing join point
-- with an occurrence there is rejected then as it is now. These are counted
-- apart because they are what the *next* step would newly reach -- duplicating
-- the RHS into the fallback, or a fallback that is not a closure. See
-- Note [Join points as loops].
joinClosureKind (Just JoinAsLoop{}) = ClosureLoopJoin
joinClosureKind (Just JoinRejectCrossBco{}) = ClosureCrossJoin
joinClosureKind (Just _) = ClosureOtherJoin
joinClosureKind Nothing = ClosureLet

-- | The stack space a join point parameter takes when compiled as a label.
joinParamSize :: Platform -> Id -> ByteOff
joinParamSize platform = wordsToBytes platform . idSizeW platform

-- | Does the code end in an instruction that never falls through?
endsInControlTransfer :: BCInstrList -> Bool
endsInControlTransfer code
  | isNilOL code = False
  | otherwise = case lastOL code of
      ENTER -> True
      RETURN{} -> True
      RETURN_TUPLE -> True
      JMP{} -> True
      CASEFAIL -> True
      PRIMCALL -> True  -- returns to the scheduler
      _ -> False


{-
   Ticked Expressions
   ------------------

  The idea is that the "breakpoint<n,fvs> E" is really just an annotation on
  the code. When we find such a thing, we pull out the useful information,
  and then compile the code as if it was just the expression E.
-}

-- | Compile an expression that cannot leave the current BCO, leaving its value
-- on top of the stack instead of returning it. Also returns the size of that
-- value in bytes.
-- 'Nothing' if the expression might leave the BCO. The decision depends only
-- on the expression and the platform, not on the stack depth or environment
-- (which is why they are arguments of the result), and runs no 'BcM' action.
-- See Note [Inlined case continuations].
schemeIntoStack :: Platform -> CgStgExpr
                -> Maybe (StackDepth -> BCEnv -> BcM (BCInstrList, ByteOff))
schemeIntoStack platform e = case e of
  StgLit lit -> Just $ \d p -> pushAtom d p (StgLitArg lit)
  StgApp x []
    | isUnliftedType (idType x) -> Just $ \d p -> pushAtom d p (StgVarArg x)
  _ | Just _ <- maybe_is_tagToEnum_call e -> Nothing
  StgOpApp (StgPrimOp op) args _ty -> do
    compute <- doPrimOpCode platform op args
    Just $ \d p -> do
      (prim_code, width) <- compute d p
      return (prim_code, wordsToBytes platform (primOpResultWords platform width))
  StgConApp con _cn args _tys
    | not (isUnboxedTupleDataCon con || isUnboxedSumDataCon con)
    -> Just $ \d p -> do
      alloc_con <- mkConAppCode d d p con args
      return (alloc_con, wordSize platform)
  StgTick (HpcTick tick_mod ix) rhs -> do
    compute <- schemeIntoStack platform rhs
    Just $ \d p ->
      first (HPC_TICK (mkHpcTickBoxesLabell platform tick_mod) (fromIntegral ix) `consOL`)
        <$> compute d p
  StgTick Breakpoint{} _ -> Nothing
  StgTick _ rhs -> schemeIntoStack platform rhs
  StgLetNoEscape xlet bnd body -> schemeIntoStack platform (StgLet xlet bnd body)
  StgLet _xlet (StgNonRec x (StgRhsCon _cc data_con _cnum _ticks args _typ)) body -> do
    compute_body <- schemeIntoStack platform body
    Just $ \d p -> do
      let !d2 = d + wordSize platform
      alloc_code <- mkConAppCode d d p data_con args
      (body_code, szb) <- compute_body d2 (UniqMap.addToUniqMap p x d2)
      -- drop the let-bound constructor from under the value
      return ( alloc_code `appOL` body_code `appOL`
               unitOL (SLIDE (bytesToWords platform szb) 1)
             , szb )
  _ -> Nothing

-- | Does a case on this binder use a tuple return frame? Unboxed tuples and
-- sums with at most one non-void component, like @(# Word# #)@ or
-- @(# Int#, State# RealWorld #)@, do not: they have the same runtime rep as
-- that component, and use the more efficient single-value return frames.
ubxTupleFrame :: Platform -> Id -> Bool
ubxTupleFrame platform bndr =
  (isUnboxedTupleType bndr_ty || isUnboxedSumType bndr_ty) &&
  length (typeArgReps platform bndr_ty) > 1
  where
    bndr_ty = idType bndr

-- | For @case scrut of bndr { alts }@: if the alternatives are compiled inline
-- into the current BCO, the code pushing the scrutinee's value; 'Nothing' if
-- the case uses a continuation BCO. Pure, and independent of the stack depth
-- and environment, so the decision can be made before any code is generated.
-- See Note [Inlined case continuations].
inlinedCaseScrutinee :: Platform -> DynFlags -> CgStgExpr -> Id
                     -> Maybe (StackDepth -> BCEnv -> BcM (BCInstrList, ByteOff))
inlinedCaseScrutinee platform dflags scrut bndr
  | gopt Opt_BcInlineCaseConts dflags
  , not (ubxTupleFrame platform bndr)
  = schemeIntoStack platform scrut
  | otherwise
  = Nothing

-- | Compile code to do a tail call.  Specifically, push the fn,
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

schemeT d s p (StgOpApp (StgPrimCallOp (PrimCall label _)) args result_ty)
   = generatePrimCall d s p label result_ty args

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

-- | Tail call @fn@ with @args@.
--
-- The arguments are pushed in list order, so the LAST element of @args@ ends
-- up nearest the closure and is therefore the one applied FIRST. Callers that
-- have the arguments in source order must pass @reverse args@; every call
-- below does.
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
        platform <- profilePlatform <$> getProfile
        assert (null reps) return ()
        case lookupBCEnv_maybe fn p of
          Just d_v
            | d - d_v == 0  -- shortcut; the first thing on the stack is what we want to enter,
            , d_v <= init_d -- and it is between init_d and sequel (which will be dropped)
            -> do
              let slide = mkSlideB platform (d - init_d + wordSize platform)
                                            (init_d - s - wordSize platform)
              return (slide `appOL` unitOL ENTER)
          _ -> do
              (push_fn, sz) <- pushAtom d p (StgVarArg fn)
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

-- | Compile a primop in tail position: compute it inline, then slide the
-- result down to the sequel and return it.
-- 'Nothing' if the interpreter has no inline implementation of the primop.
doPrimOp  :: Platform
          -> PrimOp
          -> StackDepth
          -> Sequel
          -> BCEnv
          -> [StgArg]
          -> Maybe (BcM BCInstrList)
doPrimOp platform op init_d s p args = do
  compute <- doPrimOpCode platform op args
  Just $ do
    (prim_code, width) <- compute init_d p
    let slide = mkSlideW (primOpResultWords platform width)
                         (bytesToWords platform $ init_d - s)
                `snocOL` primOpReturn width
    return $ prim_code `appOL` slide

-- | The 'Width' that fixes the size of the result slot ('primOpResultWords')
-- and the return convention ('primOpReturn') of an inline primop.
--
-- For sized arithmetic and comparison operations this is the width of the
-- first argument; for the @IndexOffAddrOp_*@ operations it is the width of
-- the result. It is not the width of the result value in general (e.g.
-- comparisons produce an @Int#@ whatever their argument width), so do not use
-- it as such.
newtype PrimOpRetWidth = PrimOpRetWidth Width

-- | Size in words of the result an inline primop leaves on the stack.
primOpResultWords :: Platform -> PrimOpRetWidth -> WordOff
primOpResultWords platform (PrimOpRetWidth width)
  | platformWordWidth platform < width = 2
  | otherwise = 1

-- | The RETURN instruction for the result of an inline primop.
primOpReturn :: PrimOpRetWidth -> BCInstr
primOpReturn (PrimOpRetWidth width)
  | W64 <- width = RETURN L -- L works for 64 bit on any platform
  | otherwise = RETURN N -- <64bit width, fits in word on all platforms

-- | Compile a primop so that its result is left on top of the stack, without
-- returning. Also returns the 'PrimOpRetWidth' of the operation.
-- 'Nothing' if the interpreter has no inline implementation of the primop;
-- this does not depend on the stack depth or environment.
doPrimOpCode :: Platform
             -> PrimOp
             -> [StgArg]
             -> Maybe (StackDepth -> BCEnv -> BcM (BCInstrList, PrimOpRetWidth))
doPrimOpCode platform op args =
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
        unexpectedRep = panic "doPrimOpCode: Unexpected argument rep"

    -- Push args, execute primop
    -- Decides width of operation based on first argument.
    sizedPrimOp op_inst = Just $ \init_d p -> do
      let width = primArg1Width (head args)
      prim_code <- mkPrimOpCode init_d p (op_inst width) $ args
      return (prim_code, PrimOpRetWidth width)

    -- primOpWithRep op w => operation @op@ resulting in result @w@ wide.
    primOpWithRep :: BCInstr -> Width
                  -> Maybe (StackDepth -> BCEnv -> BcM (BCInstrList, PrimOpRetWidth))
    primOpWithRep op_inst result_width = Just $ \init_d p -> do
      prim_code <- mkPrimOpCode init_d p op_inst $ args
      return (prim_code, PrimOpRetWidth result_width)

    -- Coerce the argument, requires them to be the same size
    mk_conv :: Width -> Maybe (StackDepth -> BCEnv -> BcM (BCInstrList, PrimOpRetWidth))
    mk_conv target_width = Just $ \init_d p -> do
      let width = primArg1Width (head args)
      massert (width == target_width)
      (push_code, _bytes) <- pushAtom init_d p (head args)
      return (push_code, PrimOpRetWidth target_width)

-- Push the arguments on the stack and emit the given instruction
-- Pushes at least one word per non void arg.
mkPrimOpCode
    :: StackDepth
    -> BCEnv
    -> BCInstr                  -- The operator
    -> [StgArg]                 -- Args, in *reverse* order (must be fully applied)
    -> BcM BCInstrList
mkPrimOpCode orig_d p op_inst args = app_code
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
        non_void_arg_reps = typeArgReps platform bndr_ty
        ubx_tuple_frame = ubxTupleFrame platform bndr

        -- Are the alternatives compiled inline? Decided before generating any
        -- code. See Note [Inlined case continuations]
        m_inline_scrut = inlinedCaseScrutinee platform (hsc_dflags hsc_env) scrut bndr

        profiling
          | Just interp <- hsc_interp hsc_env
          = interpreterProfiled interp
          | otherwise = False

        -- Top of stack is the return itbl, as usual.
        -- underneath it is the pointer to the alt_code BCO.
        -- When an alt is entered, it assumes the returned value is
        -- on top of the itbl; see Note [Return convention for non-tuple values]
        -- for details.
        -- Whether this tuple return uses a small stg_ctoi_tN frame
        -- (no old_spill slot, no TSO access) instead of the generic
        -- stg_ctoi_t frame.
        small_tuple_frame :: Bool
        small_tuple_frame =
          ubx_tuple_frame && nativeCallStackSpillSize call_info <= mAX_SMALL_TUPLE_CTOI

        ctoi_frame_header_w :: WordOff
        ctoi_frame_header_w
          | small_tuple_frame =
              if profiling then 5 else 4
          | ubx_tuple_frame =
              if profiling then 6 else 5
          | otherwise = 2

        -- The size of the ret_*_info frame header, whose frame returns the
        -- value to the case continuation frame (ctoi_*_info)
        ret_info_header_w :: WordOff
          | ubx_tuple_frame = 3
          | otherwise = 1

        -- The stack space used to save/restore the CCCS when profiling
        save_ccs_size_b | profiling &&
                          not ubx_tuple_frame = 2 * wordSize platform
                        | otherwise = 0

        (bndr_size, call_info, args_offsets)
           | ubx_tuple_frame =
               let bndr_reps = typePrimRep (idType bndr)
                   (call_info, args_offsets) =
                       layoutNativeCall profile NativeTupleReturn 0 id bndr_reps
               in ( nativeCallSize call_info
                  , call_info
                  , args_offsets
                  )
           | otherwise = ( idSizeW platform bndr
                         , voidTupleReturnInfo
                         , []
                         )

        -- Depth of stack after the return value has been pushed
        -- This is the stack depth at the continuation.
        d_bndr =
            d + wordsToBytes platform bndr_size

        -- Env in which to compile the alts, not including
        -- any vars bound by the alts themselves
        p_alts = UniqMap.addToUniqMap p bndr d_bndr

        bndr_ty = idType bndr
        isAlgCase = isAlgType bndr_ty

        -- given an alt, return a discr and code for it.
        codeAlt :: CgStgAlt -> BcM (Discr, BCInstrList)
        codeAlt GenStgAlt{alt_con=DEFAULT,alt_bndrs=_,alt_rhs=rhs}
           = do rhs_code <- schemeE d_bndr s p_alts rhs
                return (NoDiscr, rhs_code)

        codeAlt alt@GenStgAlt{alt_con=_, alt_bndrs=bndrs, alt_rhs=rhs}
           -- primitive or nullary constructor alt: no need to UNPACK
           | null real_bndrs = do
                rhs_code <- schemeE d_bndr s p_alts rhs
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

                 stack_bot = d_bndr

                 p' = UniqMap.addListToUniqMap p_alts
                        [ (arg, tuple_start -
                                wordsToBytes platform (nativeCallSize call_info) +
                                offset)
                        | (NonVoid arg, offset) <- args_offsets]
             in do
               rhs_code <- schemeE stack_bot s p' rhs
               return (NoDiscr, rhs_code)
           -- algebraic alt with some binders
           | otherwise =
             let (tot_wds, _ptrs_wds, args_offsets) =
                     mkVirtHeapOffsets profile NoHeader
                         (addIdReps (assertNonVoidIds real_bndrs))
                 size = WordOff tot_wds

                 stack_bot = d_bndr + wordsToBytes platform size

                 -- convert offsets from Sp into offsets into the virtual stack
                 p' = UniqMap.addListToUniqMap p_alts
                        [ (arg, stack_bot - ByteOff offset)
                        | (NonVoid arg, offset) <- args_offsets ]

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
              LitFloating LitFloat  x  -> DiscrF (litFloatingToHostFloat  x)
              LitFloating LitDouble x  -> DiscrD (litFloatingToHostDouble x)
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

        -- unboxed tuples get extra words in the ctoi frame after the
        -- info pointer and cont_BCO:
        --   call_info, tuple_BCO, [old_spill], [CCCS]
        -- tuple_BCO at position 1 is a pointer.
        -- Small frames (stg_ctoi_tN) omit the old_spill slot.
        (extra_pointers, extra_slots)
           | small_tuple_frame && profiling = ([1], 3) -- call_info, tuple_BCO, CCCS
           | small_tuple_frame              = ([1], 2) -- call_info, tuple_BCO
           | ubx_tuple_frame && profiling = ([1], 4) -- call_info, tuple_BCO, old_spill, CCCS
           | ubx_tuple_frame              = ([1], 3) -- call_info, tuple_BCO, old_spill
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
          rel_slots = IntSet.toAscList $ UniqMap.nonDetFoldUniqMap go IntSet.empty p
          go (var, offset) !acc
            | isUnboxedTupleType (idType var) || isUnboxedSumType (idType var)
            = acc
            | isFollowableArg (idArgRep platform var)
            = fromIntegral (bytesToWords platform (d - offset)) `IntSet.insert` acc
            | otherwise = acc

        bitmap = intsToReverseBitmap platform bitmap_size' pointers

     let alts_code = mapM codeAlt alts >>= mkMultiBranch maybe_ncons

     case m_inline_scrut of
       -- See Note [Inlined case continuations]
       Just compute_scrut -> do
          pendings <- takePendingJoins bndr
          unless (null pendings) $
            pprPanic "doCase: join point labels for the inlined case of"
              (ppr bndr <+> ppr (map pj_id pendings))
          alt_final0 <- alts_code
          (scrut_code, szb) <- compute_scrut d p
          -- A mismatch means the alternatives would read the wrong stack
          -- slots. Panic rather than falling back to the frame: a fallback
          -- would hide a layout bug in 'schemeIntoStack'.
          when (szb /= wordsToBytes platform bndr_size) $
            pprPanic "doCase: inlined scrutinee size mismatch"
              (vcat [ text "binder:" <+> ppr bndr <+> dcolon <+> ppr (idType bndr)
                    , text "scrutinee pushed (bytes):" <+> ppr szb
                    , text "binder size (words):" <+> ppr bndr_size ])
          return (scrut_code `appOL` alt_final0)
       Nothing -> do
          -- The alternatives go into the continuation BCO (the scrutinee
          -- stays in the current one).
          pendings <- takePendingJoins bndr
          alt_final0 <- withNewBco $ placePendingJoins p pendings alts_code
          let

              -- drop the stg_ctoi_*_info header...
              alt_final1 = SLIDE bndr_size ctoi_frame_header_w `consOL` alt_final0

              -- after dropping the stg_ret_*_info header
              alt_final2 = SLIDE 0 ret_info_header_w `consOL` alt_final1

          -- When entering a case continuation BCO, the stack is always headed
          -- by the stg_ret frame and the stg_ctoi frame that returned to it.
          -- See Note [Stack layout when entering run_BCO]
          --
          -- Right after the breakpoint instruction, a case continuation BCO
          -- drops the stg_ret and stg_ctoi frame headers (see alt_final1,
          -- alt_final2), leaving the stack with the scrutinee followed by the
          -- free variables (with depth==d_bndr)
          alt_final <- getLastBreakTick >>= \case
            Just (Breakpoint tick_ty tick_id fvs)
              | gopt Opt_InsertBreakpoints (hsc_dflags hsc_env)
              -- Construct an internal breakpoint to put at the start of this case
              -- continuation BCO, for step-out.
              -- See Note [Debugger: Stepout internal break locs]
              -> do

               -- same fvs available in the surrounding tick are available in the case continuation

               -- The variable offsets into the yielded AP_STACK are adjusted
               -- differently because a case continuation AP_STACK has the
               -- additional stg_ret and stg_ctoi frame headers
               -- (as per Note [Stack layout when entering run_BCO]):
               let firstVarOff = ret_info_header_w+bndr_size+ctoi_frame_header_w
                   idOffSets = map (fmap (second (+firstVarOff))) $
                               getVarOffSets platform d p fvs
                   ty_vars   = tyCoVarsOfTypesWellScoped (tick_ty:map idType fvs)
                   toWord :: Maybe (Id, WordOff) -> Maybe (Id, Word)
                   toWord = fmap (\(i, wo) -> (i, fromIntegral wo))
                   breakInfo = dehydrateCgBreakInfo ty_vars (map toWord idOffSets) tick_ty
                                 (Left (InternalBreakLoc tick_id))

               mibi <- newBreakInfo breakInfo
               return $ case mibi of
                 Nothing  -> alt_final2
                 Just ibi -> BRK_FUN ibi `consOL` alt_final2
            _ -> pure alt_final2

          add_bco_name <- shouldAddBcoName
          let
              alt_bco_name = getName bndr
              alt_bco = mkProtoBCO platform add_bco_name alt_bco_name alt_final (Left alts)
                            0{-no arity-} bitmap_size bitmap True{-is alts-}
          scrut_code <- schemeE (d + wordsToBytes platform ctoi_frame_header_w + save_ccs_size_b)
                                (d + wordsToBytes platform ctoi_frame_header_w + save_ccs_size_b)
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

-- -----------------------------------------------------------------------------
-- Join points as labels: eligibility analysis
-- See Note [Join points as labels]

-- | Whether a join point binding can be compiled as a label, and if not, why.
data JoinPointVerdict
  = JoinAsLabel             -- ^ a label in the BCO of the definition
  | JoinInCont !ContPath    -- ^ a label in a continuation BCO nested in it
  | JoinAsLoop              -- ^ self-recursive, a loop in the BCO of the definition
  | JoinRejectBreakpoints   -- ^ breakpoints are enabled
  | JoinRejectDisabled      -- ^ -fno-bc-join-points-as-labels
  | JoinRejectConRhs        -- ^ the RHS is a constructor application
  | JoinRejectRecGroup      -- ^ a group of several (mutually) recursive join points
  | JoinRejectRecursive !OtherwiseEligible  -- ^ self-recursive, not a loop
  | JoinRejectLoopInCont    -- ^ a loop whose label would go in a continuation BCO
  | JoinRejectLoopPlacedJoin -- ^ self-recursive, and a join point in its RHS is placed
  | JoinRejectCrossBco !CrossBco -- ^ the jumps are not all in one BCO
  deriving Eq

-- | The case continuation BCOs enclosing some code, relative to an enclosing
-- definition, named by the binders of their cases, innermost first. Empty for
-- code in the BCO of the definition itself.
newtype ContPath = ContPath [Id]
  deriving Eq

-- | Why the occurrences of a join point are not all jumps in one BCO.
data CrossBco
  = CrossSpread !SpreadShape   -- ^ jumps in more than one BCO, none in a closure
  | CrossClosure !ClosureKind  -- ^ some occurrence is in a closure RHS
  | CrossOther                 -- ^ some occurrence is not a jump in tail position
  deriving Eq

-- | How the BCOs of the jumps of a 'CrossSpread' join point are nested.
data SpreadShape
  = SpreadChain      -- ^ each BCO of a jump contains the next
  | SpreadSiblings   -- ^ two of them are in different branches
  deriving Eq

-- | The closure an occurrence of a 'CrossClosure' join point is in. STG
-- forbids occurrences under a lambda or in a thunk, so it should always be
-- the RHS of a join point that is compiled as a closure itself.
data ClosureKind
  = ClosureRecJoin    -- ^ the RHS of a recursive join point
  | ClosureLoopJoin   -- ^ the RHS of a join point that v2a would compile as a loop
  | ClosureCrossJoin  -- ^ the RHS of a join point rejected for cross-BCO jumps
  | ClosureOtherJoin  -- ^ the RHS of a join point rejected for another reason
  | ClosureLet        -- ^ an ordinary let, which STG should not allow
  deriving Eq

-- | Would a rejected join point be eligible without the reason it was rejected
-- for?
data OtherwiseEligible = OtherwiseEligible | NotOtherwiseEligible
  deriving Eq

-- | Does a loop's RHS exist a second time, as the fallback closure its
-- safepoint enters? 'LoopHasFallback' is what 'schemeJoinPointLoop' emits, and
-- the only mode whose verdicts decide code. 'LoopHasNoFallback' asks the same
-- question of a loop that is only a label, which is what the
-- @in-loop-join-placeable@ counter reports. See Note [Join points as loops].
data LoopFallback = LoopHasFallback | LoopHasNoFallback

-- | The verdicts for all join point binders of a module. Every binder of a
-- recursive group gets the group's verdict.
type JoinPointVerdicts = UniqMap Id JoinPointVerdict

-- | Where the label of a join point is, relative to its definition. A loop's
-- label is in the BCO of the definition, which is the only placement a loop
-- has: see 'JoinRejectLoopInCont' and Note [Join points as loops].
verdictPlacement :: JoinPointVerdict -> Maybe ContPath
verdictPlacement JoinAsLabel = Just (ContPath [])
verdictPlacement (JoinInCont path) = Just path
verdictPlacement JoinAsLoop = Just (ContPath [])
verdictPlacement _ = Nothing

-- | Would the join point's RHS be emitted into a BCO that already exists,
-- rather than into one of its own? What it decides is whether compiling an
-- enclosing RHS twice would place this join point twice.
verdictIsPlaced :: JoinPointVerdict -> Bool
verdictIsPlaced v = isJust (verdictPlacement v)

-- | Decide for every join point of the module whether it is compiled as a label.
joinPointVerdicts :: Platform -> DynFlags -> LoopFallback -> [CgStgTopBinding]
                  -> JoinPointVerdicts
joinPointVerdicts platform dflags fallback = foldl' top UniqMap.emptyUniqMap
  where
    top vs StgTopStringLit{} = vs
    top vs (StgTopLifted bind) = binds vs bind

    binds vs bind = foldl' rhs vs (bindRhss bind)
    rhs vs (StgRhsClosure _ _ _ _ body _) = expr vs body
    rhs vs StgRhsCon{} = vs

    expr vs e = case e of
      StgApp{} -> vs
      StgLit{} -> vs
      StgConApp{} -> vs
      StgOpApp{} -> vs
      StgCase scrut _ _ alts -> foldl' (\acc alt -> expr acc (alt_rhs alt)) (expr vs scrut) alts
      StgLet _ bind body -> expr (binds vs bind) body
      StgLetNoEscape _ bind body ->
        -- The verdict depends on the placement of the join points nested in
        -- the binding and the body, so decide them first.
        let !vs' = expr (binds vs bind) body
            verdict_of j = UniqMap.lookupUniqMap vs' j
            verdict = joinPointVerdict platform dflags fallback verdict_of bind body
        in UniqMap.addListToUniqMap vs' [ (j, verdict) | j <- bindBinders bind ]
      StgTick _ body -> expr vs body

-- | The verdict for the join point binding of @let-no-escape bind in body@,
-- given the verdicts of the join points nested in it.
joinPointVerdict :: Platform -> DynFlags -> LoopFallback
                 -> (Id -> Maybe JoinPointVerdict)
                 -> CgStgBinding -> CgStgExpr -> JoinPointVerdict
joinPointVerdict platform dflags fallback verdict_of bind body
  | gopt Opt_InsertBreakpoints dflags = JoinRejectBreakpoints
  | not (gopt Opt_BcJoinPointsAsLabels dflags) = JoinRejectDisabled
  | otherwise = case bind of
      StgNonRec _ StgRhsCon{} -> JoinRejectConRhs
      StgNonRec j (StgRhsClosure _ _ _ params rhs_body _)
        | exprMentions j rhs_body
        -> JoinRejectRecursive
             (recursive (sites j params body) (sites j params rhs_body))
        | otherwise
        -> notRecursive j params
      -- a self-recursive join point is a singleton group
      StgRec [(j, StgRhsClosure _ _ _ params rhs_body _)]
        -- A singleton group whose RHS does not mention its binder is not
        -- recursive: it has no back edge, so it is not a loop, and calling it
        -- one would count (and later emit) a loop that never jumps to itself.
        -- Classify it as the non-recursive join point it is.
        | not (exprMentions j rhs_body)
        -> notRecursive j params
        | otherwise
        -> let body_sites = sites j params body
               rhs_sites  = sites j params rhs_body
           in case singleBco body_sites of
                Right path
                  -- The two lists are not concatenated: a site in the body is
                  -- a path from the definition to the label, a site in the RHS
                  -- one from the label to itself.
                  | all (== SiteIn (ContPath [])) rhs_sites
                  -- The two restrictions are asked after the jump conditions,
                  -- so that each counter is exactly the join points that
                  -- lifting that one restriction would win, and not also the
                  -- ones rejected for their jumps anyway. The coverage limit
                  -- is asked before the placement one, being the coarser of
                  -- the two. See Note [Join points as loops].
                  -> if path /= ContPath []
                       then JoinRejectLoopInCont
                       else if rhsPlacesJoin rhs_body
                              then JoinRejectLoopPlacedJoin
                              else JoinAsLoop
                _ -> JoinRejectRecursive (recursive body_sites rhs_sites)
      StgRec{} -> JoinRejectRecGroup
  where
    sites j params = joinJumpSites platform dflags fallback verdict_of j (length params)

    -- The verdict of a join point whose RHS does not mention it: where its
    -- jumps in the body are decides where the label goes.
    notRecursive j params = case singleBco (sites j params body) of
      Right (ContPath []) -> JoinAsLabel
      Right path -> JoinInCont path
      Left cross -> JoinRejectCrossBco cross

    -- as for v1: all jumps in the body and the RHS in the defining BCO. For
    -- legal STG this is now always 'NotOtherwiseEligible': the other answer
    -- means the loop branch above fired, so this one was never reached. See
    -- Note [Join points as loops].
    recursive body_sites rhs_sites
      | all (== SiteIn (ContPath [])) (body_sites ++ rhs_sites)
      = OtherwiseEligible
      | otherwise
      = NotOtherwiseEligible

    -- Is some join point defined in this expression compiled into a BCO that
    -- already exists, rather than into one of its own? Only join points
    -- nested in the RHS are asked about, and their verdicts are decided
    -- before this one, so they are all in 'verdict_of'.
    rhsPlacesJoin e = case e of
      StgApp{} -> False
      StgLit{} -> False
      StgConApp{} -> False
      StgOpApp{} -> False
      StgCase scrut _ _ alts -> rhsPlacesJoin scrut || any (rhsPlacesJoin . alt_rhs) alts
      StgLet _ bnd e' -> bindPlacesJoin bnd || rhsPlacesJoin e'
      StgLetNoEscape _ bnd e' ->
        any (maybe False verdictIsPlaced . verdict_of) (bindBinders bnd)
          || bindPlacesJoin bnd || rhsPlacesJoin e'
      StgTick _ e' -> rhsPlacesJoin e'

    bindPlacesJoin bnd = any rhs_places (bindRhss bnd)
      where rhs_places (StgRhsClosure _ _ _ _ e' _) = rhsPlacesJoin e'
            rhs_places StgRhsCon{} = False

-- | The BCO that all the occurrences are jumps in, if there is one.
singleBco :: [JumpSite] -> Either CrossBco ContPath
singleBco sites
  | kind : _ <- [ k | SiteInClosure k <- sites ] = Left (CrossClosure kind)
  | SiteOther `elem` sites = Left CrossOther
  | otherwise = case sites of
      [] -> Right (ContPath [])  -- no jumps at all
      SiteIn path : rest
        | all (== SiteIn path) rest -> Right path
      _ -> Left (CrossSpread shape)
  where
    -- A path is the list of the case binders of the continuations enclosing
    -- the jump, innermost first, so one BCO contains another exactly if its
    -- path is a suffix of the other's.
    paths = [ ks | SiteIn (ContPath ks) <- sites ]
    shape | and [ p `isSuffixOf` q || q `isSuffixOf` p | p <- paths, q <- paths ]
          = SpreadChain
          | otherwise = SpreadSiblings

-- | An occurrence of a join point.
data JumpSite
  = SiteIn !ContPath          -- ^ a saturated jump, emitted into the BCO at this path
  | SiteInClosure !ClosureKind -- ^ any occurrence in the RHS of a closure
  | SiteOther                 -- ^ any other occurrence
  deriving Eq

-- | The occurrences of the join point @j@ in an expression, and which BCOs
-- their code is emitted into. The verdicts tell where the labels of the join
-- points nested in the expression are, and 'inlinedCaseScrutinee' which cases
-- have continuation BCOs, exactly as in code generation.
joinJumpSites :: Platform -> DynFlags -> LoopFallback
              -> (Id -> Maybe JoinPointVerdict) -> Id -> Int
              -> CgStgExpr -> [JumpSite]
joinJumpSites platform dflags fallback verdict_of j arity = go []
  where
    mentions_in args = any (argMentions j) args

    -- the path in reverse, innermost first
    go path e = case e of
      StgApp f args
        | f == j, length args == arity, not (mentions_in args) -> [SiteIn (ContPath path)]
        | f == j || mentions_in args -> [SiteOther]
        | otherwise -> []
      StgLit{} -> []
      StgConApp _ _ args _ -> [SiteOther | mentions_in args]
      StgOpApp _ args _ -> [SiteOther | mentions_in args]
      -- no alternatives: the scrutinee is compiled in tail position
      StgCase scrut _ _ [] -> go path scrut
      StgCase scrut bndr _ alts ->
        [SiteOther | exprMentions j scrut] ++
        if isJust (inlinedCaseScrutinee platform dflags scrut bndr)
          then concatMap (go path . alt_rhs) alts
          -- the alternatives go into a continuation BCO
          else concatMap (go (bndr : path) . alt_rhs) alts
      -- every let-bound RHS is a BCO of its own
      StgLet _ bind body -> [SiteInClosure ClosureLet | bindMentions j bind] ++ go path body
      StgLetNoEscape _ bind body
        | Just (j', StgRhsClosure _ _ _ _ rhs_body _) <- inlinedRhs bind
        , Just (ContPath rhs_path) <- verdictPlacement =<< verdict_of j'
        -- the RHS of a label is emitted where the label is
        -> go (rhs_path ++ path) rhs_body ++ go path body
        | otherwise
        -> [ SiteInClosure (joinClosureKind (verdict_of j''))
           | bindMentions j bind, j'' <- take 1 (bindBinders bind) ]
           ++ go path body
      StgTick tick body -> [SiteOther | tickMentions j tick] ++ go path body

    -- The binding whose RHS is emitted where its label is, rather than into a
    -- BCO of its own, if this is one. A loop's RHS is emitted there too, but a
    -- second copy of it goes into the fallback closure, so a jump into it is a
    -- jump into a closure and the loop is looked through only when the
    -- fallback is assumed away. See Note [Join points as loops].
    inlinedRhs (StgNonRec j' rhs) = Just (j', rhs)
    inlinedRhs (StgRec [(j', rhs)])
      | LoopHasNoFallback <- fallback
      , Just JoinAsLoop <- verdict_of j'
      = Just (j', rhs)
    inlinedRhs _ = Nothing

-- | Does the variable occur anywhere in the expression, including in closures
-- and breakpoint free variables?
exprMentions :: Id -> CgStgExpr -> Bool
exprMentions j = go
  where
    go e = case e of
      StgApp f args -> f == j || any (argMentions j) args
      StgLit{} -> False
      StgConApp _ _ args _ -> any (argMentions j) args
      StgOpApp _ args _ -> any (argMentions j) args
      StgCase scrut _ _ alts -> go scrut || any (go . alt_rhs) alts
      StgLet _ bind body -> bindMentions j bind || go body
      StgLetNoEscape _ bind body -> bindMentions j bind || go body
      StgTick tick body -> tickMentions j tick || go body

bindMentions :: Id -> CgStgBinding -> Bool
bindMentions j bind = any rhs_mentions (bindRhss bind)
  where
    rhs_mentions (StgRhsClosure _ _ _ _ body _) = exprMentions j body
    rhs_mentions (StgRhsCon _ _ _ _ args _) = any (argMentions j) args

argMentions :: Id -> StgArg -> Bool
argMentions j (StgVarArg v) = v == j
argMentions _ StgLitArg{} = False

tickMentions :: Id -> StgTickish -> Bool
tickMentions j (Breakpoint _ _ fvs) = j `elem` fvs
tickMentions _ _ = False

bindRhss :: GenStgBinding pass -> [GenStgRhs pass]
bindRhss (StgNonRec _ rhs) = [rhs]
bindRhss (StgRec pairs) = map snd pairs

bindBinders :: GenStgBinding pass -> [BinderP pass]
bindBinders (StgNonRec b _) = [b]
bindBinders (StgRec pairs) = map fst pairs

-- | Per-module counts of join point verdicts, for -ddump-bcos. The binds are
-- taken as well as their verdicts so that the counterfactual verdicts of
-- Note [Join points as loops] can be computed here, where nothing but the dump
-- can force them.
joinPointStats :: Platform -> DynFlags -> [CgStgTopBinding] -> JoinPointVerdicts
               -> SDoc
joinPointStats platform dflags binds verdict_map =
  text "join points:" <+> int (length verdicts) <+> text "total,"
    <+> number (== JoinAsLabel) <+> text "as labels,"
    <+> number isInCont <+> text "as labels in continuations"
    <+> parens (number (inContDepth (== 1)) <+> text "depth 1," <+>
                number (inContDepth (> 1)) <+> text "deeper") <> comma
    <+> number (== JoinAsLoop) <+> text "as loops,"
    <+> number isRecursive <+> text "rejected-recursive"
    <+> parens (number (== JoinRejectRecursive OtherwiseEligible)
                <+> text "otherwise eligible") <> comma
    <+> number (== JoinRejectLoopInCont) <+> text "loop-rejected-in-continuation,"
    <+> number (== JoinRejectLoopPlacedJoin) <+> text "loop-rejected-placed-join,"
    <+> number (== JoinRejectRecGroup) <+> text "rejected-rec-group,"
    <+> number (== JoinRejectConRhs) <+> text "rejected-con-rhs,"
    <+> number (== JoinRejectBreakpoints) <+> text "rejected-breakpoint,"
    <+> number (== JoinRejectDisabled) <+> text "rejected-disabled,"
    <+> number isCrossBco <+> text "rejected-cross-BCO"
    <+> parens (hsep (punctuate comma
          [ number (== JoinRejectCrossBco (CrossSpread SpreadChain))
              <+> text "spread-chain"
          , number (== JoinRejectCrossBco (CrossSpread SpreadSiblings))
              <+> text "spread-siblings"
          , number (== JoinRejectCrossBco (CrossClosure ClosureRecJoin))
              <+> text "in-rec-join"
          , number (== JoinRejectCrossBco (CrossClosure ClosureLoopJoin))
              <+> text "in-loop-join"
          , number (== JoinRejectCrossBco (CrossClosure ClosureCrossJoin))
              <+> text "in-cross-join"
          , number (== JoinRejectCrossBco (CrossClosure ClosureOtherJoin))
              <+> text "in-other-join"
          , number (== JoinRejectCrossBco (CrossClosure ClosureLet))
              <+> text "in-let"
          , number (== JoinRejectCrossBco CrossOther) <+> text "other" ])) <> comma
    <+> int in_loop_placeable <+> text "in-loop-join-placeable"
  where
    -- counting does not depend on the order
    verdicts = UniqMap.nonDetEltsUniqMap verdict_map
    -- What the analysis would say if a loop had no fallback copy of its RHS.
    -- This is its only use: it decides nothing, and it is forced only when
    -- -ddump-bcos forces this SDoc. See Note [Join points as loops].
    no_fallback = joinPointVerdicts platform dflags LoopHasNoFallback binds
    in_loop_placeable = count placeable (UniqMap.nonDetUniqMapToList verdict_map)
    placeable (j, verdict) =
      verdict == JoinRejectCrossBco (CrossClosure ClosureLoopJoin)
        && maybe False verdictIsPlaced (UniqMap.lookupUniqMap no_fallback j)
    number p = int (count p verdicts)
    isRecursive JoinRejectRecursive{} = True
    isRecursive _ = False
    isCrossBco JoinRejectCrossBco{} = True
    isCrossBco _ = False
    isInCont JoinInCont{} = True
    isInCont _ = False
    inContDepth p (JoinInCont (ContPath path)) = p (length path)
    inContDepth _ _ = False

{-
Note [Join points as labels]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
By default a join point ('StgLetNoEscape') is compiled like any other let: a
heap closure with a BCO of its own, entered at every jump. When all its jumps
are emitted into the BCO containing its definition, we instead compile it as a
label in that BCO, and each jump as a SLIDE and JMP. This saves allocating
the closure and its BCO, and a call through the closure at every jump.

Code shape. For @let-no-escape j = \params -> rhs in body@ at stack depth d
and sequel s ('schemeJoinPoint'):

      <body, compiled at depth d and sequel s, with j in the 'JoinEnv'>
  L:  <rhs, compiled at depth d + size params and sequel s, with the params
       bound just above d>

and a jump @j args@ at depth d' >= d ('schemeJump') is

      <push args in order, the first one deepest>
      SLIDE (size args) (d' - d)      -- omitted if d' == d
      JMP L

So the stack at L is that at the definition plus the arguments. The base
depth d ('JoinBaseDepth') is fixed at the definition. The RHS is compiled
with the definition's environment, not the one of any jump, even though its
code follows the body. Void parameters take no stack space at either side
('joinParamSize' and 'pushAtom' agree on this).

  * All jumps are forward, so these are no loops: we need no safepoint. This
    holds for several join points sharing one continuation BCO because they
    are placed innermost first: only the RHS of an inner join point can jump
    to an outer one, and the outer label follows it. The RHS of an outer join
    point cannot mention an inner one, which is not in scope there.
  * The body never falls through into L: every expression in tail position
    ends in a control transfer (ENTER, RETURN, RETURN_TUPLE, PRIMCALL, a JMP
    to a join point, or CASEFAIL in an incomplete inlined case).
    'schemeJoinPoint' and 'placePendingJoins' assert this.
  * The stack check needs no changes: 'mkProtoBCO' sums the stack use of all
    instructions of the BCO, which includes the pushes of the RHS and of every
    jump.
  * A jump has the same sequel as the definition (asserted): it is in tail
    position of the body, and only the scrutinee of a case changes the sequel.

We give each BCO a 'BcoId' ('withNewBco') when we start generating its code:
in 'schemeR_wrk' (functions, thunks, top-level bindings) and for the
alternatives of a case that pushes a continuation frame ('doCase'); the
scrutinee of such a case and inlined alternatives belong to the current BCO.
A 'JoinTarget' records the BCO of its label. The 'JoinEnv' stays visible in
the code of nested BCOs, so that a jump that the analysis wrongly let through
panics in 'schemeJump' instead of silently compiling to a PUSH_G of a local
variable.

Labels in continuations. If all jumps are emitted into one continuation BCO
K nested in the BCO of the definition, the label and the RHS go into K
instead, after its alternatives. The path to K is the list of binders of the
cases whose alternatives lead to it ('ContPath'). K is the innermost one, and
'schemeJoinPointInCont' records a 'PendingJoin' for its binder: the
definition's environment, depth d and sequel. When 'doCase' generates K's code
it takes the pending join points of its binder ('placePendingJoins'), puts
them into the 'JoinEnv' with fresh labels, compiles the alternatives, and
appends the labels and RHSs, innermost join point first, compiled exactly as
above, at depth d and with the
definition's environment. This is sound because a continuation BCO sees the
stack of the BCO that pushed its frame: when K is entered, it drops the frame
headers, which leaves the stack as it was at the case (at depth d_case >= d),
plus the case binder. So the stack up to d is that at the definition, and a
jump can slide its arguments down to d as in the defining BCO. No instruction
of K relies on K's frame after its entry: the stack checks happen at BCO entry
and returns, and a CCALL pushes a frame of its own. The free variables of the
RHS must be at the same offsets in the environment of the case (asserted);
the pointer bitmap of K's frame covers them while the scrutinee runs. Until
its label is placed, a join point is marked pending in the 'JoinEnv', so a jump
from elsewhere panics; so does a pending join point for an inlined case, or
one whose label was never placed or was placed twice.

Eligibility. The verdicts ('joinPointVerdicts') are computed for the whole
module before code generation, and counted in the "join points" section of
-ddump-bcos. A binding @let-no-escape j = \params -> rhs in body@ is compiled
as a label if

  * -fbc-join-points-as-labels is on (the default);
  * breakpoints are disabled. The optimisation is simply switched off with
    -fbreak-points: breakpoint wrappers can bury jumps in closures (see
    Note [Join points and bytecode preparation] in GHC.Stg.BcPrep), a
    breakpoint at the start of the join point's RHS would need a BRK_FUN in
    the middle of a BCO, and in practice the simplifier creates hardly any
    join points when breakpoints are enabled;
  * the RHS is a closure, not a constructor application;
  * it is not recursive, neither a 'StgRec' group nor mentioning j in rhs: a
    backward jump would be a loop without a safepoint;
  * every occurrence of j in body is a saturated tail call, and they are all
    emitted into the same BCO: the current one, or one continuation BCO nested
    in it ('joinJumpSites'). Code goes into another BCO in the RHS of a let,
    and in the alternatives of a case whose continuation is not inlined. Whether a case is inlined must be decided by the same function
    as in 'doCase', 'inlinedCaseScrutinee', or the analysis and the code
    generator could disagree. Occurrences of j anywhere else, e.g. in a closure
    or in a breakpoint's free variables, make the binding ineligible, even
    though STG's invariants should rule them out. The RHS of a join point
    nested in body goes where its label goes, so the verdicts of nested join
    points are decided first.
-}

{-
Note [Join points as loops]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
A self-recursive join point is a singleton 'StgRec' group, and is compiled as
a closure: every jump, including the recursive one, is a call. It could
instead be a label like any other, with the recursive jump as a backward JMP
-- a loop in the BCO that holds the label.

'schemeJoinPointLoop' emits one: the fallback closure, the body with the join
point bound to a label, the label, the RHS, and the slow path. The closure is
built as an ordinary recursive let builds it, so the static BCO count and the
allocation are what they were; what a loop buys is the instructions on its back
edges.

Eligibility is two predicates, not one, because 'joinJumpSites' reports paths
relative to the expression it is given:

  * the occurrences in the body must all be jumps in one BCO, which fixes
    where the label goes ('singleBco'); and
  * every occurrence in the RHS must be a jump in the BCO of the *label*,
    i.e. at the empty path, since the RHS is emitted where the label is.

Concatenating the two lists would be wrong whenever the label is not in the
BCO of the definition. The concatenation is what the older rule asked, and it
survives as the 'OtherwiseEligible' sub-count of 'rejected-recursive': for
legal STG that sub-count is now 0 by construction, since a binding it would
be true of takes the loop branch instead. The counter is kept, under its old
name, as the assertion that this really is so.

Only a singleton 'StgRec' is considered. A mutually recursive group stays
'JoinRejectRecGroup': its members would need one label each, and a jump from
one RHS to another is not covered by the two predicates above.

Only a label in the BCO of the definition is emitted. A loop whose body jumps
all sit in one *continuation* BCO would be a label there, like 'JoinInCont',
and the machinery for that ('PendingJoin', 'placePendingJoins') would carry it
-- but no such join point occurs in the corpus we measured or in any test we
could write, so emitting it would be untested code. It is rejected as
'JoinRejectLoopInCont' and counted, which keeps 'as loops' equal to the number
of loops actually emitted. This is a coverage limit waiting for a test, not a
design limit.

A nullary loop is a program that diverges, and is left alone by none of this:
its fallback is a thunk, so the first safepoint enters a closure under
evaluation and the program gets <<loop>>, which is what the closure
compilation gives today at the first jump instead.

Beyond that, a loop needs a slow path for the back edge, because intra-BCO
code has no safepoint: a BCO checks the heap, the stack and the
context-switch flag when it is entered, so a loop that never leaves the BCO
would never yield. 'YIELD_CHECK' is that check, emitted between the SLIDE and
the JMP of a backward jump, and it branches to a slow path that calls the join
point in the ordinary way, reaching the checks of 'run_BCO_fun'. The join
point is compiled a second time for that purpose, as the closure it would have
been. Compiling the RHS twice is what the remaining restriction is about:

  * a join point defined in the RHS that is itself placed (a label, or a
    nested loop) would be placed twice, once per copy, under one Id. Such a
    binding is rejected here ('JoinRejectLoopPlacedJoin'), and counted, so
    that the cost of lifting the restriction can be compared against what it
    would buy. A loop nested in a loop's RHS is rejected by this same rule,
    so nested loops are a known coverage limit, priced by that counter.

A jump from the RHS to a label defined outside the loop has the same problem:
the second copy of the RHS is a different BCO, and a label-compiled join point
exists only as a label, so the copy could not reach it. That conflict is
resolved by construction, and the resolution is "joins lose to loops": as long
as the join point's fallback is a closure, an enclosing join point with an
occurrence inside a loop's RHS sees that RHS as a closure ('ClosureLoopJoin')
and is rejected by the existing cross-BCO rule, so it never becomes a label
and there is nothing for the copy to jump to. This holds for emission as well,
not only for the analysis here, which is why there is no counter for the
class: it is empty.

That last sentence is an obligation on the emission step, not something the
analysis enforces: it holds only as long as 'joinJumpSites' reports an
occurrence in a loop's RHS as 'SiteInClosure'. The guard that decides this is
the @StgNonRec j'@ pattern in its 'StgLetNoEscape' case, which looks through
the RHS of a placed join point only for a non-recursive binding; a loop is an
'StgRec', so it falls to the 'otherwise' branch and its RHS is a closure.
Whoever lets a loop's RHS be looked through must revisit this paragraph.

The fallback is a loop as well ('schemeEntryLoop'), not the closure it would
have been. If it were not, the first safepoint that fires would move the loop
into the fallback for good -- the fallback's own recursive call re-enters the
fallback -- and every iteration after the first collection would pay a closure
entry again, which for a loop that allocates is nearly all of them. Its slow
path needs nothing new: the join point is a free variable of its own RHS, as
it has to be for the closure compilation to work at all, so an ordinary call
of it re-enters this very closure.

Its entry frame, however, cannot be the base of the label. A BCO is entered
with its arguments on the stack and the free variables above them, being the
stored arguments of the PAP, and a jump slides its arguments down to the base
-- which would bury the free variables, one of which is the join point the
slow path calls. So the prologue pushes a copy of the free variables and then
of the parameters and slides the entry frame out from under them, leaving the
free variables below the base and the parameters above it, at the cost of one
push per word at entry and nothing per iteration.

Leaving the entry frame in place under a copy would be cheaper still, but it
retains the arguments the fallback was entered with: they stay live in the
frame, and any callee of the loop body can collect, so everything they reach
survives that collection -- for a loop consuming a lazy structure, up to a
nursery's worth of cells it has already passed. Sliding them away costs
nothing per iteration and avoids that, so it is what we do.

The class of join points rejected because their jumps sit in a loop's RHS
stops being empty as soon as the fallback stops being a closure -- if the
RHS and the join points placed in it are duplicated into the fallback
(v2a-2), or if the fallback becomes a bare BCO entered directly. Whoever makes
that change must add the counter and the rejection then. Until then the price
of the resolution is measured from the other side by 'in-loop-join': the join
points whose jumps sit in a loop's RHS, which emitting loops would newly
reach.

'in-loop-join-placeable' is the part of that price a fallback-free loop would
actually win: the join points whose verdict is 'in-loop-join' and which a
counterfactual run of 'joinPointVerdicts' -- the same function with
'LoopHasNoFallback', where 'joinJumpSites' looks through a loop's RHS exactly
as it looks through the RHS of any other placed join point -- makes a placed
one. That map is computed inside 'joinPointStats', so it exists only when
-ddump-bcos forces the dump, and it goes nowhere else: it decides no code, in
either flag state.

Three caveats, since the number invites being read as more than it is:

  * The counterfactual is self-consistent, not a question asked of one join
    point at a time. With loops looked through, a join point that becomes a
    label is itself looked through, so one loop can win several join points
    and the effect can cascade to verdicts elsewhere. The number is "what the
    analysis would say", not a re-partition of today's 'in-loop-join'.
  * It assumes the loop stays a loop and only the duplicate RHS goes away.
    That is the intent of v2b, not a statement about any implementation of it:
    a change that removes the fallback by making the loop something else is
    not what was measured.
  * It says what removing the fallback would make eligible, and nothing about
    what removing it would cost.
-}

{-
Note [Inlined case continuations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Usually a case expression pushes a case continuation frame (PUSH_ALTS) whose
BCO holds the alternatives, and compiles the scrutinee in tail position; the
scrutinee returns into that frame. See Note [Case continuation BCOs].

If the scrutinee cannot leave the current BCO we avoid the frame and the extra
BCO: 'schemeIntoStack' compiles the scrutinee so that it leaves its value on
top of the stack at depth d, and the alternatives follow directly in the
parent BCO. The alternatives are compiled exactly as before, at depth
d_bndr = d + bndr_size with the outer sequel, because after the continuation
BCO's two header SLIDEs its stack is also just [value][parent stack at d].

"Cannot leave the BCO" means the scrutinee's code does not enter or call
anything, return, yield, or perform a heap or stack check. This holds for:

  * literals and unlifted variables (PUSH_*),
  * primops the interpreter implements inline ('doPrimOpCode'),
  * ordinary saturated constructor applications (PACK),
  * non-breakpoint ticks around such expressions (HPC_TICK or nothing),
  * a non-recursive let (or let-no-escape) of a constructor around such an
    expression; the constructor is slid away from under the value afterwards.

Whether a primop is inline must be decided by calling 'doPrimOpCode', not by
matching on the primop: e.g. 64-bit primops are only inline on 64-bit
platforms.

Everything else keeps the frame:

  * applications and lifted variables: they ENTER a closure, which may
    evaluate arbitrary code, GC or context switch.
  * tagToEnum#: compiled as a SLIDE and ENTER ('implement_tagToId').
  * primops without an inline implementation: compiled as a tail call to the
    primop wrapper.
  * foreign calls and prim calls: they call out of the interpreter and can
    return to the scheduler.
  * nested cases: the alternatives of the inner case are compiled in tail
    position against the outer sequel, so the inner case has no single point
    where its value is left on the stack. (The inner case's own scrutinee may
    of course be inlined into its alternatives' BCO.)
  * scrutinees needing an unboxed tuple frame: the value spans several
    components and tuple returns go through the tuple_bco; Unarise eliminates
    most of these cases anyway.
  * general lets: every binder allocates a closure whose code is a separate
    BCO anyway, and between ALLOC_* and MK* the closures are uninitialised.

Why this is safe without a frame:

  * GC: the continuation frame's bitmap describes the parent's stack while the
    scrutinee runs. Since the scrutinee cannot GC, nothing needs describing.
    The alternatives' own safepoints are unaffected, as they use the outer
    sequel just like before.
  * Heap checks: allocation by the inlined code is still bounded per BCO
    entry, because each instruction runs at most once per entry.
  * Stack checks: 'mkProtoBCO' sums 'bciStackUse' over all instructions.
    Each instruction's stack growth is bounded by its 'bciStackUse' and jumps
    only go forward, so the sum bounds the peak; PUSH_ALTS counted its BCO's
    stack use as well, so the parent's STKCHECK stays sufficient.
  * Profiling: the dropped stg_restore_cccs frame only matters if the
    scrutinee could change the CCCS, which it cannot.
  * Breakpoints: the step-out BRK_FUN of a continuation BCO is only reachable
    by returning into its frame, which can't happen while a non-leaving
    scrutinee runs. It can't be kept either: BRK_FUN may only be the first
    instruction of a BCO, as the interpreter only resumes a BCO at pc 0.
    See Note [Debugger: Stepout internal break locs].

-fno-bc-inline-case-conts disables this.
-}

{-
Note [Debugger: Stepout internal break locs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Step-out tells the interpreter to run until the current function
returns to where it was called from, and stop there.

This is achieved by enabling the BRK_FUN found on the first RET_BCO
frame on the stack (See [Note Debugger: Step-out]).

Case continuation BCOs (which select an alternative branch) must
therefore be headed by a BRK_FUN. An example:

    f x = case g x of <--- end up here
        1 -> ...
        2 -> ...

    g y = ... <--- step out from here

- `g` will return a value to the case continuation BCO in `f`
- The case continuation BCO will receive the value returned from g
- Match on it and push the alternative continuation for that branch
- And then enter that alternative.

If we step-out of `g`, the first RET_BCO on the stack is the case
continuation of `f` -- execution should stop at its start, before
selecting an alternative. (One might ask, "why not enable the breakpoint
in the alternative instead?", because the alternative continuation is
only pushed to the stack *after* it is selected by the case cont. BCO)

However, the case cont. BCO is not associated with any source-level
tick, it is merely the glue code which selects alternatives which do
have source level ticks. Therefore, we have to come up at code
generation time with a breakpoint location ('InternalBreakLoc') to
display to the user when it is stopped there.

Our solution is to use the last tick seen just before reaching the case
continuation. This is robust because a case continuation will thus
always have a relevant breakpoint location:

    - The source location will be the last source-relevant expression
      executed before the continuation is pushed

    - So the source location will point to the thing you've just stepped
      out of

    - The variables available are the same as the ones bound just before entering

    - Doing :step-local from there will put you on the selected
      alternative (which at the source level may also be the e.g. next
      line in a do-block)

Examples, using angle brackets (<<...>>) to denote the breakpoint span:

    f x = case <<g x>> {- step in here -} of
        1 -> ...
        2 -> ...>

    g y = <<...>> <--- step out from here

    ...

    f x = <<case g x of <--- end up here, whole case highlighted
        1 -> ...
        2 -> ...>>

    doing :step-local ...

    f x = case g x of
        1 -> <<...>> <--- stop in the alternative
        2 -> ...

A second example based on T26042d2, where the source is a do-block IO
action, optimised to a chain of `case expressions`.

    main = do
      putStrLn "hello1"
      <<f>> <--- step-in here
      putStrLn "hello3"
      putStrLn "hello4"

    f = do
      <<putStrLn "hello2.1">> <--- step-out from here
      putStrLn "hello2.2"

    ...

    main = do
      putStrLn "hello1"
      <<f>> <--- end up here again, the previously executed expression
      putStrLn "hello3"
      putStrLn "hello4"

    doing step/step-local ...

    main = do
      putStrLn "hello1"
      f
      <<putStrLn "hello3">> <--- straight to the next line
      putStrLn "hello4"
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

  - bit 24+:   Unsigned value indicating the stack offset
               of the continuation in words. For tuple returns
               this is the number of words returned on the
               stack. For primcalls this field is unused, since
               we don't jump to a continuation.

If a register is smaller than a word on the stack (for example a
single precision float on a 64 bit system), then the stack slot
is padded to a whole word.

  Example:

    If a tuple is returned in three registers and an additional two
    words on the stack, then three bits in the register bitmap
    (bits 0-23) would be set. And the stack offset (bits 24+) would
    encode the value two.

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
    which is found by inspecting the value of bits 24+. In this
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
  instruction is executed, followed by stg_ctoi_t_info. It also saves
  the old ctoi_tuple_spill_words value from the TSO in the frame and sets
  the TSO field to the number of stack words used by the tuple in the
  GHC native calling convention. This spill count is derived from
  call_info.

  For example if we expect a tuple with three words on the stack, the stack
  looks as follows after PUSH_ALTS_TUPLE:

      ...
      next_frame
      cont_free_var_1
      cont_free_var_2
      ...
      cont_free_var_n
      old_spill
      call_info
      tuple_BCO
      cont_BCO
      stg_ctoi_t_info  <- Sp

  If the tuple is returned by object code, stg_ctoi_t will deal with
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

tupleBCO :: Platform -> NativeCallInfo -> [(PrimRep, ByteOff)] -> ProtoBCO
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

primCallBCO :: Platform -> NativeCallInfo -> [(PrimRep, ByteOff)] -> ProtoBCO
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
    -> Type
    -> [StgArg]              -- args (atoms)
    -> BcM BCInstrList
generatePrimCall d s p target _result_ty args
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

         push_target = PUSH_UBX (LitLabel (mkFastStringShortText target) IsFunction) 1
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
    -> CCallSpec              -- where to call
    -> Type
    -> [StgArg]              -- args (atoms)
    -> BcM BCInstrList
generateCCall d0 s p (CCallSpec target PrimCallConv _) result_ty args
 | (StaticTarget _ label _) <- target
 = generatePrimCall d0 s p label result_ty args
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
                 DynamicTarget{} -> Nothing
                 StaticTarget _ _ ForeignValue ->
                   panic "generateCCall: unexpected FFI value import"
                 StaticTarget _ target ForeignFunction ->
                   Just (LitLabel (mkFastStringShortText target) IsFunction)

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
        DoubleRep   -> mkLitDouble 0
        FloatRep    -> mkLitFloat  0
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
           , isBoxedDataTyCon tyc
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
          Just con
            -- See Note [LFInfo of DataCon workers and wrappers] in GHC.Types.Id.Make.
            | isNullaryRepDataCon con ->
              return (unitOL (PACK con 0), szb)

          _
            -- see Note [Generating code for top-level string literal bindings]
            | idType var `eqType` addrPrimTy ->
              return (unitOL (PUSH_ADDR (getName var)), szb)

            | otherwise -> do
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
        LitFloating LitFloat  _ -> code FloatRep
        LitFloating LitDouble _ -> code DoubleRep
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
lookupBCEnv_maybe v env = UniqMap.lookupUniqMap env v

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

-- | Read only environment for generating ByteCode
data BcM_Env
   = BcM_Env
        { bcm_hsc_env    :: !HscEnv
        , bcm_module     :: !Module -- current module (for breakpoints)
        , modBreaks      :: !(Maybe ModBreaks)
        , last_bp_tick   :: !(Maybe StgTickish)
        , join_verdicts  :: !JoinPointVerdicts
        , join_env       :: !JoinEnv -- ^ join points compiled as labels in scope
        , pending_joins  :: !PendingJoins -- ^ labels to place in continuation BCOs
        , current_bco    :: !BcoId   -- ^ the BCO whose code is being generated
        }

data BcM_State
   = BcM_State
        { nextlabel      :: !Word32 -- ^ For generating local labels
        , breakInfoIdx   :: !Int    -- ^ Next index for breakInfo array
        , breakInfo      :: !(IntMap CgBreakInfo)
          -- ^ Info at breakpoints occurrences. Indexed with
          -- 'InternalBreakpointId'. See Note [Breakpoint identifiers] in
          -- GHC.ByteCode.Breakpoints.
        , nextBcoId      :: !Word   -- ^ For generating 'BcoId's
        , placedJoins    :: !IdSet  -- ^ pending join points whose labels are placed
        }

newtype BcM r = BcM (BcM_Env -> BcM_State -> IO (r, BcM_State))
  deriving (Functor, Applicative, Monad, MonadIO)
    via (ReaderT BcM_Env (StateT BcM_State IO))

runBc :: HscEnv -> Module -> Maybe ModBreaks -> JoinPointVerdicts -> BcM r
      -> IO (r, BcM_State)
runBc hsc_env this_mod mbs verdicts (BcM m)
   = m (BcM_Env hsc_env this_mod mbs Nothing verdicts UniqMap.emptyUniqMap
                UniqMap.emptyUniqMap (BcoId 0))
       (BcM_State 0 0 IntMap.empty 1 emptyVarSet)

instance HasDynFlags BcM where
    getDynFlags = hsc_dflags <$> getHscEnv

instance HasHscEnv BcM where
    getHscEnv = BcM $ \env st -> return (bcm_hsc_env env, st)

getProfile :: BcM Profile
getProfile = targetProfile <$> getDynFlags

shouldAddBcoName :: BcM (Maybe Module)
shouldAddBcoName = do
  add <- gopt Opt_AddBcoName <$> getDynFlags
  if add
    then Just <$> getCurrentModule
    else return Nothing

getLabelBc :: BcM LocalLabel
getLabelBc = BcM $ \_ st ->
  do let nl = nextlabel st
     when (nl == maxBound) $
         panic "getLabelBc: Ran out of labels"
     return (LocalLabel nl, st{nextlabel = nl + 1})

getLabelsBc :: Word32 -> BcM [LocalLabel]
getLabelsBc n = BcM $ \_ st ->
  let ctr = nextlabel st
   in return (coerce [ctr .. ctr+n-1], st{nextlabel = ctr+n})

newBreakInfo :: CgBreakInfo -> BcM (Maybe InternalBreakpointId)
newBreakInfo info = BcM $ \env st -> do
  -- if we're not generating ModBreaks for this module for some reason, we
  -- can't store breakpoint occurrence information.
  case modBreaks env of
    Nothing -> pure (Nothing, st)
    Just modBreaks -> do
      let ix = breakInfoIdx st
          st' = st
            { breakInfo = IntMap.insert ix info (breakInfo st)
            , breakInfoIdx = ix + 1
            }
      return (Just $ InternalBreakpointId (modBreaks_module modBreaks) ix, st')

getCurrentModule :: BcM Module
getCurrentModule = BcM $ \env st -> return (bcm_module env, st)

withBreakTick :: StgTickish -> BcM a -> BcM a
withBreakTick bp (BcM act) = BcM $ \env st ->
  act env{last_bp_tick=Just bp} st

getLastBreakTick :: BcM (Maybe StgTickish)
getLastBreakTick = BcM $ \env st ->
  pure (last_bp_tick env, st)

-- | Generate the code of a new BCO. See Note [Join points as labels].
withNewBco :: BcM a -> BcM a
withNewBco (BcM act) = BcM $ \env st ->
  let n = nextBcoId st
  in act env{current_bco = BcoId n} st{nextBcoId = n + 1}

getCurrentBco :: BcM BcoId
getCurrentBco = BcM $ \env st -> pure (current_bco env, st)

-- | Is this let-no-escape binder compiled as a label, and where?
joinPlacement :: Id -> BcM (Maybe ContPath)
joinPlacement j = BcM $ \env st ->
  pure (verdictPlacement =<< UniqMap.lookupUniqMap (join_verdicts env) j, st)

withJoinPoint :: Id -> JoinTarget -> BcM a -> BcM a
withJoinPoint j target (BcM act) = BcM $ \env st ->
  act env{join_env = UniqMap.addToUniqMap (join_env env) j (JoinLabel target)} st

lookupJoinBinding :: Id -> BcM (Maybe JoinBinding)
lookupJoinBinding f = BcM $ \env st ->
  pure (UniqMap.lookupUniqMap (join_env env) f, st)

-- | Record a join point whose label goes into the continuation BCO of the case
-- with the given binder.
withPendingJoin :: Id -> PendingJoin -> BcM a -> BcM a
withPendingJoin k pending (BcM act) = BcM $ \env st ->
  -- Innermost first: the RHS of a join point can jump to one defined further
  -- out, so the outer label must follow the inner RHS to keep jumps forward.
  let others = fromMaybe [] (UniqMap.lookupUniqMap (pending_joins env) k)
  in act env{ join_env = UniqMap.addToUniqMap (join_env env) (pj_id pending) JoinLabelPending
            , pending_joins =
                UniqMap.addToUniqMap (pending_joins env) k (pending : others) } st

-- | The pending join points for the case with the given binder, innermost
-- first. Panics if one of them is already placed. The entry stays in the
-- environment, which is harmless: 'doCase' visits every case binder once, and
-- placing a label twice panics.
takePendingJoins :: Id -> BcM [PendingJoin]
takePendingJoins k = BcM $ \env st -> do
  let pendings = fromMaybe [] (UniqMap.lookupUniqMap (pending_joins env) k)
      placed = [ pj_id pj | pj <- pendings, pj_id pj `elemVarSet` placedJoins st ]
  unless (null placed) $
    pprPanic "takePendingJoins: labels placed twice" (ppr k <+> ppr placed)
  pure (pendings, st)

markJoinPlaced :: Id -> BcM ()
markJoinPlaced j = BcM $ \_ st ->
  pure ((), st{placedJoins = extendVarSet (placedJoins st) j})

isJoinPlaced :: Id -> BcM Bool
isJoinPlaced j = BcM $ \_ st -> pure (j `elemVarSet` placedJoins st, st)

tickFS :: FastString
tickFS = fsLit "ticked"

mkHpcTickBoxesLabell :: Platform -> Module -> FastString
mkHpcTickBoxesLabell platform mod =
  fsLit (Coverage.mkHpcTickBoxesLabell platform mod)

-- Dehydrating CgBreakInfo

dehydrateCgBreakInfo :: [TyVar] -> [Maybe (Id, Word)] -> Type -> Either InternalBreakLoc BreakpointId -> CgBreakInfo
dehydrateCgBreakInfo ty_vars idOffSets tick_ty bid =
          CgBreakInfo
            { cgb_tyvars = map toIfaceTvBndr ty_vars
            , cgb_vars = map (fmap (\(i, offset) -> (toIfaceIdBndr i, offset))) idOffSets
            , cgb_resty = toIfaceType tick_ty
            , cgb_tick_id = bid
            }
