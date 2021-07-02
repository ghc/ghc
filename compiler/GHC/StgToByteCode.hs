{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

{-# OPTIONS_GHC -fprof-auto-top #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

--
--  (c) The University of Glasgow 2002-2006
--

-- | GHC.StgToByteCode: Generate bytecode from STG
module GHC.StgToByteCode ( UnlinkedBCO, byteCodeGen, stgExprToBCOs ) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Driver.Session
import GHC.Driver.Env

import GHC.ByteCode.Instr
import GHC.ByteCode.Asm
import GHC.ByteCode.Types

import GHC.Cmm.CallConv
import GHC.Cmm.Expr
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
import GHC.Types.Id.Make
import GHC.Types.Id
import GHC.Types.ForeignCall
import GHC.Core
import GHC.Types.Literal
import GHC.Builtin.PrimOps
import GHC.Core.Type
import GHC.Types.RepType
import GHC.Core.DataCon
import GHC.Core.TyCon
import GHC.Utils.Misc
import GHC.Utils.Logger
import GHC.Types.Var.Set
import GHC.Builtin.Types ( unboxedUnitTy )
import GHC.Builtin.Types.Prim
import GHC.Core.TyCo.Ppr ( pprType )
import GHC.Utils.Error
import GHC.Types.Unique
import GHC.Builtin.Uniques
import GHC.Builtin.Utils ( primOpId )
import GHC.Data.FastString
import GHC.Utils.Panic
import GHC.StgToCmm.Closure ( NonVoid(..), fromNonVoid, nonVoidIds )
import GHC.StgToCmm.Layout
import GHC.Runtime.Heap.Layout hiding (WordOff, ByteOff, wordsToBytes)
import GHC.Data.Bitmap
import GHC.Data.OrdList
import GHC.Data.Maybe
import GHC.Types.Var.Env
import GHC.Types.Tickish

import Data.List ( genericReplicate, genericLength, intersperse
                 , partition, scanl', sort, sortBy, zip4, zip6, nub )
import Foreign
import Control.Monad
import Data.Char

import GHC.Types.Unique.Supply
import GHC.Unit.Module

import Control.Exception
import Data.Array
import Data.Coerce (coerce)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.IntMap (IntMap)
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified GHC.Data.FiniteMap as Map
import Data.Ord
import GHC.Stack.CCS
import Data.Either ( partitionEithers )

import qualified GHC.Types.CostCentre as CC
import GHC.Stg.Syntax
import GHC.Stg.FVs

-- -----------------------------------------------------------------------------
-- Generating byte code for a complete module

byteCodeGen :: HscEnv
            -> Module
            -> [StgTopBinding]
            -> [TyCon]
            -> Maybe ModBreaks
            -> IO CompiledByteCode
byteCodeGen hsc_env this_mod binds tycs mb_modBreaks
   = withTiming logger dflags
                (text "GHC.StgToByteCode"<+>brackets (ppr this_mod))
                (const ()) $ do
        -- Split top-level binds into strings and others.
        -- See Note [generating code for top-level string literal bindings].
        let (strings, lifted_binds) = partitionEithers $ do  -- list monad
                bnd <- binds
                case bnd of
                  StgTopLifted bnd      -> [Right bnd]
                  StgTopStringLit b str -> [Left (b, str)]
            flattenBind (StgNonRec b e) = [(b,e)]
            flattenBind (StgRec bs)     = bs
        stringPtrs <- allocateTopStrings interp strings

        us <- mkSplitUniqSupply 'y'
        (BcM_State{..}, proto_bcos) <-
           runBc hsc_env us this_mod mb_modBreaks (mkVarEnv stringPtrs) $ do
             prepd_binds <- mapM bcPrepBind lifted_binds
             let flattened_binds =
                   concatMap (flattenBind . annBindingFreeVars) (reverse prepd_binds)
             mapM schemeTopBind flattened_binds

        when (notNull ffis)
             (panic "GHC.StgToByteCode.byteCodeGen: missing final emitBc?")

        dumpIfSet_dyn logger dflags Opt_D_dump_BCOs
           "Proto-BCOs" FormatByteCode
           (vcat (intersperse (char ' ') (map ppr proto_bcos)))

        cbc <- assembleBCOs interp profile proto_bcos tycs (map snd stringPtrs)
          (case modBreaks of
             Nothing -> Nothing
             Just mb -> Just mb{ modBreaks_breakInfo = breakInfo })

        -- Squash space leaks in the CompiledByteCode.  This is really
        -- important, because when loading a set of modules into GHCi
        -- we don't touch the CompiledByteCode until the end when we
        -- do linking.  Forcing out the thunks here reduces space
        -- usage by more than 50% when loading a large number of
        -- modules.
        evaluate (seqCompiledByteCode cbc)

        return cbc

  where dflags = hsc_dflags hsc_env
        logger = hsc_logger hsc_env
        interp  = hscInterp hsc_env
        profile = targetProfile dflags

allocateTopStrings
  :: Interp
  -> [(Id, ByteString)]
  -> IO [(Var, RemotePtr ())]
allocateTopStrings interp topStrings = do
  let !(bndrs, strings) = unzip topStrings
  ptrs <- interpCmd interp $ MallocStrings strings
  return $ zip bndrs ptrs

{-
Note [generating code for top-level string literal bindings]

Here is a summary on how the byte code generator deals with top-level string
literals:

1. Top-level string literal bindings are separated from the rest of the module.

2. The strings are allocated via interpCmd, in allocateTopStrings

3. The mapping from binders to allocated strings (topStrings) are maintained in
   BcM and used when generating code for variable references.
-}

-- -----------------------------------------------------------------------------
-- Generating byte code for an expression

-- Returns: the root BCO for this expression
stgExprToBCOs :: HscEnv
              -> Module
              -> Type
              -> StgRhs
              -> IO UnlinkedBCO
stgExprToBCOs hsc_env this_mod expr_ty expr
 = withTiming logger dflags
              (text "GHC.StgToByteCode"<+>brackets (ppr this_mod))
              (const ()) $ do

      -- the uniques are needed to generate fresh variables when we introduce new
      -- let bindings for ticked expressions
      us <- mkSplitUniqSupply 'y'
      (BcM_State _dflags _us _this_mod _final_ctr mallocd _ _ _, proto_bco)
         <- runBc hsc_env us this_mod Nothing emptyVarEnv $ do
              prepd_expr <- annBindingFreeVars <$>
                                       bcPrepBind (StgNonRec dummy_id expr)
              case prepd_expr of
                (StgNonRec _ cg_expr) -> schemeR [] (idName dummy_id, cg_expr)
                _                     ->
                  panic "GHC.StgByteCode.stgExprToBCOs"

      when (notNull mallocd)
           (panic "GHC.StgToByteCode.stgExprToBCOs: missing final emitBc?")

      dumpIfSet_dyn logger dflags Opt_D_dump_BCOs "Proto-BCOs" FormatByteCode
         (ppr proto_bco)

      assembleOneBCO interp profile proto_bco
  where dflags = hsc_dflags hsc_env
        logger = hsc_logger hsc_env
        profile = targetProfile dflags
        interp  = hscInterp hsc_env
        -- we need an otherwise unused Id for bytecode generation
        dummy_id = mkSysLocal (fsLit "BCO_toplevel")
                              (mkPseudoUniqueE 0)
                              Many
                              expr_ty
{-
  Prepare the STG for bytecode generation:

   - Ensure that all breakpoints are directly under
        a let-binding, introducing a new binding for
        those that aren't already.

   - Protect Not-necessarily lifted join points, see
        Note [Not-necessarily-lifted join points]

 -}

bcPrepRHS :: StgRhs -> BcM StgRhs
-- explicitly match all constructors so we get a warning if we miss any
bcPrepRHS (StgRhsClosure fvs cc upd args (StgTick bp@Breakpoint{} expr)) = do
  {- If we have a breakpoint directly under an StgRhsClosure we don't
     need to introduce a new binding for it.
   -}
  expr' <- bcPrepExpr expr
  pure (StgRhsClosure fvs cc upd args (StgTick bp expr'))
bcPrepRHS (StgRhsClosure fvs cc upd args expr) =
  StgRhsClosure fvs cc upd args <$> bcPrepExpr expr
bcPrepRHS con@StgRhsCon{} = pure con

bcPrepExpr :: StgExpr -> BcM StgExpr
-- explicitly match all constructors so we get a warning if we miss any
bcPrepExpr (StgTick bp@(Breakpoint tick_ty _ _) rhs)
  | isLiftedTypeKind (typeKind tick_ty) = do
      id <- newId tick_ty
      rhs' <- bcPrepExpr rhs
      let expr' = StgTick bp rhs'
          bnd = StgNonRec id (StgRhsClosure noExtFieldSilent
                                            CC.dontCareCCS
                                            ReEntrant
                                            []
                                            expr'
                             )
          letExp = StgLet noExtFieldSilent bnd (StgApp id [])
      pure letExp
  | otherwise = do
      id <- newId (mkVisFunTyMany realWorldStatePrimTy tick_ty)
      st <- newId realWorldStatePrimTy
      rhs' <- bcPrepExpr rhs
      let expr' = StgTick bp rhs'
          bnd = StgNonRec id (StgRhsClosure noExtFieldSilent
                                            CC.dontCareCCS
                                            ReEntrant
                                            [voidArgId]
                                            expr'
                             )
      pure $ StgLet noExtFieldSilent bnd (StgApp id [StgVarArg st])
bcPrepExpr (StgTick tick rhs) =
  StgTick tick <$> bcPrepExpr rhs
bcPrepExpr (StgLet xlet bnds expr) =
  StgLet xlet <$> bcPrepBind bnds
              <*> bcPrepExpr expr
bcPrepExpr (StgLetNoEscape xlne bnds expr) =
  StgLet xlne <$> bcPrepBind bnds
              <*> bcPrepExpr expr
bcPrepExpr (StgCase expr bndr alt_type alts) =
  StgCase <$> bcPrepExpr expr
          <*> pure bndr
          <*> pure alt_type
          <*> mapM bcPrepAlt alts
bcPrepExpr lit@StgLit{} = pure lit
-- See Note [Not-necessarily-lifted join points], step 3.
bcPrepExpr (StgApp x [])
  | isNNLJoinPoint x = pure $
      StgApp (protectNNLJoinPointId x) [StgVarArg voidPrimId]
bcPrepExpr app@StgApp{} = pure app
bcPrepExpr app@StgConApp{} = pure app
bcPrepExpr app@StgOpApp{} = pure app

bcPrepAlt :: StgAlt -> BcM StgAlt
bcPrepAlt (ac, bndrs, expr) = (,,) ac bndrs <$> bcPrepExpr expr

bcPrepBind :: StgBinding -> BcM StgBinding
-- explicitly match all constructors so we get a warning if we miss any
bcPrepBind (StgNonRec bndr rhs) =
  let (bndr', rhs') = bcPrepSingleBind (bndr, rhs)
  in  StgNonRec bndr' <$> bcPrepRHS rhs'
bcPrepBind (StgRec bnds) =
  StgRec <$> mapM ((\(b,r) -> (,) b <$> bcPrepRHS r) . bcPrepSingleBind)
                  bnds

bcPrepSingleBind :: (Id, StgRhs) -> (Id, StgRhs)
-- If necessary, modify this Id and body to protect not-necessarily-lifted join points.
-- See Note [Not-necessarily-lifted join points], step 2.
bcPrepSingleBind (x, StgRhsClosure ext cc upd_flag args body)
  | isNNLJoinPoint x
  = ( protectNNLJoinPointId x
    , StgRhsClosure ext cc upd_flag (args ++ [voidArgId]) body)
bcPrepSingleBind bnd = bnd

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
   :: Platform
   -> name
   -> BCInstrList
   -> Either  [CgStgAlt] (CgStgRhs)
        -- ^ original expression; for debugging only
   -> Int
   -> Word16
   -> [StgWord]
   -> Bool      -- True <=> is a return point, rather than a function
   -> [FFIInfo]
   -> ProtoBCO name
mkProtoBCO platform nm instrs_ordlist origin arity bitmap_size bitmap is_ret ffis
   = ProtoBCO {
        protoBCOName = nm,
        protoBCOInstrs = maybe_with_stack_check,
        protoBCOBitmap = bitmap,
        protoBCOBitmapSize = bitmap_size,
        protoBCOArity = arity,
        protoBCOExpr = origin,
        protoBCOFFIs = ffis
      }
     where
        -- Overestimate the stack usage (in words) of this BCO,
        -- and if >= iNTERP_STACK_CHECK_THRESH, add an explicit
        -- stack check.  (The interpreter always does a stack check
        -- for iNTERP_STACK_CHECK_THRESH words at the start of each
        -- BCO anyway, so we only need to add an explicit one in the
        -- (hopefully rare) cases when the (overestimated) stack use
        -- exceeds iNTERP_STACK_CHECK_THRESH.
        maybe_with_stack_check
           | is_ret && stack_usage < fromIntegral (pc_AP_STACK_SPLIM (platformConstants platform)) = peep_d
                -- don't do stack checks at return points,
                -- everything is aggregated up to the top BCO
                -- (which must be a function).
                -- That is, unless the stack usage is >= AP_STACK_SPLIM,
                -- see bug #1466.
           | stack_usage >= fromIntegral iNTERP_STACK_CHECK_THRESH
           = STKCHECK stack_usage : peep_d
           | otherwise
           = peep_d     -- the supposedly common case

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
  | otherwise = take (argRepSizeW platform rep) (repeat True) ++ argBits platform args

non_void :: [ArgRep] -> [ArgRep]
non_void = filter nv
  where nv V = False
        nv _ = True

-- -----------------------------------------------------------------------------
-- schemeTopBind

-- Compile code for the right-hand side of a top-level binding

schemeTopBind :: (Id, CgStgRhs) -> BcM (ProtoBCO Name)
schemeTopBind (id, rhs)
  | Just data_con <- isDataConWorkId_maybe id,
    isNullaryRepDataCon data_con = do
    platform <- profilePlatform <$> getProfile
        -- Special case for the worker of a nullary data con.
        -- It'll look like this:        Nil = /\a -> Nil a
        -- If we feed it into schemeR, we'll get
        --      Nil = Nil
        -- because mkConAppCode treats nullary constructor applications
        -- by just re-using the single top-level definition.  So
        -- for the worker itself, we must allocate it directly.
    -- ioToBc (putStrLn $ "top level BCO")
    emitBc (mkProtoBCO platform (getName id) (toOL [PACK data_con 0, ENTER])
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
collect (StgRhsClosure _ _ _ args body) = (args, body)
collect (StgRhsCon _cc dc cnum _ticks args) = ([], StgConApp dc cnum args [])

schemeR_wrk
    :: [Id]
    -> Name
    -> CgStgRhs            -- expression e, for debugging only
    -> ([Var], CgStgExpr)  -- result of collect on e
    -> BcM (ProtoBCO Name)
schemeR_wrk fvs nm original_body (args, body)
   = do
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
         p_init    = Map.fromList (zip all_args (mkStackOffsets 0 szsb_args))

         -- make the arg bitmap
         bits = argBits platform (reverse (map (bcIdArgRep platform) all_args))
         bitmap_size = genericLength bits
         bitmap = mkBitmap platform bits
     body_code <- schemeER_wrk sum_szsb_args p_init body

     emitBc (mkProtoBCO platform nm body_code (Right original_body)
                 arity bitmap_size bitmap False{-not alts-})

-- introduce break instructions for ticked expressions
schemeER_wrk :: StackDepth -> BCEnv -> CgStgExpr -> BcM BCInstrList
schemeER_wrk d p (StgTick (Breakpoint tick_ty tick_no fvs) rhs)
  = do  code <- schemeE d 0 p rhs
        cc_arr <- getCCArray
        this_mod <- moduleName <$> getCurrentModule
        platform <- profilePlatform <$> getProfile
        let idOffSets = getVarOffSets platform d p fvs
        let breakInfo = CgBreakInfo
                        { cgb_vars = idOffSets
                        , cgb_resty = tick_ty
                        }
        newBreakInfo tick_no breakInfo
        hsc_env <- getHscEnv
        let cc | Just interp <- hsc_interp hsc_env
               , interpreterProfiled interp
               = cc_arr ! tick_no
               | otherwise = toRemotePtr nullPtr
        let breakInstr = BRK_FUN (fromIntegral tick_no) (getUnique this_mod) cc
        return $ breakInstr `consOL` code
schemeER_wrk d p rhs = schemeE d 0 p rhs

getVarOffSets :: Platform -> StackDepth -> BCEnv -> [Id] -> [Maybe (Id, Word16)]
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
            let !var_depth_ws =
                    trunc16W $ bytesToWords platform (depth - offset) + 2
            in Just (id, var_depth_ws)

truncIntegral16 :: Integral a => a -> Word16
truncIntegral16 w
    | w > fromIntegral (maxBound :: Word16)
    = panic "stack depth overflow"
    | otherwise
    = fromIntegral w

trunc16B :: ByteOff -> Word16
trunc16B = truncIntegral16

trunc16W :: WordOff -> Word16
trunc16W = truncIntegral16

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
returnUnboxedAtom
    :: StackDepth
    -> Sequel
    -> BCEnv
    -> StgArg
    -> BcM BCInstrList
returnUnboxedAtom d s p e = do
    let reps = case e of
                 StgLitArg lit -> typePrimRepArgs (literalType lit)
                 StgVarArg i   -> bcIdPrimReps i
    (push, szb) <- pushAtom d p e
    ret <- returnUnboxedReps d s szb reps
    return (push `appOL` ret)

-- return an unboxed value from the top of the stack
returnUnboxedReps
    :: StackDepth
    -> Sequel
    -> ByteOff    -- size of the thing we're returning
    -> [PrimRep]  -- representations
    -> BcM BCInstrList
returnUnboxedReps d s szb reps = do
    profile <- getProfile
    let platform = profilePlatform profile
        non_void VoidRep = False
        non_void _ = True
    ret <- case filter non_void reps of
             -- use RETURN_UBX for unary representations
             []    -> return (unitOL $ RETURN_UBX V)
             [rep] -> return (unitOL $ RETURN_UBX (toArgRep platform rep))
             -- otherwise use RETURN_TUPLE with a tuple descriptor
             nv_reps -> do
               let (tuple_info, args_offsets) = layoutTuple profile 0 (primRepCmmType platform) nv_reps
                   args_ptrs = map (\(rep, off) -> (isFollowableArg (toArgRep platform rep), off)) args_offsets
               tuple_bco <- emitBc (tupleBCO platform tuple_info args_ptrs)
               return $ PUSH_UBX (mkTupleInfoLit platform tuple_info) 1 `consOL`
                        PUSH_BCO tuple_bco `consOL`
                        unitOL RETURN_TUPLE
    return ( mkSlideB platform szb (d - s) -- clear to sequel
             `appOL`  ret)                 -- go

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
        arg_ty e = primRepCmmType platform (atomPrimRep e)
        (tuple_info, tuple_components) = layoutTuple profile d arg_ty es
        go _   pushes [] = return (reverse pushes)
        go !dd pushes ((a, off):cs) = do (push, szb) <- pushAtom dd p a
                                         MASSERT(off == dd + szb)
                                         go (dd + szb) (push:pushes) cs
    pushes <- go d [] tuple_components
    ret <- returnUnboxedReps d
                             s
                             (wordsToBytes platform $ tupleSize tuple_info)
                             (map atomPrimRep es)
    return (mconcat pushes `appOL` ret)

-- Compile code to apply the given expression to the remaining args
-- on the stack, returning a HNF.
schemeE
    :: StackDepth -> Sequel -> BCEnv -> CgStgExpr -> BcM BCInstrList
schemeE d s p (StgLit lit) = returnUnboxedAtom d s p (StgLitArg lit)
schemeE d s p (StgApp x [])
   | isUnliftedType (idType x) = returnUnboxedAtom d s p (StgVarArg x)
-- Delegate tail-calls to schemeT.
schemeE d s p e@(StgApp {}) = schemeT d s p e
schemeE d s p e@(StgConApp {}) = schemeT d s p e
schemeE d s p e@(StgOpApp {}) = schemeT d s p e
schemeE d s p (StgLetNoEscape xlet bnd body)
   = schemeE d s p (StgLet xlet bnd body)
schemeE d s p (StgLet _xlet
                      (StgNonRec x (StgRhsCon _cc data_con _cnum _ticks args))
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
         n_binds = genericLength xs

         fvss  = map (fvsToEnv p') rhss

         -- Sizes of free vars
         size_w = trunc16W . idSizeW platform
         sizes = map (\rhs_fvs -> sum (map size_w rhs_fvs)) fvss

         -- the arity of each rhs
         arities = map (genericLength . fst . collect) rhss

         -- This p', d' defn is safe because all the items being pushed
         -- are ptrs, so all have size 1 word.  d' and p' reflect the stack
         -- after the closures have been allocated in the heap (but not
         -- filled in), and pointers to them parked on the stack.
         offsets = mkStackOffsets d (genericReplicate n_binds (wordSize platform))
         p' = Map.insertList (zipE xs offsets) p
         d' = d + wordsToBytes platform n_binds
         zipE = zipEqual "schemeE"

         -- ToDo: don't build thunks for things with no free variables
         build_thunk
             :: StackDepth
             -> [Id]
             -> Word16
             -> ProtoBCO Name
             -> Word16
             -> Word16
             -> BcM BCInstrList
         build_thunk _ [] size bco off arity
            = return (PUSH_BCO bco `consOL` unitOL (mkap (off+size) size))
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
                    | is_tick     = ALLOC_AP_NOUPD sz
                    | otherwise   = ALLOC_AP sz
                 mkAlloc sz arity = ALLOC_PAP arity sz

         is_tick = case binds of
                     StgNonRec id _ -> occNameFS (getOccName id) == tickFS
                     _other -> False

         compile_bind d' fvs x (rhs::CgStgRhs) size arity off = do
                bco <- schemeR fvs (getName x,rhs)
                build_thunk d' fvs size bco off arity

         compile_binds =
            [ compile_bind d' fvs x rhs size arity (trunc16W n)
            | (fvs, x, rhs, size, arity, n) <-
                zip6 fvss xs rhss sizes arities [n_binds, n_binds-1 .. 1]
            ]
     body_code <- schemeE d' s p' body
     thunk_codes <- sequence compile_binds
     return (alloc_code `appOL` concatOL thunk_codes `appOL` body_code)

schemeE _d _s _p (StgTick (Breakpoint _ bp_id _) _rhs)
   = panic ("schemeE: Breakpoint without let binding: " ++
            show bp_id ++
            " forgot to run bcPrep?")

-- ignore other kinds of tick
schemeE d s p (StgTick _ rhs) = schemeE d s p rhs

-- no alts: scrut is guaranteed to diverge
schemeE d s p (StgCase scrut _ _ []) = schemeE d s p scrut

schemeE d s p (StgCase scrut bndr _ alts)
   = doCase d s p scrut bndr alts

-- Is this Id a not-necessarily-lifted join point?
-- See Note [Not-necessarily-lifted join points], step 1
isNNLJoinPoint :: Id -> Bool
isNNLJoinPoint x = isJoinId x &&
                   Just True /= isLiftedType_maybe (idType x)

-- Update an Id's type to take a Void# argument.
-- Precondition: the Id is a not-necessarily-lifted join point.
-- See Note [Not-necessarily-lifted join points]
protectNNLJoinPointId :: Id -> Id
protectNNLJoinPointId x
  = ASSERT( isNNLJoinPoint x )
    updateIdTypeButNotMult (unboxedUnitTy `mkVisFunTyMany`) x

{-
   Ticked Expressions
   ------------------

  The idea is that the "breakpoint<n,fvs> E" is really just an annotation on
  the code. When we find such a thing, we pull out the useful information,
  and then compile the code as if it was just the expression E.

Note [Not-necessarily-lifted join points]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A join point variable is essentially a goto-label: it is, for example,
never used as an argument to another function, and it is called only
in tail position. See Note [Join points] and Note [Invariants on join points],
both in GHC.Core. Because join points do not compile to true, red-blooded
variables (with, e.g., registers allocated to them), they are allowed
to be levity-polymorphic. (See invariant #6 in Note [Invariants on join points]
in GHC.Core.)

However, in this byte-code generator, join points *are* treated just as
ordinary variables. There is no check whether a binding is for a join point
or not; they are all treated uniformly. (Perhaps there is a missed optimization
opportunity here, but that is beyond the scope of my (Richard E's) Thursday.)

We thus must have *some* strategy for dealing with levity-polymorphic and
unlifted join points. Levity-polymorphic variables are generally not allowed
(though levity-polymorphic join points *are*; see Note [Invariants on join points]
in GHC.Core, point 6), and we don't wish to evaluate unlifted join points eagerly.
The questionable join points are *not-necessarily-lifted join points*
(NNLJPs). (Not having such a strategy led to #16509, which panicked in the
isUnliftedType check in the AnnVar case of schemeE.) Here is the strategy:

1. Detect NNLJPs. This is done in isNNLJoinPoint.

2. When binding an NNLJP, add a `\ (_ :: (# #)) ->` to its RHS, and modify the
   type to tack on a `(# #) ->`.
   Note that functions are never levity-polymorphic, so this transformation
   changes an NNLJP to a non-levity-polymorphic join point. This is done
   in bcPrepSingleBind.

3. At an occurrence of an NNLJP, add an application to void# (called voidPrimId),
   being careful to note the new type of the NNLJP. This is done in the AnnVar
   case of schemeE, with help from protectNNLJoinPointId.

Here is an example. Suppose we have

  f = \(r :: RuntimeRep) (a :: TYPE r) (x :: T).
      join j :: a
           j = error @r @a "bloop"
      in case x of
           A -> j
           B -> j
           C -> error @r @a "blurp"

Our plan is to behave is if the code was

  f = \(r :: RuntimeRep) (a :: TYPE r) (x :: T).
      let j :: (Void# -> a)
          j = \ _ -> error @r @a "bloop"
      in case x of
           A -> j void#
           B -> j void#
           C -> error @r @a "blurp"

It's a bit hacky, but it works well in practice and is local. I suspect the
Right Fix is to take advantage of join points as goto-labels.

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
      then generateCCall d s p ccall_spec result_ty (reverse args)
      else unsupportedCConvException

schemeT d s p (StgOpApp (StgPrimOp op) args _ty)
   = doTailCall d s p (primOpId op) (reverse args)

schemeT _d _s _p (StgOpApp StgPrimCallOp{} _args _ty)
   = unsupportedCConvException

   -- Case 2: Unboxed tuple
schemeT d s p (StgConApp con _ext args _tys)
   | isUnboxedTupleDataCon con || isUnboxedSumDataCon con
   = returnUnboxedTuple d s p args

   -- Case 3: Ordinary data constructor
   | otherwise
   = do alloc_con <- mkConAppCode d s p con args
        platform <- profilePlatform <$> getProfile
        return (alloc_con         `appOL`
                mkSlideW 1 (bytesToWords platform $ d - s) `snocOL`
                ENTER)

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
                [ NonVoid (prim_rep, arg)
                | arg <- args
                , let prim_rep = atomPrimRep arg
                , not (isVoidRep prim_rep)
                ]
            (_, _, args_offsets) =
                mkVirtHeapOffsetsWithPadding profile StdHeader non_voids

            do_pushery !d (arg : args) = do
                (push, arg_bytes) <- case arg of
                    (Padding l _) -> return $! pushPadding (ByteOff l)
                    (FieldOff a _) -> pushConstrAtom d p (fromNonVoid a)
                more_push_code <- do_pushery (d + arg_bytes) args
                return (push `appOL` more_push_code)
            do_pushery !d [] = do
                let !n_arg_words = trunc16W $ bytesToWords platform (d - orig_d)
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
   do_pushes init_d args (map (atomRep platform) args)
  where
  do_pushes !d [] reps = do
        ASSERT( null reps ) return ()
        (push_fn, sz) <- pushAtom d p (StgVarArg fn)
        platform <- profilePlatform <$> getProfile
        ASSERT( sz == wordSize platform ) return ()
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
findPushSeq _
  = panic "GHC.StgToByteCode.findPushSeq"

-- -----------------------------------------------------------------------------
-- Case expressions

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
        -- like (# Word# #) or (# Int#, State# RealWorld# #) do not have a
        -- tuple return frame. This is because (# foo #) and (# foo, Void# #)
        -- have the same runtime rep. We have more efficient specialized
        -- return frames for the situations with one non-void element.

        ubx_tuple_frame =
          (isUnboxedTupleType bndr_ty || isUnboxedSumType bndr_ty) &&
          length non_void_arg_reps > 1

        non_void_arg_reps = non_void (typeArgReps platform bndr_ty)

        profiling
          | Just interp <- hsc_interp hsc_env
          = interpreterProfiled interp
          | otherwise = False

        -- Top of stack is the return itbl, as usual.
        -- underneath it is the pointer to the alt_code BCO.
        -- When an alt is entered, it assumes the returned value is
        -- on top of the itbl.
        ret_frame_size_b :: StackDepth
        ret_frame_size_b | ubx_tuple_frame =
                             (if profiling then 5 else 4) * wordSize platform
                         | otherwise = 2 * wordSize platform

        -- The stack space used to save/restore the CCCS when profiling
        save_ccs_size_b | profiling &&
                          not ubx_tuple_frame = 2 * wordSize platform
                        | otherwise = 0

        -- An unlifted value gets an extra info table pushed on top
        -- when it is returned.
        unlifted_itbl_size_b :: StackDepth
        unlifted_itbl_size_b | isAlgCase       = 0
                             | ubx_tuple_frame = 3 * wordSize platform
                             | otherwise       = wordSize platform

        (bndr_size, tuple_info, args_offsets)
           | ubx_tuple_frame =
               let bndr_ty = primRepCmmType platform
                   bndr_reps = filter (not.isVoidRep) (bcIdPrimReps bndr)
                   (tuple_info, args_offsets) =
                       layoutTuple profile 0 bndr_ty bndr_reps
               in ( wordsToBytes platform (tupleSize tuple_info)
                  , tuple_info
                  , args_offsets
                  )
           | otherwise = ( wordsToBytes platform (idSizeW platform bndr)
                         , voidTupleInfo
                         , []
                         )

        -- depth of stack after the return value has been pushed
        d_bndr =
            d + ret_frame_size_b + bndr_size

        -- depth of stack after the extra info table for an unboxed return
        -- has been pushed, if any.  This is the stack depth at the
        -- continuation.
        d_alts = d + ret_frame_size_b + bndr_size + unlifted_itbl_size_b

        -- Env in which to compile the alts, not including
        -- any vars bound by the alts themselves
        p_alts = Map.insert bndr d_bndr p

        bndr_ty = idType bndr
        isAlgCase = not (isUnliftedType bndr_ty)

        -- given an alt, return a discr and code for it.
        codeAlt (DEFAULT, _, rhs)
           = do rhs_code <- schemeE d_alts s p_alts rhs
                return (NoDiscr, rhs_code)

        codeAlt alt@(_, bndrs, rhs)
           -- primitive or nullary constructor alt: no need to UNPACK
           | null real_bndrs = do
                rhs_code <- schemeE d_alts s p_alts rhs
                return (my_discr alt, rhs_code)
           | isUnboxedTupleType bndr_ty || isUnboxedSumType bndr_ty =
             let bndr_ty = primRepCmmType platform . bcIdPrimRep
                 tuple_start = d_bndr
                 (tuple_info, args_offsets) =
                   layoutTuple profile
                               0
                               bndr_ty
                               bndrs

                 stack_bot = d_alts

                 p' = Map.insertList
                        [ (arg, tuple_start -
                                wordsToBytes platform (tupleSize tuple_info) +
                                offset)
                        | (arg, offset) <- args_offsets
                        , not (isVoidRep $ bcIdPrimRep arg)]
                        p_alts
             in do
               rhs_code <- schemeE stack_bot s p' rhs
               return (NoDiscr, rhs_code)
           -- algebraic alt with some binders
           | otherwise =
             let (tot_wds, _ptrs_wds, args_offsets) =
                     mkVirtHeapOffsets profile NoHeader
                         [ NonVoid (bcIdPrimRep id, id)
                         | NonVoid id <- nonVoidIds real_bndrs
                         ]
                 size = WordOff tot_wds

                 stack_bot = d_alts + wordsToBytes platform size

                 -- convert offsets from Sp into offsets into the virtual stack
                 p' = Map.insertList
                        [ (arg, stack_bot - ByteOff offset)
                        | (NonVoid arg, offset) <- args_offsets ]
                        p_alts
             in do
             MASSERT(isAlgCase)
             rhs_code <- schemeE stack_bot s p' rhs
             return (my_discr alt,
                     unitOL (UNPACK (trunc16W size)) `appOL` rhs_code)
           where
             real_bndrs = filterOut isTyVar bndrs

        my_discr (DEFAULT, _, _) = NoDiscr {-shouldn't really happen-}
        my_discr (DataAlt dc, _, _)
           | isUnboxedTupleDataCon dc || isUnboxedSumDataCon dc
           = NoDiscr
           | otherwise
           = DiscrP (fromIntegral (dataConTag dc - fIRST_TAG))
        my_discr (LitAlt l, _, _)
           = case l of LitNumber LitNumInt i  -> DiscrI (fromInteger i)
                       LitNumber LitNumWord w -> DiscrW (fromInteger w)
                       LitFloat r   -> DiscrF (fromRational r)
                       LitDouble r  -> DiscrD (fromRational r)
                       LitChar i    -> DiscrI (ord i)
                       _ -> pprPanic "schemeE(StgCase).my_discr" (ppr l)

        maybe_ncons
           | not isAlgCase = Nothing
           | otherwise
           = case [dc | (DataAlt dc, _, _) <- alts] of
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
           | ubx_tuple_frame && profiling = ([1], 3) -- tuple_info, tuple_BCO, CCCS
           | ubx_tuple_frame              = ([1], 2) -- tuple_info, tuple_BCO
           | otherwise                    = ([], 0)

        bitmap_size = trunc16W $ fromIntegral extra_slots +
                                 bytesToWords platform (d - s)

        bitmap_size' :: Int
        bitmap_size' = fromIntegral bitmap_size


        pointers =
          extra_pointers ++
          sort (filter (< bitmap_size') (map (+extra_slots) rel_slots))
          where
          binds = Map.toList p
          -- NB: unboxed tuple cases bind the scrut binder to the same offset
          -- as one of the alt binders, so we have to remove any duplicates here:
          rel_slots = nub $ map fromIntegral $ concatMap spread binds
          spread (id, offset) | isUnboxedTupleType (idType id) ||
                                isUnboxedSumType (idType id) = []
                              | isFollowableArg (bcIdArgRep platform id) = [ rel_offset ]
                              | otherwise                      = []
                where rel_offset = trunc16W $ bytesToWords platform (d - offset)

        bitmap = intsToReverseBitmap platform bitmap_size'{-size-} pointers

     alt_stuff <- mapM codeAlt alts
     alt_final <- mkMultiBranch maybe_ncons alt_stuff

     let
         alt_bco_name = getName bndr
         alt_bco = mkProtoBCO platform alt_bco_name alt_final (Left alts)
                       0{-no arity-} bitmap_size bitmap True{-is alts-}
     scrut_code <- schemeE (d + ret_frame_size_b + save_ccs_size_b)
                           (d + ret_frame_size_b + save_ccs_size_b)
                           p scrut
     alt_bco' <- emitBc alt_bco
     if ubx_tuple_frame
       then do
              let args_ptrs =
                    map (\(rep, off) -> (isFollowableArg (toArgRep platform rep), off))
                        args_offsets
              tuple_bco <- emitBc (tupleBCO platform tuple_info args_ptrs)
              return (PUSH_ALTS_TUPLE alt_bco' tuple_info tuple_bco
                      `consOL` scrut_code)
       else let push_alts
                  | isAlgCase
                  = PUSH_ALTS alt_bco'
                  | otherwise
                  = let unlifted_rep =
                          case non_void_arg_reps of
                            []    -> V
                            [rep] -> rep
                            _     -> panic "schemeE(StgCase).push_alts"
                    in PUSH_ALTS_UNLIFTED alt_bco' unlifted_rep
            in return (push_alts `consOL` scrut_code)


-- -----------------------------------------------------------------------------
-- Deal with tuples

-- The native calling convention uses registers for tuples, but in the
-- bytecode interpreter, all values live on the stack.

layoutTuple :: Profile
            -> ByteOff
            -> (a -> CmmType)
            -> [a]
            -> ( TupleInfo      -- See Note [GHCi TupleInfo]
               , [(a, ByteOff)] -- argument, offset on stack
               )
layoutTuple profile start_off arg_ty reps =
  let platform = profilePlatform profile
      (orig_stk_bytes, pos) = assignArgumentsPos profile
                                                 0
                                                 NativeReturn
                                                 arg_ty
                                                 reps

      -- keep the stack parameters in the same place
      orig_stk_params = [(x, fromIntegral off) | (x, StackParam off) <- pos]

      -- sort the register parameters by register and add them to the stack
      regs_order :: Map.Map GlobalReg Int
      regs_order = Map.fromList $ zip (tupleRegsCover platform) [0..]

      reg_order :: GlobalReg -> (Int, GlobalReg)
      reg_order reg | Just n <- Map.lookup reg regs_order = (n, reg)
      -- a VanillaReg goes to the same place regardless of whether it
      -- contains a pointer
      reg_order (VanillaReg n VNonGcPtr) = reg_order (VanillaReg n VGcPtr)
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

  in ( TupleInfo
         { tupleSize        = bytesToWords platform (ByteOff new_stk_bytes)
         , tupleRegs        = regs_set
         , tupleNativeStackSize = bytesToWords platform
                                               (ByteOff orig_stk_bytes)
         }
     , sortBy (comparing snd) $
              map (\(x, o) -> (x, o + start_off))
                  (orig_stk_params ++ map get_byte_off new_stk_params)
     )

{- Note [unboxed tuple bytecodes and tuple_BCO]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  We have the bytecode instructions RETURN_TUPLE and PUSH_ALTS_TUPLE to
  return and receive arbitrary unboxed tuples, respectively. These
  instructions use the helper data tuple_BCO and tuple_info.

  The helper data is used to convert tuples between GHCs native calling
  convention (object code), which uses stack and registers, and the bytecode
  calling convention, which only uses the stack. See Note [GHCi TupleInfo]
  for more details.


  Returning a tuple
  =================

  Bytecode that returns a tuple first pushes all the tuple fields followed
  by the appropriate tuple_info and tuple_BCO onto the stack. It then
  executes the RETURN_TUPLE instruction, which causes the interpreter
  to push stg_ret_t_info to the top of the stack. The stack (growing down)
  then looks as follows:

      ...
      next_frame
      tuple_field_1
      tuple_field_2
      ...
      tuple_field_n
      tuple_info
      tuple_BCO
      stg_ret_t_info <- Sp

  If next_frame is bytecode, the interpreter will start executing it. If
  it's object code, the interpreter jumps back to the scheduler, which in
  turn jumps to stg_ret_t. stg_ret_t converts the tuple to the native
  calling convention using the description in tuple_info, and then jumps
  to next_frame.


  Receiving a tuple
  =================

  Bytecode that receives a tuple uses the PUSH_ALTS_TUPLE instruction to
  push a continuation, followed by jumping to the code that produces the
  tuple. The PUSH_ALTS_TUPLE instuction contains three pieces of data:

     * cont_BCO: the continuation that receives the tuple
     * tuple_info: see below
     * tuple_BCO: see below

  The interpreter pushes these onto the stack when the PUSH_ALTS_TUPLE
  instruction is executed, followed by stg_ctoi_tN_info, with N depending
  on the number of stack words used by the tuple in the GHC native calling
  convention. N is derived from tuple_info.

  For example if we expect a tuple with three words on the stack, the stack
  looks as follows after PUSH_ALTS_TUPLE:

      ...
      next_frame
      cont_free_var_1
      cont_free_var_2
      ...
      cont_free_var_n
      tuple_info
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


  The tuple_info word
  ===================

  The tuple_info word describes the stack and STG register (e.g. R1..R6,
  D1..D6) usage for the tuple. tuple_info contains enough information to
  convert the tuple between the stack-only bytecode and stack+registers
  GHC native calling conventions.

  See Note [GHCi tuple layout] for more details of how the data is packed
  in a single word.

 -}

tupleBCO :: Platform -> TupleInfo -> [(Bool, ByteOff)] -> [FFIInfo] -> ProtoBCO Name
tupleBCO platform info pointers =
  mkProtoBCO platform invented_name body_code (Left [])
             0{-no arity-} bitmap_size bitmap False{-is alts-}

  where
    {-
      The tuple BCO is never referred to by name, so we can get away
      with using a fake name here. We will need to change this if we want
      to save some memory by sharing the BCO between places that have
      the same tuple shape
    -}
    invented_name  = mkSystemVarName (mkPseudoUniqueE 0) (fsLit "tuple")

    -- the first word in the frame is the tuple_info word,
    -- which is not a pointer
    bitmap_size = trunc16W $ 1 + tupleSize info
    bitmap      = intsToReverseBitmap platform (fromIntegral bitmap_size) $
                  map ((+1) . fromIntegral . bytesToWords platform . snd)
                      (filter fst pointers)
    body_code = mkSlideW 0 1          -- pop frame header
                `snocOL` RETURN_TUPLE -- and add it again

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
generateCCall d0 s p (CCallSpec target cconv safety) result_ty args_r_to_l
 = do
     profile <- getProfile

     let
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
             :: ByteOff -> [StgArg] -> BcM [(BCInstrList, PrimRep)]
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
                 return ((code, AddrRep) : rest)
         pargs d (aa:az) =  do (code_a, sz_a) <- pushAtom d p aa
                               rest <- pargs (d + sz_a) az
                               return ((code_a, atomPrimRep aa) : rest)

     code_n_reps <- pargs d0 args_r_to_l
     let
         (pushs_arg, a_reps_pushed_r_to_l) = unzip code_n_reps
         a_reps_sizeW = sum (map (repSizeWords platform) a_reps_pushed_r_to_l)

         push_args    = concatOL pushs_arg
         !d_after_args = d0 + wordsToBytes platform a_reps_sizeW
         a_reps_pushed_RAW
            | null a_reps_pushed_r_to_l || not (isVoidRep (head a_reps_pushed_r_to_l))
            = panic "GHC.StgToByteCode.generateCCall: missing or invalid World token?"
            | otherwise
            = reverse (tail a_reps_pushed_r_to_l)

         -- Now: a_reps_pushed_RAW are the reps which are actually on the stack.
         -- push_args is the code to do that.
         -- d_after_args is the stack depth once the args are on.

         -- Get the result rep.
         (returns_void, r_rep)
            = case maybe_getCCallReturnRep result_ty of
                 Nothing -> (True,  VoidRep)
                 Just rr -> (False, rr)
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

         The interpreter then calls the marshall code mentioned
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
            void marshall_code ( StgWord* ptr_to_top_of_stack )
         -}
         -- resolve static address
         maybe_static_target :: Maybe Literal
         maybe_static_target =
             case target of
                 DynamicTarget -> Nothing
                 StaticTarget _ _ _ False ->
                   panic "generateCCall: unexpected FFI value import"
                 StaticTarget _ target _ True ->
                   Just (LitLabel target mb_size IsFunction)
                   where
                      mb_size
                          | OSMinGW32 <- platformOS platform
                          , StdCallConv <- cconv
                          = Just (fromIntegral a_reps_sizeW * platformWordSizeInBytes platform)
                          | otherwise
                          = Nothing

     let
         is_static = isJust maybe_static_target

         -- Get the arg reps, zapping the leading Addr# in the dynamic case
         a_reps --  | trace (showSDoc (ppr a_reps_pushed_RAW)) False = error "???"
                | is_static = a_reps_pushed_RAW
                | otherwise = if null a_reps_pushed_RAW
                              then panic "GHC.StgToByteCode.generateCCall: dyn with no args"
                              else tail a_reps_pushed_RAW

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
         push_r =
             if returns_void
                then nilOL
                else unitOL (PUSH_UBX (mkDummyLiteral platform r_rep) (trunc16W r_sizeW))

         -- generate the marshalling code we're going to call

         -- Offset of the next stack frame down the stack.  The CCALL
         -- instruction needs to describe the chunk of stack containing
         -- the ccall args to the GC, so it needs to know how large it
         -- is.  See comment in Interpreter.c with the CCALL instruction.
         stk_offset   = trunc16W $ bytesToWords platform (d_after_r - s)

         conv = case cconv of
           CCallConv -> FFICCall
           StdCallConv -> FFIStdCall
           _ -> panic "GHC.StgToByteCode: unexpected calling convention"

     -- the only difference in libffi mode is that we prepare a cif
     -- describing the call type by calling libffi, and we attach the
     -- address of this to the CCALL instruction.


     let ffires = primRepToFFIType platform r_rep
         ffiargs = map (primRepToFFIType platform) a_reps
     interp <- hscInterp <$> getHscEnv
     token <- ioToBc $ interpCmd interp (PrepFFI conv ffiargs ffires)
     recordFFIBc token

     let
         -- do the call
         do_call      = unitOL (CCALL stk_offset token flags)
           where flags = case safety of
                           PlaySafe          -> 0x0
                           PlayInterruptible -> 0x1
                           PlayRisky         -> 0x2

         -- slide and return
         d_after_r_min_s = bytesToWords platform (d_after_r - s)
         wrapup       = mkSlideW (trunc16W r_sizeW) (d_after_r_min_s - r_sizeW)
                        `snocOL` RETURN_UBX (toArgRep platform r_rep)
         --trace (show (arg1_offW, args_offW  ,  (map argRepSizeW a_reps) )) $
     return (
         push_args `appOL`
         push_Addr `appOL` push_r `appOL` do_call `appOL` wrapup
         )

primRepToFFIType :: Platform -> PrimRep -> FFIType
primRepToFFIType platform r
  = case r of
     VoidRep     -> FFIVoid
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
     _           -> panic "primRepToFFIType"
  where
    (signed_word, unsigned_word) = case platformWordSize platform of
       PW4 -> (FFISInt32, FFIUInt32)
       PW8 -> (FFISInt64, FFIUInt64)

-- Make a dummy literal, to be used as a placeholder for FFI return
-- values on the stack.
mkDummyLiteral :: Platform -> PrimRep -> Literal
mkDummyLiteral platform pr
   = case pr of
        IntRep    -> mkLitInt  platform 0
        WordRep   -> mkLitWord platform 0
        Int8Rep   -> mkLitInt8 0
        Word8Rep  -> mkLitWord8 0
        Int16Rep  -> mkLitInt16 0
        Word16Rep -> mkLitWord16 0
        Int32Rep  -> mkLitInt32 0
        Word32Rep -> mkLitWord32 0
        Int64Rep  -> mkLitInt64 0
        Word64Rep -> mkLitWord64 0
        AddrRep   -> LitNullAddr
        DoubleRep -> LitDouble 0
        FloatRep  -> LitFloat 0
        _         -> pprPanic "mkDummyLiteral" (ppr pr)


-- Convert (eg)
--     GHC.Prim.Char# -> GHC.Prim.State# GHC.Prim.RealWorld
--                   -> (# GHC.Prim.State# GHC.Prim.RealWorld, GHC.Prim.Int# #)
--
-- to  Just IntRep
-- and check that an unboxed pair is returned wherein the first arg is V'd.
--
-- Alternatively, for call-targets returning nothing, convert
--
--     GHC.Prim.Char# -> GHC.Prim.State# GHC.Prim.RealWorld
--                   -> (# GHC.Prim.State# GHC.Prim.RealWorld #)
--
-- to  Nothing

maybe_getCCallReturnRep :: Type -> Maybe PrimRep
maybe_getCCallReturnRep fn_ty
   = let
       (_a_tys, r_ty) = splitFunTys (dropForAlls fn_ty)
       r_reps = typePrimRepArgs r_ty

       blargh :: a -- Used at more than one type
       blargh = pprPanic "maybe_getCCallReturn: can't handle:"
                         (pprType fn_ty)
     in
       case r_reps of
         []            -> panic "empty typePrimRepArgs"
         [VoidRep]     -> Nothing
         [rep]
           | isGcPtrRep rep -> blargh
           | otherwise      -> Just rep

                 -- if it was, it would be impossible to create a
                 -- valid return value placeholder on the stack
         _             -> blargh

maybe_is_tagToEnum_call :: CgStgExpr -> Maybe (Id, [Name])
-- Detect and extract relevant info for the tagToEnum kludge.
maybe_is_tagToEnum_call (StgOpApp (StgPrimOp TagToEnumOp) [StgVarArg v] t)
  = Just (v, extract_constr_Names t)
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
                push bogus-word

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

The 'bogus-word' push is because TESTEQ_I expects the top of the stack
to have an info-table, and the next word to have the value to be
tested.  This is very weird, but it's the way it is right now.  See
Interpreter.c.  We don't actually need an info-table here; we just
need to have the argument to be one-from-top on the stack, hence pushing
a 1-word null. See #8383.
-}


implement_tagToId
    :: StackDepth
    -> Sequel
    -> BCEnv
    -> Id
    -> [Name]
    -> BcM BCInstrList
-- See Note [Implementing tagToEnum#]
implement_tagToId d s p arg names
  = ASSERT( notNull names )
    do (push_arg, arg_bytes) <- pushAtom d p (StgVarArg arg)
       labels <- getLabelsBc (genericLength names)
       label_fail <- getLabelBc
       label_exit <- getLabelBc
       dflags <- getDynFlags
       let infos = zip4 labels (tail labels ++ [label_fail])
                               [0 ..] names
           platform = targetPlatform dflags
           steps = map (mkStep label_exit) infos
           slide_ws = bytesToWords platform (d - s + arg_bytes)

       return (push_arg
               `appOL` unitOL (PUSH_UBX LitNullAddr 1)
                   -- Push bogus word (see Note [Implementing tagToEnum#])
               `appOL` concatOL steps
               `appOL` toOL [ LABEL label_fail, CASEFAIL,
                              LABEL label_exit ]
               `appOL` mkSlideW 1 (slide_ws + 1)
                   -- "+1" to account for bogus word
                   --      (see Note [Implementing tagToEnum#])
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

pushAtom
    :: StackDepth -> BCEnv -> StgArg -> BcM (BCInstrList, ByteOff)

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
            with_instr instr = do
                let !off_b = trunc16B $ d - d_v
                return (unitOL (instr off_b), wordSize platform)

        case szb of
            1 -> with_instr PUSH8_W
            2 -> with_instr PUSH16_W
            4 -> with_instr PUSH32_W
            _ -> do
                let !szw = bytesToWords platform szb
                    !off_w = trunc16W $ bytesToWords platform (d - d_v) + szw - 1
                return (toOL (genericReplicate szw (PUSH_L off_w)),
                              wordsToBytes platform szw)
        -- d - d_v           offset from TOS to the first slot of the object
        --
        -- d - d_v + sz - 1  offset from the TOS of the last slot of the object
        --
        -- Having found the last slot, we proceed to copy the right number of
        -- slots on to the top of the stack.

   | otherwise  -- var must be a global variable
   = do topStrings <- getTopStrings
        platform <- targetPlatform <$> getDynFlags
        case lookupVarEnv topStrings var of
            Just ptr -> pushAtom d p $ StgLitArg $ mkLitWord platform $
              fromIntegral $ ptrToWordPtr $ fromRemotePtr ptr
            Nothing -> do
                let sz = idSizeCon platform var
                MASSERT( sz == wordSize platform )
                return (unitOL (PUSH_G (getName var)), sz)


pushAtom _ _ (StgLitArg lit) = pushLiteral True lit

pushLiteral :: Bool -> Literal -> BcM (BCInstrList, ByteOff)
pushLiteral padded lit =
  do
     platform <- targetPlatform <$> getDynFlags
     let code :: PrimRep -> BcM (BCInstrList, ByteOff)
         code rep =
            return (padding_instr `snocOL` instr, size_bytes + padding_bytes)
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
                _  -> PUSH_UBX lit (trunc16W $ bytesToWords platform size_bytes)

     case lit of
        LitLabel {}     -> code AddrRep
        LitFloat {}     -> code FloatRep
        LitDouble {}    -> code DoubleRep
        LitChar {}      -> code WordRep
        LitNullAddr     -> code AddrRep
        LitString {}    -> code AddrRep
        LitRubbish {}   -> code WordRep
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
          -- No LitInteger's or LitNatural's should be left by the time this is
          -- called. CorePrep should have converted them all to a real core
          -- representation.
          LitNumInteger -> panic "pushAtom: LitInteger"
          LitNumNatural -> panic "pushAtom: LitNatural"

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
                let !off = trunc16B $ d - d_v
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

            -- Note [CASEFAIL] It may be that this case has no default
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
                  vals_lo = take n vals
                  vals_hi = drop n vals
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
         testLT (DiscrW i) fail_label = TESTLT_W i fail_label
         testLT (DiscrF i) fail_label = TESTLT_F i fail_label
         testLT (DiscrD i) fail_label = TESTLT_D i fail_label
         testLT (DiscrP i) fail_label = TESTLT_P i fail_label
         testLT NoDiscr    _          = panic "mkMultiBranch NoDiscr"

         testEQ (DiscrI i) fail_label = TESTEQ_I i fail_label
         testEQ (DiscrW i) fail_label = TESTEQ_W i fail_label
         testEQ (DiscrF i) fail_label = TESTEQ_F i fail_label
         testEQ (DiscrD i) fail_label = TESTEQ_D i fail_label
         testEQ (DiscrP i) fail_label = TESTEQ_P i fail_label
         testEQ NoDiscr    _          = panic "mkMultiBranch NoDiscr"

         -- None of these will be needed if there are no non-default alts
         (init_lo, init_hi)
            | null notd_ways
            = panic "mkMultiBranch: awesome foursome"
            | otherwise
            = case fst (head notd_ways) of
                DiscrI _ -> ( DiscrI minBound,  DiscrI maxBound )
                DiscrW _ -> ( DiscrW minBound,  DiscrW maxBound )
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
   | DiscrW Word
   | DiscrF Float
   | DiscrD Double
   | DiscrP Word16
   | NoDiscr
    deriving (Eq, Ord)

instance Outputable Discr where
   ppr (DiscrI i) = int i
   ppr (DiscrW w) = text (show w)
   ppr (DiscrF f) = text (show f)
   ppr (DiscrD d) = text (show d)
   ppr (DiscrP i) = ppr i
   ppr NoDiscr    = text "DEF"


lookupBCEnv_maybe :: Id -> BCEnv -> Maybe ByteOff
lookupBCEnv_maybe = Map.lookup

idSizeW :: Platform -> Id -> WordOff
idSizeW platform = WordOff . argRepSizeW platform . bcIdArgRep platform

idSizeCon :: Platform -> Id -> ByteOff
idSizeCon platform var
  -- unboxed tuple components are padded to word size
  | isUnboxedTupleType (idType var) ||
    isUnboxedSumType (idType var) =
    wordsToBytes platform .
    WordOff . sum . map (argRepSizeW platform . toArgRep platform) .
    bcIdPrimReps $ var
  | otherwise = ByteOff (primRepSizeB platform (bcIdPrimRep var))

bcIdArgRep :: Platform -> Id -> ArgRep
bcIdArgRep platform = toArgRep platform . bcIdPrimRep

bcIdPrimRep :: Id -> PrimRep
bcIdPrimRep id
  | [rep] <- typePrimRepArgs (idType id)
  = rep
  | otherwise
  = pprPanic "bcIdPrimRep" (ppr id <+> dcolon <+> ppr (idType id))


bcIdPrimReps :: Id -> [PrimRep]
bcIdPrimReps id = typePrimRepArgs (idType id)

repSizeWords :: Platform -> PrimRep -> WordOff
repSizeWords platform rep = WordOff $ argRepSizeW platform (toArgRep platform rep)

isFollowableArg :: ArgRep -> Bool
isFollowableArg P = True
isFollowableArg _ = False

-- | Indicate if the calling convention is supported
isSupportedCConv :: CCallSpec -> Bool
isSupportedCConv (CCallSpec _ cconv _) = case cconv of
   CCallConv            -> True     -- we explicitly pattern match on every
   StdCallConv          -> True     -- convention to ensure that a warning
   PrimCallConv         -> False    -- is triggered when a new one is added
   JavaScriptCallConv   -> False
   CApiConv             -> False

-- See bug #10462
unsupportedCConvException :: a
unsupportedCConvException = throwGhcException (ProgramError
  ("Error: bytecode compiler can't handle some foreign calling conventions\n"++
   "  Workaround: use -fobject-code, or compile this module to .o separately."))

mkSlideB :: Platform -> ByteOff -> ByteOff -> OrdList BCInstr
mkSlideB platform !nb !db = mkSlideW n d
  where
    !n = trunc16W $ bytesToWords platform nb
    !d = bytesToWords platform db

mkSlideW :: Word16 -> WordOff -> OrdList BCInstr
mkSlideW !n !ws
    | ws > fromIntegral limit
    -- If the amount to slide doesn't fit in a Word16, generate multiple slide
    -- instructions
    = SLIDE n limit `consOL` mkSlideW n (ws - fromIntegral limit)
    | ws == 0
    = nilOL
    | otherwise
    = unitOL (SLIDE n $ fromIntegral ws)
  where
    limit :: Word16
    limit = maxBound

atomPrimRep :: StgArg -> PrimRep
atomPrimRep (StgVarArg v) = bcIdPrimRep v
atomPrimRep (StgLitArg l) = typePrimRep1 (literalType l)

atomRep :: Platform -> StgArg -> ArgRep
atomRep platform e = toArgRep platform (atomPrimRep e)

-- | Let szsw be the sizes in bytes of some items pushed onto the stack, which
-- has initial depth @original_depth@.  Return the values which the stack
-- environment should map these items to.
mkStackOffsets :: ByteOff -> [ByteOff] -> [ByteOff]
mkStackOffsets original_depth szsb = tail (scanl' (+) original_depth szsb)

typeArgReps :: Platform -> Type -> [ArgRep]
typeArgReps platform = map (toArgRep platform) . typePrimRepArgs

-- -----------------------------------------------------------------------------
-- The bytecode generator's monad

data BcM_State
   = BcM_State
        { bcm_hsc_env :: HscEnv
        , uniqSupply  :: UniqSupply      -- for generating fresh variable names
        , thisModule  :: Module          -- current module (for breakpoints)
        , nextlabel   :: Word32          -- for generating local labels
        , ffis        :: [FFIInfo]       -- ffi info blocks, to free later
                                         -- Should be free()d when it is GCd
        , modBreaks   :: Maybe ModBreaks -- info about breakpoints
        , breakInfo   :: IntMap CgBreakInfo
        , topStrings  :: IdEnv (RemotePtr ()) -- top-level string literals
          -- See Note [generating code for top-level string literal bindings].
        }

newtype BcM r = BcM (BcM_State -> IO (BcM_State, r)) deriving (Functor)

ioToBc :: IO a -> BcM a
ioToBc io = BcM $ \st -> do
  x <- io
  return (st, x)

runBc :: HscEnv -> UniqSupply -> Module -> Maybe ModBreaks
      -> IdEnv (RemotePtr ())
      -> BcM r
      -> IO (BcM_State, r)
runBc hsc_env us this_mod modBreaks topStrings (BcM m)
   = m (BcM_State hsc_env us this_mod 0 [] modBreaks IntMap.empty topStrings)

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

emitBc :: ([FFIInfo] -> ProtoBCO Name) -> BcM (ProtoBCO Name)
emitBc bco
  = BcM $ \st -> return (st{ffis=[]}, bco (ffis st))

recordFFIBc :: RemotePtr C_ffi_cif -> BcM ()
recordFFIBc a
  = BcM $ \st -> return (st{ffis = FFIInfo a : ffis st}, ())

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

getCCArray :: BcM (Array BreakIndex (RemotePtr CostCentre))
getCCArray = BcM $ \st ->
  let breaks = expectJust "GHC.StgToByteCode.getCCArray" $ modBreaks st in
  return (st, modBreaks_ccs breaks)


newBreakInfo :: BreakIndex -> CgBreakInfo -> BcM ()
newBreakInfo ix info = BcM $ \st ->
  return (st{breakInfo = IntMap.insert ix info (breakInfo st)}, ())

newUnique :: BcM Unique
newUnique = BcM $
   \st -> case takeUniqFromSupply (uniqSupply st) of
             (uniq, us) -> let newState = st { uniqSupply = us }
                           in  return (newState, uniq)

getCurrentModule :: BcM Module
getCurrentModule = BcM $ \st -> return (st, thisModule st)

getTopStrings :: BcM (IdEnv (RemotePtr ()))
getTopStrings = BcM $ \st -> return (st, topStrings st)

newId :: Type -> BcM Id
newId ty = do
    uniq <- newUnique
    return $ mkSysLocal tickFS uniq Many ty

tickFS :: FastString
tickFS = fsLit "ticked"
