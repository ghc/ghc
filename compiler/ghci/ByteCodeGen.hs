{-# LANGUAGE CPP, MagicHash, RecordWildCards #-}
{-# OPTIONS_GHC -fprof-auto-top #-}
--
--  (c) The University of Glasgow 2002-2006
--

-- | ByteCodeGen: Generate bytecode from Core
module ByteCodeGen ( UnlinkedBCO, byteCodeGen, coreExprToBCOs ) where

#include "HsVersions.h"

import ByteCodeInstr
import ByteCodeAsm
import ByteCodeTypes

import GHCi
import GHCi.FFI
import GHCi.RemoteTypes
import BasicTypes
import DynFlags
import Outputable
import Platform
import Name
import MkId
import Id
import ForeignCall
import HscTypes
import CoreUtils
import CoreSyn
import PprCore
import Literal
import PrimOp
import CoreFVs
import Type
import RepType
import Kind            ( isLiftedTypeKind )
import DataCon
import TyCon
import Util
import VarSet
import TysPrim
import ErrUtils
import Unique
import FastString
import Panic
import StgCmmLayout     ( ArgRep(..), toArgRep, argRepSizeW )
import SMRep
import Bitmap
import OrdList
import Maybes

import Data.List
import Foreign
import Control.Monad
import Data.Char

import UniqSupply
import Module
import Control.Arrow ( second )

import Control.Exception
import Data.Array
import Data.Map (Map)
import Data.IntMap (IntMap)
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified FiniteMap as Map
import Data.Ord
import GHC.Stack.CCS

-- -----------------------------------------------------------------------------
-- Generating byte code for a complete module

byteCodeGen :: HscEnv
            -> Module
            -> CoreProgram
            -> [TyCon]
            -> Maybe ModBreaks
            -> IO CompiledByteCode
byteCodeGen hsc_env this_mod binds tycs mb_modBreaks
   = withTiming (pure dflags)
                (text "ByteCodeGen"<+>brackets (ppr this_mod))
                (const ()) $ do
        let flatBinds = [ (bndr, simpleFreeVars rhs)
                        | (bndr, rhs) <- flattenBinds binds]

        us <- mkSplitUniqSupply 'y'
        (BcM_State{..}, proto_bcos) <-
           runBc hsc_env us this_mod mb_modBreaks $
             mapM schemeTopBind flatBinds

        when (notNull ffis)
             (panic "ByteCodeGen.byteCodeGen: missing final emitBc?")

        dumpIfSet_dyn dflags Opt_D_dump_BCOs
           "Proto-BCOs" (vcat (intersperse (char ' ') (map ppr proto_bcos)))

        cbc <- assembleBCOs hsc_env proto_bcos tycs
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

-- -----------------------------------------------------------------------------
-- Generating byte code for an expression

-- Returns: the root BCO for this expression
coreExprToBCOs :: HscEnv
               -> Module
               -> CoreExpr
               -> IO UnlinkedBCO
coreExprToBCOs hsc_env this_mod expr
 = withTiming (pure dflags)
              (text "ByteCodeGen"<+>brackets (ppr this_mod))
              (const ()) $ do
      -- create a totally bogus name for the top-level BCO; this
      -- should be harmless, since it's never used for anything
      let invented_name  = mkSystemVarName (mkPseudoUniqueE 0) (fsLit "ExprTopLevel")
          invented_id    = Id.mkLocalId invented_name (panic "invented_id's type")

      -- the uniques are needed to generate fresh variables when we introduce new
      -- let bindings for ticked expressions
      us <- mkSplitUniqSupply 'y'
      (BcM_State _dflags _us _this_mod _final_ctr mallocd _ _ , proto_bco)
         <- runBc hsc_env us this_mod Nothing $
              schemeTopBind (invented_id, simpleFreeVars expr)

      when (notNull mallocd)
           (panic "ByteCodeGen.coreExprToBCOs: missing final emitBc?")

      dumpIfSet_dyn dflags Opt_D_dump_BCOs "Proto-BCOs" (ppr proto_bco)

      assembleOneBCO hsc_env proto_bco
  where dflags = hsc_dflags hsc_env

-- The regular freeVars function gives more information than is useful to
-- us here. simpleFreeVars does the impedence matching.
simpleFreeVars :: CoreExpr -> AnnExpr Id DVarSet
simpleFreeVars = go . freeVars
  where
    go :: AnnExpr Id FVAnn -> AnnExpr Id DVarSet
    go (ann, e) = (freeVarsOfAnn ann, go' e)

    go' :: AnnExpr' Id FVAnn -> AnnExpr' Id DVarSet
    go' (AnnVar id)                  = AnnVar id
    go' (AnnLit lit)                 = AnnLit lit
    go' (AnnLam bndr body)           = AnnLam bndr (go body)
    go' (AnnApp fun arg)             = AnnApp (go fun) (go arg)
    go' (AnnConApp dc args)          = AnnConApp dc (map go args)
    go' (AnnCase scrut bndr ty alts) = AnnCase (go scrut) bndr ty (map go_alt alts)
    go' (AnnLet bind body)           = AnnLet (go_bind bind) (go body)
    go' (AnnCast expr (ann, co))     = AnnCast (go expr) (freeVarsOfAnn ann, co)
    go' (AnnTick tick body)          = AnnTick tick (go body)
    go' (AnnType ty)                 = AnnType ty
    go' (AnnCoercion co)             = AnnCoercion co

    go_alt (con, args, expr) = (con, args, go expr)

    go_bind (AnnNonRec bndr rhs) = AnnNonRec bndr (go rhs)
    go_bind (AnnRec pairs)       = AnnRec (map (second go) pairs)

-- -----------------------------------------------------------------------------
-- Compilation schema for the bytecode generator

type BCInstrList = OrdList BCInstr

type Sequel = Word -- back off to this depth before ENTER

-- Maps Ids to the offset from the stack _base_ so we don't have
-- to mess with it after each push/pop.
type BCEnv = Map Id Word -- To find vars on the stack

{-
ppBCEnv :: BCEnv -> SDoc
ppBCEnv p
   = text "begin-env"
     $$ nest 4 (vcat (map pp_one (sortBy cmp_snd (Map.toList p))))
     $$ text "end-env"
     where
        pp_one (var, offset) = int offset <> colon <+> ppr var <+> ppr (bcIdArgRep var)
        cmp_snd x y = compare (snd x) (snd y)
-}

-- Create a BCO and do a spot of peephole optimisation on the insns
-- at the same time.
mkProtoBCO
   :: DynFlags
   -> name
   -> BCInstrList
   -> Either  [AnnAlt Id DVarSet] (AnnExpr Id DVarSet)
   -> Int
   -> Word16
   -> [StgWord]
   -> Bool      -- True <=> is a return point, rather than a function
   -> [FFIInfo]
   -> ProtoBCO name
mkProtoBCO dflags nm instrs_ordlist origin arity bitmap_size bitmap is_ret ffis
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
           | is_ret && stack_usage < fromIntegral (aP_STACK_SPLIM dflags) = peep_d
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

argBits :: DynFlags -> [ArgRep] -> [Bool]
argBits _      [] = []
argBits dflags (rep : args)
  | isFollowableArg rep  = False : argBits dflags args
  | otherwise = take (argRepSizeW dflags rep) (repeat True) ++ argBits dflags args

-- -----------------------------------------------------------------------------
-- schemeTopBind

-- Compile code for the right-hand side of a top-level binding

schemeTopBind :: (Id, AnnExpr Id DVarSet) -> BcM (ProtoBCO Name)


schemeTopBind (id, rhs)
  | Just data_con <- isDataConWorkId_maybe id,
    isNullaryRepDataCon data_con = do
    dflags <- getDynFlags
        -- Special case for the worker of a nullary data con.
        -- It'll look like this:        Nil = /\a -> Nil a
        -- If we feed it into schemeR, we'll get
        --      Nil = Nil
        -- because mkConAppCode treats nullary constructor applications
        -- by just re-using the single top-level definition.  So
        -- for the worker itself, we must allocate it directly.
    -- ioToBc (putStrLn $ "top level BCO")
    emitBc (mkProtoBCO dflags (getName id) (toOL [PACK data_con 0, ENTER])
                       (Right rhs) 0 0 [{-no bitmap-}] False{-not alts-})

  | otherwise
  = schemeR [{- No free variables -}] (id, rhs)


-- -----------------------------------------------------------------------------
-- schemeR

-- Compile code for a right-hand side, to give a BCO that,
-- when executed with the free variables and arguments on top of the stack,
-- will return with a pointer to the result on top of the stack, after
-- removing the free variables and arguments.
--
-- Park the resulting BCO in the monad.  Also requires the
-- variable to which this value was bound, so as to give the
-- resulting BCO a name.

schemeR :: [Id]                 -- Free vars of the RHS, ordered as they
                                -- will appear in the thunk.  Empty for
                                -- top-level things, which have no free vars.
        -> (Id, AnnExpr Id DVarSet)
        -> BcM (ProtoBCO Name)
schemeR fvs (nm, rhs)
{-
   | trace (showSDoc (
              (char ' '
               $$ (ppr.filter (not.isTyVar).dVarSetElems.fst) rhs
               $$ pprCoreExpr (deAnnotate rhs)
               $$ char ' '
              ))) False
   = undefined
   | otherwise
-}
   = schemeR_wrk fvs nm rhs (collect rhs)

collect :: AnnExpr Id DVarSet -> ([Var], AnnExpr' Id DVarSet)
collect (_, e) = go [] e
  where
    go xs e | Just e' <- bcView e = go xs e'
    go xs (AnnLam x (_,e))
      | repTypeArgs (idType x) `lengthExceeds` 1
      = multiValException
      | otherwise
      = go (x:xs) e
    go xs not_lambda = (reverse xs, not_lambda)

schemeR_wrk :: [Id] -> Id -> AnnExpr Id DVarSet -> ([Var], AnnExpr' Var DVarSet) -> BcM (ProtoBCO Name)
schemeR_wrk fvs nm original_body (args, body)
   = do
     dflags <- getDynFlags
     let
         all_args  = reverse args ++ fvs
         arity     = length all_args
         -- all_args are the args in reverse order.  We're compiling a function
         -- \fv1..fvn x1..xn -> e
         -- i.e. the fvs come first

         szsw_args = map (fromIntegral . idSizeW dflags) all_args
         szw_args  = sum szsw_args
         p_init    = Map.fromList (zip all_args (mkStackOffsets 0 szsw_args))

         -- make the arg bitmap
         bits = argBits dflags (reverse (map bcIdArgRep all_args))
         bitmap_size = genericLength bits
         bitmap = mkBitmap dflags bits
     body_code <- schemeER_wrk szw_args p_init body

     emitBc (mkProtoBCO dflags (getName nm) body_code (Right original_body)
                 arity bitmap_size bitmap False{-not alts-})

-- introduce break instructions for ticked expressions
schemeER_wrk :: Word -> BCEnv -> AnnExpr' Id DVarSet -> BcM BCInstrList
schemeER_wrk d p rhs
  | AnnTick (Breakpoint tick_no fvs) (_annot, newRhs) <- rhs
  = do  code <- schemeE (fromIntegral d) 0 p newRhs
        cc_arr <- getCCArray
        this_mod <- moduleName <$> getCurrentModule
        let idOffSets = getVarOffSets d p fvs
        let breakInfo = CgBreakInfo
                        { cgb_vars = idOffSets
                        , cgb_resty = exprType (deAnnotate' newRhs)
                        }
        newBreakInfo tick_no breakInfo
        dflags <- getDynFlags
        let cc | interpreterProfiled dflags = cc_arr ! tick_no
               | otherwise = toRemotePtr nullPtr
        let breakInstr = BRK_FUN (fromIntegral tick_no) (getUnique this_mod) cc
        return $ breakInstr `consOL` code
   | otherwise = schemeE (fromIntegral d) 0 p rhs

getVarOffSets :: Word -> BCEnv -> [Id] -> [(Id, Word16)]
getVarOffSets d p = catMaybes . map (getOffSet d p)

getOffSet :: Word -> BCEnv -> Id -> Maybe (Id, Word16)
getOffSet d env id
   = case lookupBCEnv_maybe id env of
        Nothing     -> Nothing
        Just offset -> Just (id, trunc16 $ d - offset)

trunc16 :: Word -> Word16
trunc16 w
    | w > fromIntegral (maxBound :: Word16)
    = panic "stack depth overflow"
    | otherwise
    = fromIntegral w

fvsToEnv :: BCEnv -> DVarSet -> [Id]
-- Takes the free variables of a right-hand side, and
-- delivers an ordered list of the local variables that will
-- be captured in the thunk for the RHS
-- The BCEnv argument tells which variables are in the local
-- environment: these are the ones that should be captured
--
-- The code that constructs the thunk, and the code that executes
-- it, have to agree about this layout
fvsToEnv p fvs = [v | v <- dVarSetElems fvs,
                      isId v,           -- Could be a type variable
                      v `Map.member` p]

-- -----------------------------------------------------------------------------
-- schemeE

returnUnboxedAtom :: Word -> Sequel -> BCEnv
                 -> AnnExpr' Id DVarSet -> ArgRep
                 -> BcM BCInstrList
-- Returning an unlifted value.
-- Heave it on the stack, SLIDE, and RETURN.
returnUnboxedAtom d s p e e_rep
   = do (push, szw) <- pushAtom d p e
        return (push                       -- value onto stack
                `appOL`  mkSLIDE szw (d-s) -- clear to sequel
                `snocOL` RETURN_UBX e_rep) -- go

-- Compile code to apply the given expression to the remaining args
-- on the stack, returning a HNF.
schemeE :: Word -> Sequel -> BCEnv -> AnnExpr' Id DVarSet -> BcM BCInstrList

schemeE d s p e
   | Just e' <- bcView e
   = schemeE d s p e'

-- Delegate tail-calls to schemeT.
schemeE d s p e@(AnnApp _ _) = schemeT d s p e
schemeE d s p e@(AnnConApp _ _) = schemeT d s p e

schemeE d s p e@(AnnLit lit)     = returnUnboxedAtom d s p e (typeArgRep (literalType lit))
schemeE d s p e@(AnnCoercion {}) = returnUnboxedAtom d s p e V

schemeE d s p e@(AnnVar v)
    | isUnliftedType (idType v) = returnUnboxedAtom d s p e (bcIdArgRep v)
    | otherwise                 = schemeT d s p e

schemeE d s p (AnnLet (AnnNonRec x (_,rhs)) (_,body))
   | (AnnVar v, args_r_to_l) <- splitApp rhs,
     Just data_con <- isDataConWorkId_maybe v,
     dataConRepArity data_con == length args_r_to_l
     -- TODO #12618 remove eventually
   = do -- Special case for a non-recursive let whose RHS is a
        -- saturatred constructor application.
        -- Just allocate the constructor and carry on
        alloc_code <- mkConAppCode d s p data_con args_r_to_l
        body_code <- schemeE (d+1) s (Map.insert x d p) body
        return (alloc_code `appOL` body_code)

schemeE d s p (AnnLet (AnnNonRec x (_,AnnConApp dc args)) (_,body))
   = do -- Special case for a non-recursive let whose RHS is a
        -- saturatred constructor application.
        -- Just allocate the constructor and carry on
        let args_r_to_l = reverse $ map snd $ dropWhile isAnnTypeArg args
        alloc_code <- mkConAppCode d s p dc args_r_to_l
        body_code <- schemeE (d+1) s (Map.insert x d p) body
        return (alloc_code `appOL` body_code)

-- General case for let.  Generates correct, if inefficient, code in
-- all situations.
schemeE d s p (AnnLet binds (_,body)) = do
     dflags <- getDynFlags
     let (xs,rhss) = case binds of AnnNonRec x rhs  -> ([x],[rhs])
                                   AnnRec xs_n_rhss -> unzip xs_n_rhss
         n_binds = genericLength xs

         fvss  = map (fvsToEnv p' . fst) rhss

         -- Sizes of free vars
         sizes = map (\rhs_fvs -> sum (map (fromIntegral . idSizeW dflags) rhs_fvs)) fvss

         -- the arity of each rhs
         arities = map (genericLength . fst . collect) rhss

         -- This p', d' defn is safe because all the items being pushed
         -- are ptrs, so all have size 1.  d' and p' reflect the stack
         -- after the closures have been allocated in the heap (but not
         -- filled in), and pointers to them parked on the stack.
         p'    = Map.insertList (zipE xs (mkStackOffsets d (genericReplicate n_binds 1))) p
         d'    = d + fromIntegral n_binds
         zipE  = zipEqual "schemeE"

         -- ToDo: don't build thunks for things with no free variables
         build_thunk _ [] size bco off arity
            = return (PUSH_BCO bco `consOL` unitOL (mkap (off+size) size))
           where
                mkap | arity == 0 = MKAP
                     | otherwise  = MKPAP
         build_thunk dd (fv:fvs) size bco off arity = do
              (push_code, pushed_szw) <- pushAtom dd p' (AnnVar fv)
              more_push_code <- build_thunk (dd + fromIntegral pushed_szw) fvs size bco off arity
              return (push_code `appOL` more_push_code)

         alloc_code = toOL (zipWith mkAlloc sizes arities)
           where mkAlloc sz 0
                    | is_tick     = ALLOC_AP_NOUPD sz
                    | otherwise   = ALLOC_AP sz
                 mkAlloc sz arity = ALLOC_PAP arity sz

         is_tick = case binds of
                     AnnNonRec id _ -> occNameFS (getOccName id) == tickFS
                     _other -> False

         compile_bind d' fvs x rhs size arity off = do
                bco <- schemeR fvs (x,rhs)
                build_thunk d' fvs size bco off arity

         compile_binds =
            [ compile_bind d' fvs x rhs size arity n
            | (fvs, x, rhs, size, arity, n) <-
                zip6 fvss xs rhss sizes arities [n_binds, n_binds-1 .. 1]
            ]
     body_code <- schemeE d' s p' body
     thunk_codes <- sequence compile_binds
     return (alloc_code `appOL` concatOL thunk_codes `appOL` body_code)

-- Introduce a let binding for a ticked case expression. This rule
-- *should* only fire when the expression was not already let-bound
-- (the code gen for let bindings should take care of that).  Todo: we
-- call exprFreeVars on a deAnnotated expression, this may not be the
-- best way to calculate the free vars but it seemed like the least
-- intrusive thing to do
schemeE d s p exp@(AnnTick (Breakpoint _id _fvs) _rhs)
   | isLiftedTypeKind (typeKind ty)
   = do   id <- newId ty
          -- Todo: is emptyVarSet correct on the next line?
          let letExp = AnnLet (AnnNonRec id (fvs, exp)) (emptyDVarSet, AnnVar id)
          schemeE d s p letExp

   | otherwise
   = do   -- If the result type is not definitely lifted, then we must generate
          --   let f = \s . tick<n> e
          --   in  f realWorld#
          -- When we stop at the breakpoint, _result will have an unlifted
          -- type and hence won't be bound in the environment, but the
          -- breakpoint will otherwise work fine.
          --
          -- NB (Trac #12007) this /also/ applies for if (ty :: TYPE r), where
          --    r :: RuntimeRep is a variable. This can happen in the
          --    continuations for a pattern-synonym matcher
          --    match = /\(r::RuntimeRep) /\(a::TYPE r).
          --            \(k :: Int -> a) \(v::T).
          --            case v of MkV n -> k n
          -- Here (k n) :: a :: Type r, so we don't know if it's lifted
          -- or not; but that should be fine provided we add that void arg.

          id <- newId (mkFunTy realWorldStatePrimTy ty)
          st <- newId realWorldStatePrimTy
          let letExp = AnnLet (AnnNonRec id (fvs, AnnLam st (emptyDVarSet, exp)))
                              (emptyDVarSet, (AnnApp (emptyDVarSet, AnnVar id)
                                                    (emptyDVarSet, AnnVar realWorldPrimId)))
          schemeE d s p letExp

   where
     exp' = deAnnotate' exp
     fvs  = exprFreeVarsDSet exp'
     ty   = exprType exp'

-- ignore other kinds of tick
schemeE d s p (AnnTick _ (_, rhs)) = schemeE d s p rhs

schemeE d s p (AnnCase (_,scrut) _ _ []) = schemeE d s p scrut
        -- no alts: scrut is guaranteed to diverge

schemeE d s p (AnnCase scrut bndr _ [(DataAlt dc, [bind1, bind2], rhs)])
   | isUnboxedTupleCon dc -- handles pairs with one void argument (e.g. state token)
   , [rep_ty1] <- repTypeArgs (idType bind1)
   , [rep_ty2] <- repTypeArgs (idType bind2)
        -- Convert
        --      case .... of x { (# V'd-thing, a #) -> ... }
        -- to
        --      case .... of a { DEFAULT -> ... }
        -- because the return convention for both are identical.
        --
        -- Note that it does not matter losing the void-rep thing from the
        -- envt (it won't be bound now) because we never look such things up.
   , Just res <- case () of
                   _ | isVoidTy rep_ty1 && not (isVoidTy rep_ty2)
                     -> Just $ doCase d s p scrut bind2 [(DEFAULT, [], rhs)] (Just bndr)
                     | isVoidTy rep_ty2 && not (isVoidTy rep_ty1)
                     -> Just $ doCase d s p scrut bind1 [(DEFAULT, [], rhs)] (Just bndr)
                     | otherwise
                     -> Nothing
   = res

schemeE d s p (AnnCase scrut bndr _ [(DataAlt dc, [bind1], rhs)])
   | isUnboxedTupleCon dc
   , repTypeArgs (idType bndr) `lengthIs` 1 -- handles unit tuples
   = doCase d s p scrut bind1 [(DEFAULT, [], rhs)] (Just bndr)

schemeE d s p (AnnCase scrut bndr _ alt@[(DEFAULT, [], _)])
   | isUnboxedTupleType (idType bndr)
   , [ty] <- repTypeArgs (idType bndr)
       -- handles any pattern with a single non-void binder; in particular I/O
       -- monad returns (# RealWorld#, a #)
   = doCase d s p scrut (bndr `setIdType` ty) alt (Just bndr)

schemeE d s p (AnnCase scrut bndr _ alts)
   = doCase d s p scrut bndr alts Nothing{-not an unboxed tuple-}

schemeE _ _ _ expr
   = pprPanic "ByteCodeGen.schemeE: unhandled case"
               (pprCoreExpr (deAnnotate' expr))

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
-- 2.  (Another nasty hack).  Spot (# a::V, b #) and treat
--     it simply as  b  -- since the representations are identical
--     (the V takes up zero stack space).  Also, spot
--     (# b #) and treat it as  b.
--
-- 3.  Application of a constructor, by defn saturated.
--     Split the args into ptrs and non-ptrs, and push the nonptrs,
--     then the ptrs, and then do PACK and RETURN.
--
-- 4.  Otherwise, it must be a function call.  Push the args
--     right to left, SLIDE and ENTER.

schemeT :: Word         -- Stack depth
        -> Sequel       -- Sequel depth
        -> BCEnv        -- stack env
        -> AnnExpr' Id DVarSet
        -> BcM BCInstrList

schemeT d s p (AnnConApp dc all_args)
   | isUnboxedTupleCon dc
   = case args of
        [arg2,arg1] | isVAtom arg1 ->
                  unboxedTupleReturn d s p arg2
        [arg2,arg1] | isVAtom arg2 ->
                  unboxedTupleReturn d s p arg1
        _other -> multiValException
   | otherwise
   = do ASSERT( dataConRepFullArity dc == length all_args ) return ()
        alloc_con <- mkConAppCode d s p dc (reverse args)
        return (alloc_con         `appOL`
                mkSLIDE 1 (d - s) `snocOL`
                ENTER)
  where args = map snd $ dropWhile isAnnTypeArg all_args
schemeT d s p app

--   | trace ("schemeT: env in = \n" ++ showSDocDebug (ppBCEnv p)) False
--   = panic "schemeT ?!?!"

--   | trace ("\nschemeT\n" ++ showSDoc (pprCoreExpr (deAnnotate' app)) ++ "\n") False
--   = error "?!?!"

   -- Case 0
   | Just (arg, constr_names) <- maybe_is_tagToEnum_call app
   = implement_tagToId d s p arg constr_names

   -- Case 1
   | Just (CCall ccall_spec) <- isFCallId_maybe fn
   = if isSupportedCConv ccall_spec
      then generateCCall d s p ccall_spec fn args_r_to_l
      else unsupportedCConvException


   -- Case 2: Constructor application
   | Just con <- maybe_saturated_dcon
   , isUnboxedTupleCon con
   = case args_r_to_l of
        [arg1,arg2] | isVAtom arg1 ->
                  unboxedTupleReturn d s p arg2
        [arg1,arg2] | isVAtom arg2 ->
                  unboxedTupleReturn d s p arg1
        _other -> multiValException

   -- Case 3: Ordinary data constructor
   | Just con <- maybe_saturated_dcon
   = do alloc_con <- mkConAppCode d s p con args_r_to_l
        return (alloc_con         `appOL`
                mkSLIDE 1 (d - s) `snocOL`
                ENTER)

   -- Case 4: Tail call of function
   | otherwise
   = doTailCall d s p fn args_r_to_l

   where
        -- Extract the args (R->L) and fn
        -- The function will necessarily be a variable,
        -- because we are compiling a tail call
      (AnnVar fn, args_r_to_l) = splitApp app

      -- Only consider this to be a constructor application iff it is
      -- saturated.  Otherwise, we'll call the constructor wrapper.
      n_args = length args_r_to_l
      maybe_saturated_dcon
        = case isDataConWorkId_maybe fn of
                Just con | dataConRepArity con == n_args -> Just con
                _ -> Nothing

-- -----------------------------------------------------------------------------
-- Generate code to build a constructor application,
-- leaving it on top of the stack

mkConAppCode :: Word -> Sequel -> BCEnv
             -> DataCon                 -- The data constructor
             -> [AnnExpr' Id DVarSet]    -- Args, in *reverse* order
             -> BcM BCInstrList

mkConAppCode _ _ _ con []       -- Nullary constructor
  = ASSERT( isNullaryRepDataCon con )
    return (unitOL (PUSH_G (getName (dataConWorkId con))))
        -- Instead of doing a PACK, which would allocate a fresh
        -- copy of this constructor, use the single shared version.

mkConAppCode orig_d _ p con args_r_to_l
  = ASSERT2( dataConRepArity con == length args_r_to_l, ppr con <+> ppr (length args_r_to_l) )
    do_pushery orig_d (non_ptr_args ++ ptr_args)
 where
        -- The args are already in reverse order, which is the way PACK
        -- expects them to be.  We must push the non-ptrs after the ptrs.
      (ptr_args, non_ptr_args) = partition isPtrAtom args_r_to_l

      do_pushery d (arg:args)
         = do (push, arg_words) <- pushAtom d p arg
              more_push_code <- do_pushery (d + fromIntegral arg_words) args
              return (push `appOL` more_push_code)
      do_pushery d []
         = return (unitOL (PACK con n_arg_words))
         where
           n_arg_words = trunc16 $ d - orig_d


-- -----------------------------------------------------------------------------
-- Returning an unboxed tuple with one non-void component (the only
-- case we can handle).
--
-- Remember, we don't want to *evaluate* the component that is being
-- returned, even if it is a pointed type.  We always just return.

unboxedTupleReturn
        :: Word -> Sequel -> BCEnv
        -> AnnExpr' Id DVarSet -> BcM BCInstrList
unboxedTupleReturn d s p arg = returnUnboxedAtom d s p arg (atomRep arg)

-- -----------------------------------------------------------------------------
-- Generate code for a tail-call

doTailCall
        :: Word -> Sequel -> BCEnv
        -> Id -> [AnnExpr' Id DVarSet]
        -> BcM BCInstrList
doTailCall init_d s p fn args
  = do_pushes init_d args (map atomRep args)
  where
  do_pushes d [] reps = do
        ASSERT( null reps ) return ()
        (push_fn, sz) <- pushAtom d p (AnnVar fn)
        ASSERT( sz == 1 ) return ()
        return (push_fn `appOL` (
                  mkSLIDE (trunc16 $ d - init_d + 1) (init_d - s) `appOL`
                  unitOL ENTER))
  do_pushes d args reps = do
      let (push_apply, n, rest_of_reps) = findPushSeq reps
          (these_args, rest_of_args) = splitAt n args
      (next_d, push_code) <- push_seq d these_args
      instrs <- do_pushes (next_d + 1) rest_of_args rest_of_reps
      --                          ^^^ for the PUSH_APPLY_ instruction
      return (push_code `appOL` (push_apply `consOL` instrs))

  push_seq d [] = return (d, nilOL)
  push_seq d (arg:args) = do
    (push_code, sz) <- pushAtom d p arg
    (final_d, more_push_code) <- push_seq (d + fromIntegral sz) args
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
  = panic "ByteCodeGen.findPushSeq"

-- -----------------------------------------------------------------------------
-- Case expressions

doCase  :: Word -> Sequel -> BCEnv
        -> AnnExpr Id DVarSet -> Id -> [AnnAlt Id DVarSet]
        -> Maybe Id  -- Just x <=> is an unboxed tuple case with scrut binder, don't enter the result
        -> BcM BCInstrList
doCase d s p (_,scrut) bndr alts is_unboxed_tuple
  | repTypeArgs (idType bndr) `lengthExceeds` 1
  = multiValException
  | otherwise
  = do
     dflags <- getDynFlags
     let
        profiling
          | gopt Opt_ExternalInterpreter dflags = gopt Opt_SccProfilingOn dflags
          | otherwise = rtsIsProfiled

        -- Top of stack is the return itbl, as usual.
        -- underneath it is the pointer to the alt_code BCO.
        -- When an alt is entered, it assumes the returned value is
        -- on top of the itbl.
        ret_frame_sizeW :: Word
        ret_frame_sizeW = 2

        -- The extra frame we push to save/restor the CCCS when profiling
        save_ccs_sizeW | profiling = 2
                       | otherwise = 0

        -- An unlifted value gets an extra info table pushed on top
        -- when it is returned.
        unlifted_itbl_sizeW :: Word
        unlifted_itbl_sizeW | isAlgCase = 0
                            | otherwise = 1

        -- depth of stack after the return value has been pushed
        d_bndr = d + ret_frame_sizeW + fromIntegral (idSizeW dflags bndr)

        -- depth of stack after the extra info table for an unboxed return
        -- has been pushed, if any.  This is the stack depth at the
        -- continuation.
        d_alts = d_bndr + unlifted_itbl_sizeW

        -- Env in which to compile the alts, not including
        -- any vars bound by the alts themselves
        d_bndr' = fromIntegral d_bndr - 1
        p_alts0 = Map.insert bndr d_bndr' p
        p_alts = case is_unboxed_tuple of
                   Just ubx_bndr -> Map.insert ubx_bndr d_bndr' p_alts0
                   Nothing       -> p_alts0

        bndr_ty = idType bndr
        isAlgCase = not (isUnliftedType bndr_ty) && isNothing is_unboxed_tuple

        -- given an alt, return a discr and code for it.
        codeAlt (DEFAULT, _, (_,rhs))
           = do rhs_code <- schemeE d_alts s p_alts rhs
                return (NoDiscr, rhs_code)

        codeAlt alt@(_, bndrs, (_,rhs))
           -- primitive or nullary constructor alt: no need to UNPACK
           | null real_bndrs = do
                rhs_code <- schemeE d_alts s p_alts rhs
                return (my_discr alt, rhs_code)
           -- algebraic alt with some binders
           | otherwise =
             let
                 (ptrs,nptrs) = partition (isFollowableArg.bcIdArgRep) real_bndrs
                 ptr_sizes    = map (fromIntegral . idSizeW dflags) ptrs
                 nptrs_sizes  = map (fromIntegral . idSizeW dflags) nptrs
                 bind_sizes   = ptr_sizes ++ nptrs_sizes
                 size         = sum ptr_sizes + sum nptrs_sizes
                 -- the UNPACK instruction unpacks in reverse order...
                 p' = Map.insertList
                        (zip (reverse (ptrs ++ nptrs))
                          (mkStackOffsets d_alts (reverse bind_sizes)))
                        p_alts
             in do
             MASSERT(isAlgCase)
             rhs_code <- schemeE (d_alts + size) s p' rhs
             return (my_discr alt, unitOL (UNPACK (trunc16 size)) `appOL` rhs_code)
           where
             real_bndrs = filterOut isTyVar bndrs

        my_discr (DEFAULT, _, _) = NoDiscr {-shouldn't really happen-}
        my_discr (DataAlt dc, _, _)
           | isUnboxedTupleCon dc || isUnboxedSumCon dc
           = multiValException
           | otherwise
           = DiscrP (fromIntegral (dataConTag dc - fIRST_TAG))
        my_discr (LitAlt l, _, _)
           = case l of MachInt i     -> DiscrI (fromInteger i)
                       MachWord w    -> DiscrW (fromInteger w)
                       MachFloat r   -> DiscrF (fromRational r)
                       MachDouble r  -> DiscrD (fromRational r)
                       MachChar i    -> DiscrI (ord i)
                       _ -> pprPanic "schemeE(AnnCase).my_discr" (ppr l)

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
        bitmap_size = trunc16 $ d-s
        bitmap_size' :: Int
        bitmap_size' = fromIntegral bitmap_size
        bitmap = intsToReverseBitmap dflags bitmap_size'{-size-}
                        (sort (filter (< bitmap_size') rel_slots))
          where
          binds = Map.toList p
          -- NB: unboxed tuple cases bind the scrut binder to the same offset
          -- as one of the alt binders, so we have to remove any duplicates here:
          rel_slots = nub $ map fromIntegral $ concat (map spread binds)
          spread (id, offset) | isFollowableArg (bcIdArgRep id) = [ rel_offset ]
                              | otherwise                      = []
                where rel_offset = trunc16 $ d - fromIntegral offset - 1

     alt_stuff <- mapM codeAlt alts
     alt_final <- mkMultiBranch maybe_ncons alt_stuff

     let
         alt_bco_name = getName bndr
         alt_bco = mkProtoBCO dflags alt_bco_name alt_final (Left alts)
                       0{-no arity-} bitmap_size bitmap True{-is alts-}
--     trace ("case: bndr = " ++ showSDocDebug (ppr bndr) ++ "\ndepth = " ++ show d ++ "\nenv = \n" ++ showSDocDebug (ppBCEnv p) ++
--            "\n      bitmap = " ++ show bitmap) $ do

     scrut_code <- schemeE (d + ret_frame_sizeW + save_ccs_sizeW)
                           (d + ret_frame_sizeW + save_ccs_sizeW)
                           p scrut
     alt_bco' <- emitBc alt_bco
     let push_alts
            | isAlgCase = PUSH_ALTS alt_bco'
            | otherwise = PUSH_ALTS_UNLIFTED alt_bco' (typeArgRep bndr_ty)
     return (push_alts `consOL` scrut_code)


-- -----------------------------------------------------------------------------
-- Deal with a CCall.

-- Taggedly push the args onto the stack R->L,
-- deferencing ForeignObj#s and adjusting addrs to point to
-- payloads in Ptr/Byte arrays.  Then, generate the marshalling
-- (machine) code for the ccall, and create bytecodes to call that and
-- then return in the right way.

generateCCall :: Word -> Sequel         -- stack and sequel depths
              -> BCEnv
              -> CCallSpec              -- where to call
              -> Id                     -- of target, for type info
              -> [AnnExpr' Id DVarSet]   -- args (atoms)
              -> BcM BCInstrList

generateCCall d0 s p (CCallSpec target cconv safety) fn args_r_to_l
 = do
     dflags <- getDynFlags

     let
         -- useful constants
         addr_sizeW :: Word16
         addr_sizeW = fromIntegral (argRepSizeW dflags N)

         -- Get the args on the stack, with tags and suitably
         -- dereferenced for the CCall.  For each arg, return the
         -- depth to the first word of the bits for that arg, and the
         -- ArgRep of what was actually pushed.

         pargs _ [] = return []
         pargs d (a:az)
            = let [arg_ty] = repTypeArgs (exprType (deAnnotate' a))

              in case tyConAppTyCon_maybe arg_ty of
                    -- Don't push the FO; instead push the Addr# it
                    -- contains.
                    Just t
                     | t == arrayPrimTyCon || t == mutableArrayPrimTyCon
                       -> do rest <- pargs (d + fromIntegral addr_sizeW) az
                             code <- parg_ArrayishRep (fromIntegral (arrPtrsHdrSize dflags)) d p a
                             return ((code,AddrRep):rest)

                     | t == smallArrayPrimTyCon || t == smallMutableArrayPrimTyCon
                       -> do rest <- pargs (d + fromIntegral addr_sizeW) az
                             code <- parg_ArrayishRep (fromIntegral (smallArrPtrsHdrSize dflags)) d p a
                             return ((code,AddrRep):rest)

                     | t == byteArrayPrimTyCon || t == mutableByteArrayPrimTyCon
                       -> do rest <- pargs (d + fromIntegral addr_sizeW) az
                             code <- parg_ArrayishRep (fromIntegral (arrWordsHdrSize dflags)) d p a
                             return ((code,AddrRep):rest)

                    -- Default case: push taggedly, but otherwise intact.
                    _
                       -> do (code_a, sz_a) <- pushAtom d p a
                             rest <- pargs (d + fromIntegral sz_a) az
                             return ((code_a, atomPrimRep a) : rest)

         -- Do magic for Ptr/Byte arrays.  Push a ptr to the array on
         -- the stack but then advance it over the headers, so as to
         -- point to the payload.
         parg_ArrayishRep :: Word16 -> Word -> BCEnv -> AnnExpr' Id DVarSet
                          -> BcM BCInstrList
         parg_ArrayishRep hdrSize d p a
            = do (push_fo, _) <- pushAtom d p a
                 -- The ptr points at the header.  Advance it over the
                 -- header and then pretend this is an Addr#.
                 return (push_fo `snocOL` SWIZZLE 0 hdrSize)

     code_n_reps <- pargs d0 args_r_to_l
     let
         (pushs_arg, a_reps_pushed_r_to_l) = unzip code_n_reps
         a_reps_sizeW = fromIntegral (sum (map (primRepSizeW dflags) a_reps_pushed_r_to_l))

         push_args    = concatOL pushs_arg
         d_after_args = d0 + a_reps_sizeW
         a_reps_pushed_RAW
            | null a_reps_pushed_r_to_l || head a_reps_pushed_r_to_l /= VoidRep
            = panic "ByteCodeGen.generateCCall: missing or invalid World token?"
            | otherwise
            = reverse (tail a_reps_pushed_r_to_l)

         -- Now: a_reps_pushed_RAW are the reps which are actually on the stack.
         -- push_args is the code to do that.
         -- d_after_args is the stack depth once the args are on.

         -- Get the result rep.
         (returns_void, r_rep)
            = case maybe_getCCallReturnRep (idType fn) of
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
         maybe_static_target =
             case target of
                 DynamicTarget -> Nothing
                 StaticTarget _ _ _ False ->
                   panic "generateCCall: unexpected FFI value import"
                 StaticTarget _ target _ True ->
                   Just (MachLabel target mb_size IsFunction)
                   where
                      mb_size
                          | OSMinGW32 <- platformOS (targetPlatform dflags)
                          , StdCallConv <- cconv
                          = Just (fromIntegral a_reps_sizeW * wORD_SIZE dflags)
                          | otherwise
                          = Nothing

     let
         is_static = isJust maybe_static_target

         -- Get the arg reps, zapping the leading Addr# in the dynamic case
         a_reps --  | trace (showSDoc (ppr a_reps_pushed_RAW)) False = error "???"
                | is_static = a_reps_pushed_RAW
                | otherwise = if null a_reps_pushed_RAW
                              then panic "ByteCodeGen.generateCCall: dyn with no args"
                              else tail a_reps_pushed_RAW

         -- push the Addr#
         (push_Addr, d_after_Addr)
            | Just machlabel <- maybe_static_target
            = (toOL [PUSH_UBX machlabel addr_sizeW],
               d_after_args + fromIntegral addr_sizeW)
            | otherwise -- is already on the stack
            = (nilOL, d_after_args)

         -- Push the return placeholder.  For a call returning nothing,
         -- this is a V (tag).
         r_sizeW   = fromIntegral (primRepSizeW dflags r_rep)
         d_after_r = d_after_Addr + fromIntegral r_sizeW
         push_r    = (if   returns_void
                      then nilOL
                      else unitOL (PUSH_UBX (mkDummyLiteral r_rep) r_sizeW))

         -- generate the marshalling code we're going to call

         -- Offset of the next stack frame down the stack.  The CCALL
         -- instruction needs to describe the chunk of stack containing
         -- the ccall args to the GC, so it needs to know how large it
         -- is.  See comment in Interpreter.c with the CCALL instruction.
         stk_offset   = trunc16 $ d_after_r - s

         conv = case cconv of
           CCallConv -> FFICCall
           StdCallConv -> FFIStdCall
           _ -> panic "ByteCodeGen: unexpected calling convention"

     -- the only difference in libffi mode is that we prepare a cif
     -- describing the call type by calling libffi, and we attach the
     -- address of this to the CCALL instruction.


     let ffires = primRepToFFIType dflags r_rep
         ffiargs = map (primRepToFFIType dflags) a_reps
     hsc_env <- getHscEnv
     token <- ioToBc $ iservCmd hsc_env (PrepFFI conv ffiargs ffires)
     recordFFIBc token

     let
         -- do the call
         do_call      = unitOL (CCALL stk_offset token
                                 (fromIntegral (fromEnum (playInterruptible safety))))
         -- slide and return
         wrapup       = mkSLIDE r_sizeW (d_after_r - fromIntegral r_sizeW - s)
                        `snocOL` RETURN_UBX (toArgRep r_rep)
         --trace (show (arg1_offW, args_offW  ,  (map argRepSizeW a_reps) )) $
     return (
         push_args `appOL`
         push_Addr `appOL` push_r `appOL` do_call `appOL` wrapup
         )

primRepToFFIType :: DynFlags -> PrimRep -> FFIType
primRepToFFIType dflags r
  = case r of
     VoidRep     -> FFIVoid
     IntRep      -> signed_word
     WordRep     -> unsigned_word
     Int64Rep    -> FFISInt64
     Word64Rep   -> FFIUInt64
     AddrRep     -> FFIPointer
     FloatRep    -> FFIFloat
     DoubleRep   -> FFIDouble
     _           -> panic "primRepToFFIType"
  where
    (signed_word, unsigned_word)
       | wORD_SIZE dflags == 4  = (FFISInt32, FFIUInt32)
       | wORD_SIZE dflags == 8  = (FFISInt64, FFIUInt64)
       | otherwise              = panic "primTyDescChar"

-- Make a dummy literal, to be used as a placeholder for FFI return
-- values on the stack.
mkDummyLiteral :: PrimRep -> Literal
mkDummyLiteral pr
   = case pr of
        IntRep    -> MachInt 0
        WordRep   -> MachWord 0
        AddrRep   -> MachNullAddr
        DoubleRep -> MachDouble 0
        FloatRep  -> MachFloat 0
        Int64Rep  -> MachInt64 0
        Word64Rep -> MachWord64 0
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
       r_reps = repTypeArgs r_ty

       blargh :: a -- Used at more than one type
       blargh = pprPanic "maybe_getCCallReturn: can't handle:"
                         (pprType fn_ty)
     in
       case r_reps of
         [] -> panic "empty repTypeArgs"
         [ty]
           | typePrimRep ty == PtrRep
            -> blargh
           | isVoidTy ty
            -> Nothing
           | otherwise
            -> Just (typePrimRep ty)
                 -- if it was, it would be impossible to create a
                 -- valid return value placeholder on the stack
         _  -> blargh

maybe_is_tagToEnum_call :: AnnExpr' Id DVarSet -> Maybe (AnnExpr' Id DVarSet, [Name])
-- Detect and extract relevant info for the tagToEnum kludge.
maybe_is_tagToEnum_call app
  | AnnApp (_, AnnApp (_, AnnVar v) (_, AnnType t)) arg <- app
  , Just TagToEnumOp <- isPrimOpId_maybe v
  = Just (snd arg, extract_constr_Names t)
  | otherwise
  = Nothing
  where
    extract_constr_Names ty
           | [rep_ty] <- repTypeArgs ty
           , Just tyc <- tyConAppTyCon_maybe rep_ty
           , isDataTyCon tyc
           = map (getName . dataConWorkId) (tyConDataCons tyc)
           -- NOTE: use the worker name, not the source name of
           -- the DataCon.  See DataCon.hs for details.
           | otherwise
           = pprPanic "maybe_is_tagToEnum_call.extract_constr_Ids" (ppr ty)

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
Interpreter.c.  We don't acutally need an info-table here; we just
need to have the argument to be one-from-top on the stack, hence pushing
a 1-word null. See Trac #8383.
-}


implement_tagToId :: Word -> Sequel -> BCEnv
                  -> AnnExpr' Id DVarSet -> [Name] -> BcM BCInstrList
-- See Note [Implementing tagToEnum#]
implement_tagToId d s p arg names
  = ASSERT( notNull names )
    do (push_arg, arg_words) <- pushAtom d p arg
       labels <- getLabelsBc (genericLength names)
       label_fail <- getLabelBc
       label_exit <- getLabelBc
       let infos = zip4 labels (tail labels ++ [label_fail])
                               [0 ..] names
           steps = map (mkStep label_exit) infos

       return (push_arg
               `appOL` unitOL (PUSH_UBX MachNullAddr 1)
                   -- Push bogus word (see Note [Implementing tagToEnum#])
               `appOL` concatOL steps
               `appOL` toOL [ LABEL label_fail, CASEFAIL,
                              LABEL label_exit ]
                `appOL` mkSLIDE 1 (d - s + fromIntegral arg_words + 1)
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

pushAtom :: Word -> BCEnv -> AnnExpr' Id DVarSet -> BcM (BCInstrList, Word16)

pushAtom d p e
   | Just e' <- bcView e
   = pushAtom d p e'

pushAtom _ _ (AnnCoercion {})   -- Coercions are zero-width things,
   = return (nilOL, 0)          -- treated just like a variable V

-- See Note [Empty case alternatives] in coreSyn/CoreSyn.hs
-- and Note [Bottoming expressions] in coreSyn/CoreUtils.hs:
-- The scrutinee of an empty case evaluates to bottom
pushAtom d p (AnnCase (_, a) _ _ []) -- trac #12128
   = pushAtom d p a

pushAtom d p (AnnVar v)
   | [rep_ty] <- repTypeArgs (idType v)
   , V <- typeArgRep rep_ty
   = return (nilOL, 0)

   | isFCallId v
   = pprPanic "pushAtom: shouldn't get an FCallId here" (ppr v)

   | Just primop <- isPrimOpId_maybe v
   = return (unitOL (PUSH_PRIMOP primop), 1)

   | Just d_v <- lookupBCEnv_maybe v p  -- v is a local variable
   = do dflags <- getDynFlags
        let sz :: Word16
            sz = fromIntegral (idSizeW dflags v)
            l = trunc16 $ d - d_v + fromIntegral sz - 2
        return (toOL (genericReplicate sz (PUSH_L l)), sz)
         -- d - d_v                 the number of words between the TOS
         --                         and the 1st slot of the object
         --
         -- d - d_v - 1             the offset from the TOS of the 1st slot
         --
         -- d - d_v - 1 + sz - 1    the offset from the TOS of the last slot
         --                         of the object.
         --
         -- Having found the last slot, we proceed to copy the right number of
         -- slots on to the top of the stack.

   | otherwise  -- v must be a global variable
   = do dflags <- getDynFlags
        let sz :: Word16
            sz = fromIntegral (idSizeW dflags v)
        MASSERT(sz == 1)
        return (unitOL (PUSH_G (getName v)), sz)

pushAtom _ _ (AnnConApp dc args) = do
    MASSERT( all isAnnTypeArg args )
    dflags <- getDynFlags
    let v = dataConWorkId dc
    let sz :: Word16
        sz = fromIntegral (idSizeW dflags v)
    MASSERT(sz == 1)
    return (unitOL (PUSH_G (getName v)), sz)



pushAtom _ _ (AnnLit lit) = do
     dflags <- getDynFlags
     let code rep
             = let size_host_words = fromIntegral (argRepSizeW dflags rep)
               in  return (unitOL (PUSH_UBX lit size_host_words),
                           size_host_words)

     case lit of
        MachLabel _ _ _ -> code N
        MachWord _    -> code N
        MachInt _     -> code N
        MachWord64 _  -> code L
        MachInt64 _   -> code L
        MachFloat _   -> code F
        MachDouble _  -> code D
        MachChar _    -> code N
        MachNullAddr  -> code N
        MachStr _     -> code N
        -- No LitInteger's should be left by the time this is called.
        -- CorePrep should have converted them all to a real core
        -- representation.
        LitInteger {} -> panic "pushAtom: LitInteger"

pushAtom _ _ expr
   = pprPanic "ByteCodeGen.pushAtom"
              (pprCoreExpr (deAnnotate' expr))


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


lookupBCEnv_maybe :: Id -> BCEnv -> Maybe Word
lookupBCEnv_maybe = Map.lookup

idSizeW :: DynFlags -> Id -> Int
idSizeW dflags = argRepSizeW dflags . bcIdArgRep

bcIdArgRep :: Id -> ArgRep
bcIdArgRep = toArgRep . bcIdPrimRep

bcIdPrimRep :: Id -> PrimRep
bcIdPrimRep = typePrimRep . bcIdUnaryType

isFollowableArg :: ArgRep -> Bool
isFollowableArg P = True
isFollowableArg _ = False

isVoidArg :: ArgRep -> Bool
isVoidArg V = True
isVoidArg _ = False

bcIdUnaryType :: Id -> UnaryType
bcIdUnaryType x = case repTypeArgs (idType x) of
    [rep_ty] -> rep_ty
    _ -> pprPanic "bcIdUnaryType" (ppr x $$ ppr (idType x))

-- See bug #1257
multiValException :: a
multiValException = throwGhcException (ProgramError
  ("Error: bytecode compiler can't handle unboxed tuples and sums.\n"++
   "  Possibly due to foreign import/export decls in source.\n"++
   "  Workaround: use -fobject-code, or compile this module to .o separately."))

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

mkSLIDE :: Word16 -> Word -> OrdList BCInstr
mkSLIDE n d
    -- if the amount to slide doesn't fit in a word,
    -- generate multiple slide instructions
    | d > fromIntegral limit
    = SLIDE n limit `consOL` mkSLIDE n (d - fromIntegral limit)
    | d == 0
    = nilOL
    | otherwise
    = if d == 0 then nilOL else unitOL (SLIDE n $ fromIntegral d)
    where
        limit :: Word16
        limit = maxBound

splitApp :: AnnExpr' Var ann -> (AnnExpr' Var ann, [AnnExpr' Var ann])
        -- The arguments are returned in *right-to-left* order
splitApp e | Just e' <- bcView e = splitApp e'
splitApp (AnnApp (_,f) (_,a))    = case splitApp f of
                                      (f', as) -> (f', a:as)
splitApp e                       = (e, [])


bcView :: AnnExpr' Var ann -> Maybe (AnnExpr' Var ann)
-- The "bytecode view" of a term discards
--  a) type abstractions
--  b) type applications
--  c) casts
--  d) ticks (but not breakpoints)
-- Type lambdas *can* occur in random expressions,
-- whereas value lambdas cannot; that is why they are nuked here
-- TODO #12618: what to do with data con apps here? Keep types or not?
bcView (AnnCast (_,e) _)             = Just e
bcView (AnnLam v (_,e)) | isTyVar v  = Just e
bcView (AnnApp (_,e) (_, AnnType _)) = Just e
bcView (AnnTick Breakpoint{} _)      = Nothing
bcView (AnnTick _other_tick (_,e))   = Just e
bcView _                             = Nothing

isVAtom :: AnnExpr' Var ann -> Bool
isVAtom e | Just e' <- bcView e = isVAtom e'
isVAtom (AnnVar v)              = isVoidArg (bcIdArgRep v)
isVAtom (AnnCoercion {})        = True
isVAtom _                     = False

atomPrimRep :: AnnExpr' Id ann -> PrimRep
atomPrimRep e | Just e' <- bcView e = atomPrimRep e'
atomPrimRep (AnnVar v)              = bcIdPrimRep v
atomPrimRep (AnnConApp dc args)     = ASSERT (all isAnnTypeArg args)
                                      bcIdPrimRep (dataConWorkId dc)
atomPrimRep (AnnLit l)              = typePrimRep (literalType l)

-- Trac #12128:
-- A case expresssion can be an atom because empty cases evaluate to bottom.
-- See Note [Empty case alternatives] in coreSyn/CoreSyn.hs
atomPrimRep (AnnCase _ _ ty _)      = ASSERT(typePrimRep ty == PtrRep) PtrRep
atomPrimRep (AnnCoercion {})        = VoidRep
atomPrimRep other = pprPanic "atomPrimRep" (ppr (deAnnotate' other))

atomRep :: AnnExpr' Id ann -> ArgRep
atomRep e = toArgRep (atomPrimRep e)

isPtrAtom :: AnnExpr' Id ann -> Bool
isPtrAtom e = isFollowableArg (atomRep e)

-- Let szsw be the sizes in words of some items pushed onto the stack,
-- which has initial depth d'.  Return the values which the stack environment
-- should map these items to.
mkStackOffsets :: Word -> [Word] -> [Word]
mkStackOffsets original_depth szsw
   = map (subtract 1) (tail (scanl (+) original_depth szsw))

typeArgRep :: Type -> ArgRep
typeArgRep = toArgRep . typePrimRep

-- -----------------------------------------------------------------------------
-- The bytecode generator's monad

data BcM_State
   = BcM_State
        { bcm_hsc_env :: HscEnv
        , uniqSupply  :: UniqSupply      -- for generating fresh variable names
        , thisModule  :: Module          -- current module (for breakpoints)
        , nextlabel   :: Word16          -- for generating local labels
        , ffis        :: [FFIInfo]       -- ffi info blocks, to free later
                                         -- Should be free()d when it is GCd
        , modBreaks   :: Maybe ModBreaks -- info about breakpoints
        , breakInfo   :: IntMap CgBreakInfo
        }

newtype BcM r = BcM (BcM_State -> IO (BcM_State, r))

ioToBc :: IO a -> BcM a
ioToBc io = BcM $ \st -> do
  x <- io
  return (st, x)

runBc :: HscEnv -> UniqSupply -> Module -> Maybe ModBreaks -> BcM r
      -> IO (BcM_State, r)
runBc hsc_env us this_mod modBreaks (BcM m)
   = m (BcM_State hsc_env us this_mod 0 [] modBreaks IntMap.empty)

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

instance Functor BcM where
    fmap = liftM

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

emitBc :: ([FFIInfo] -> ProtoBCO Name) -> BcM (ProtoBCO Name)
emitBc bco
  = BcM $ \st -> return (st{ffis=[]}, bco (ffis st))

recordFFIBc :: RemotePtr C_ffi_cif -> BcM ()
recordFFIBc a
  = BcM $ \st -> return (st{ffis = FFIInfo a : ffis st}, ())

getLabelBc :: BcM Word16
getLabelBc
  = BcM $ \st -> do let nl = nextlabel st
                    when (nl == maxBound) $
                        panic "getLabelBc: Ran out of labels"
                    return (st{nextlabel = nl + 1}, nl)

getLabelsBc :: Word16 -> BcM [Word16]
getLabelsBc n
  = BcM $ \st -> let ctr = nextlabel st
                 in return (st{nextlabel = ctr+n}, [ctr .. ctr+n-1])

getCCArray :: BcM (Array BreakIndex (RemotePtr CostCentre))
getCCArray = BcM $ \st ->
  let breaks = expectJust "ByteCodeGen.getCCArray" $ modBreaks st in
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

newId :: Type -> BcM Id
newId ty = do
    uniq <- newUnique
    return $ mkSysLocal tickFS uniq ty

tickFS :: FastString
tickFS = fsLit "ticked"
