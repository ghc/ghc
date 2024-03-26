{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.StgToJS.Expr
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Jeffrey Young  <jeffrey.young@iohk.io>
--                Luite Stegeman <luite.stegeman@iohk.io>
--                Sylvain Henry  <sylvain.henry@iohk.io>
--                Josh Meredith  <josh.meredith@iohk.io>
-- Stability   :  experimental
--
--  Code generation of Expressions
-----------------------------------------------------------------------------

module GHC.StgToJS.Expr
  ( genExpr
  , genEntryType
  , loadLiveFun
  , genStaticRefsRhs
  , genStaticRefs
  , genBody
  )
where

import GHC.Prelude

import GHC.JS.Unsat.Syntax
import GHC.JS.Transform
import GHC.JS.Make

import GHC.StgToJS.Apply
import GHC.StgToJS.Arg
import GHC.StgToJS.Closure
import GHC.StgToJS.ExprCtx
import GHC.StgToJS.FFI
import GHC.StgToJS.Heap
import GHC.StgToJS.Monad
import GHC.StgToJS.DataCon
import GHC.StgToJS.Types
import GHC.StgToJS.Literal
import GHC.StgToJS.Prim
import GHC.StgToJS.Profiling
import GHC.StgToJS.Regs
import GHC.StgToJS.Utils
import GHC.StgToJS.Stack
import GHC.StgToJS.Ids

import GHC.Types.CostCentre
import GHC.Types.Tickish
import GHC.Types.Var.Set
import GHC.Types.Id
import GHC.Types.Unique.FM
import GHC.Types.RepType
import GHC.Types.Literal

import GHC.Stg.Syntax
import GHC.Stg.Utils

import GHC.Builtin.PrimOps
import GHC.Builtin.Names

import GHC.Core
import GHC.Core.TyCon
import GHC.Core.DataCon
import GHC.Core.Opt.Arity (isOneShotBndr)
import GHC.Core.Type hiding (typeSize)

import GHC.Utils.Misc
import GHC.Utils.Encoding
import GHC.Utils.Monad
import GHC.Utils.Panic
import GHC.Utils.Outputable (ppr, renderWithContext, defaultSDocContext)
import qualified Control.Monad.Trans.State.Strict as State
import GHC.Data.FastString
import qualified GHC.Data.List.SetOps as ListSetOps

import Data.Monoid
import Data.Maybe
import Data.Function
import Data.Either
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad
import Control.Arrow ((&&&))

-- | Evaluate an expression in the given expression context (continuation)
genExpr :: HasDebugCallStack => ExprCtx -> CgStgExpr -> G (JStat, ExprResult)
genExpr ctx stg = case stg of
  StgApp f args -> genApp ctx f args
  StgLit l      -> do
    ls <- genLit l
    let r = assignToExprCtx ctx ls
    pure (r,ExprInline Nothing)
  StgConApp con _n args _ -> do
    as <- concatMapM genArg args
    c <- genCon ctx con as
    return (c, ExprInline (Just as))
  StgOpApp (StgFCallOp f _) args t
    -> genForeignCall ctx f t (concatMap typex_expr $ ctxTarget ctx) args
  StgOpApp (StgPrimOp op) args t
    -> genPrimOp ctx op args t
  StgOpApp (StgPrimCallOp c) args t
    -> genPrimCall ctx c args t
  StgCase e b at alts
    -> genCase ctx b e at alts (liveVars $ stgExprLive False stg)
  StgLet _ b e -> do
    (b',ctx') <- genBind ctx b
    (s,r)     <- genExpr ctx' e
    return (b' <> s, r)
  StgLetNoEscape _ b e -> do
    (b', ctx') <- genBindLne ctx b
    (s, r)     <- genExpr ctx' e
    return (b' <> s, r)
  StgTick (ProfNote cc count scope) e -> do
    setSCCstats <- ifProfilingM $ setCC cc count scope
    (stats, result) <- genExpr ctx e
    return (setSCCstats <> stats, result)
  StgTick (SourceNote span _sname) e
    -> genExpr (ctxSetSrcSpan span ctx) e
  StgTick _m e
    -> genExpr ctx e

-- | regular let binding: allocate heap object
genBind :: HasDebugCallStack
        => ExprCtx
        -> CgStgBinding
        -> G (JStat, ExprCtx)
genBind ctx bndr =
  case bndr of
    StgNonRec b r -> do
       j <- assign b r >>= \case
         Just ja -> return ja
         Nothing -> allocCls Nothing [(b,r)]
       return (j, addEvalRhs ctx [(b,r)])
    StgRec bs     -> do
       jas <- mapM (uncurry assign) bs -- fixme these might depend on parts initialized by allocCls
       let m = if null jas then Nothing else Just (mconcat $ catMaybes jas)
       j <- allocCls m . map snd . filter (isNothing . fst) $ zip jas bs
       return (j, addEvalRhs ctx bs)
   where
     ctx' = ctxClearLneFrame ctx

     assign :: Id -> CgStgRhs -> G (Maybe JStat)
     assign b (StgRhsClosure _ _ccs {-[the_fv]-} _upd [] expr _typ)
       | let strip = snd . stripStgTicksTop (not . tickishIsCode)
       , StgCase (StgApp scrutinee []) _ (AlgAlt _) [GenStgAlt (DataAlt _) params sel_expr] <- strip expr
       , StgApp selectee [] <- strip sel_expr
       , let params_w_offsets = zip params (L.scanl' (+) 1 $ map (typeSize . idType) params)
       , let total_size = sum (map (typeSize . idType) params)
       -- , the_fv == scrutinee -- fixme check
       , Just the_offset <- ListSetOps.assocMaybe params_w_offsets selectee
       , the_offset <= 16 -- fixme make this some configurable constant
       = do
           let the_fv = scrutinee -- error "the_fv" -- fixme
           let sel_tag | the_offset == 2 = if total_size == 2 then "2a"
                                                              else "2b"
                       | otherwise       = show the_offset
           tgts <- identsForId b
           the_fvjs <- varsForId the_fv
           case (tgts, the_fvjs) of
             ([tgt], [the_fvj]) -> return $ Just
               (tgt ||= ApplExpr (var ("h$c_sel_" <> mkFastString sel_tag)) [the_fvj])
             _ -> panic "genBind.assign: invalid size"
     assign b (StgRhsClosure _ext _ccs _upd [] expr _typ)
       | snd (isInlineExpr (ctxEvaluatedIds ctx) expr) = do
           d   <- declVarsForId b
           tgt <- varsForId b
           let ctx' = ctx { ctxTarget = assocIdExprs b tgt }
           (j, _) <- genExpr ctx' expr
           return (Just (d <> j))
     assign _b StgRhsCon{} = return Nothing
     assign  b r           = genEntry ctx' b r >> return Nothing

     addEvalRhs c [] = c
     addEvalRhs c ((b,r):xs)
       | StgRhsCon{} <- r                         = addEvalRhs (ctxAssertEvaluated b c) xs
       | (StgRhsClosure _ _ ReEntrant _ _ _) <- r = addEvalRhs (ctxAssertEvaluated b c) xs
       | otherwise                                = addEvalRhs c xs

genBindLne :: HasDebugCallStack
           => ExprCtx
           -> CgStgBinding
           -> G (JStat, ExprCtx)
genBindLne ctx bndr = do
  -- compute live variables and the offsets where they will be stored in the
  -- stack
  vis  <- map (\(x,y,_) -> (x,y)) <$>
            optimizeFree oldFrameSize (newLvs++map fst updBinds)
  -- initialize updatable bindings to null_
  declUpds <- mconcat <$> mapM (fmap (||= null_) . identForId . fst) updBinds
  -- update expression context to include the updated LNE frame
  let ctx' = ctxUpdateLneFrame vis bound ctx
  mapM_ (uncurry $ genEntryLne ctx') binds
  return (declUpds, ctx')
  where
    oldFrameSize = ctxLneFrameSize ctx
    isOldLv i    = ctxIsLneBinding ctx i ||
                   ctxIsLneLiveVar ctx i
    live         = liveVars $ mkDVarSet $ stgLneLive' bndr
    newLvs       = filter (not . isOldLv) (dVarSetElems live)
    binds = case bndr of
              StgNonRec b e -> [(b,e)]
              StgRec    bs  -> bs
    bound = map fst binds
    (updBinds, _nonUpdBinds) = L.partition (isUpdatableRhs . snd) binds

-- | Generate let-no-escape entry
--
-- Let-no-escape entries live on the stack. There is no heap object associated with them.
--
-- A let-no-escape entry is called like a normal stack frame, although as an optimization,
-- `Stack`[`Sp`] is not set when making the call. This is done later if the
-- thread needs to be suspended.
--
-- Updatable let-no-escape binders have one 'private' slot in the stack frame. This slot
-- is initially set to null, changed to h$blackhole when the thunk is being evaluated.
--
genEntryLne :: HasDebugCallStack => ExprCtx -> Id -> CgStgRhs -> G ()
genEntryLne ctx i rhs@(StgRhsClosure _ext _cc update args body typ) =
  resetSlots $ do
  let payloadSize = ctxLneFrameSize ctx
      vars        = ctxLneFrameVars ctx
      myOffset    =
        maybe (panic "genEntryLne: updatable binder not found in let-no-escape frame")
              ((payloadSize-) . fst)
              (L.find ((==i) . fst . snd) (zip [0..] vars))
      bh | isUpdatable update =
             jVar (\x -> mconcat
              [ x |= ApplExpr (var "h$bh_lne") [Sub sp (toJExpr myOffset), toJExpr (payloadSize+1)]
              , IfStat x (ReturnStat x) mempty
              ])
         | otherwise = mempty
  lvs  <- popLneFrame True payloadSize ctx
  body <- genBody ctx R1 args body typ
  ei@(TxtI eii) <- identForEntryId i
  sr   <- genStaticRefsRhs rhs
  let f = (bh <> lvs <> body)
  emitClosureInfo $
    ClosureInfo ei
                (CIRegs 0 $ concatMap idVt args)
                (eii <> ", " <> mkFastString (renderWithContext defaultSDocContext (ppr i)))
                (fixedLayout . reverse $
                    map (stackSlotType . fst) (ctxLneFrameVars ctx))
                CIStackFrame
                sr
  emitToplevel (jFunction ei [] f)
genEntryLne ctx i (StgRhsCon cc con _mu _ticks args _typ) = resetSlots $ do
  let payloadSize = ctxLneFrameSize ctx
  ei@(TxtI _eii) <- identForEntryId i
  -- di <- varForDataConWorker con
  ii <- freshIdent
  p  <- popLneFrame True payloadSize ctx
  args' <- concatMapM genArg args
  ac    <- allocCon ii con cc args'
  emitToplevel (jFunction ei [] (mconcat [decl ii, p, ac, r1 |= toJExpr ii, returnStack]))

-- | Generate the entry function for a local closure
genEntry :: HasDebugCallStack => ExprCtx -> Id -> CgStgRhs -> G ()
genEntry _ _i StgRhsCon {} = return ()
genEntry ctx i rhs@(StgRhsClosure _ext cc {-_bi live-} upd_flag args body typ) = resetSlots $ do
  let live = stgLneLiveExpr rhs -- error "fixme" -- probably find live vars in body
  ll    <- loadLiveFun live
  llv   <- verifyRuntimeReps live
  upd   <- genUpdFrame upd_flag i
  body  <- genBody entryCtx R2 args body typ
  ei@(TxtI eii) <- identForEntryId i
  et    <- genEntryType args
  setcc <- ifProfiling $
             if et == CIThunk
               then enterCostCentreThunk
               else enterCostCentreFun cc
  sr <- genStaticRefsRhs rhs
  emitClosureInfo $ ClosureInfo ei
                                (CIRegs 0 $ PtrV : concatMap idVt args)
                                (eii <> ", " <> mkFastString (renderWithContext defaultSDocContext (ppr i)))
                                (fixedLayout $ map (uTypeVt . idType) live)
                                et
                                sr
  emitToplevel (jFunction ei [] (mconcat [ll, llv, upd, setcc, body]))
  where
    entryCtx = ctxSetTarget [] (ctxClearLneFrame ctx)

-- | Generate the entry function types for identifiers. Note that this only
-- returns either 'CIThunk' or 'CIFun'. Everything else (PAP Blackhole etc.) is
-- filtered as not a RuntimeRepKinded type.
genEntryType :: HasDebugCallStack => [Id] -> G CIType
genEntryType []   = return CIThunk
genEntryType args0 = do
  args' <- mapM genIdArg args
  return $ CIFun (length args) (length $ concat args')
  where
    args = filter (not . isRuntimeRepKindedTy . idType) args0

-- | Generate the body of an object
genBody :: HasDebugCallStack
         => ExprCtx
         -> StgReg
         -> [Id]
         -> CgStgExpr
         -> Type
         -> G JStat
genBody ctx startReg args e typ = do
  -- load arguments into local variables
  la <- do
    args' <- concatMapM genIdArgI args
    return (declAssignAll args' (fmap toJExpr [startReg..]))

  -- assert that arguments have valid runtime reps
  lav <- verifyRuntimeReps args

  -- compute PrimReps and their number of slots required to return the result of
  -- i applied to args.
  let res_vars = resultSize typ

  -- compute typed expressions for each slot and assign registers
  let go_var regs = \case
        []              -> []
        ((rep,size):rs) ->
          let !(regs0,regs1) = splitAt size regs
              !ts = go_var regs1 rs
          in TypedExpr rep regs0 : ts

  let tgt  = go_var jsRegsFromR1 res_vars
  let !ctx' = ctx { ctxTarget = tgt }

  -- generate code for the expression
  (e, _r) <- genExpr ctx' e

  return $ la <> lav <> e <> returnStack

-- | Find the result type after applying the function to the arguments
--
-- It's trickier than it looks because:
--
-- 1. we don't have the Arity of the Id. The following functions return
-- different values in some cases:
--    - idArity
--    - typeArity . idType
--    - idFunRepArity
--    - typeArity . unwrapType . idType
-- Moreover the number of args may be different than all of these arities
--
-- 2. sometimes the type is Any, perhaps after some unwrapping. For example
-- HappyAbsSyn is a newtype around HappyAny which is Any or (forall a. a).
--
-- Se we're left to use the applied arguments to peel the type (unwrapped) one
-- arg at a time. But passed args are args after unarisation so we need to
-- unarise every argument type that we peel (using typePrimRepArgs) to get the
-- number of passed args consumed by each type arg.
--
-- In case of failure to determine the type, we default to LiftedRep as it's
-- probably what it is.
--
resultSize :: HasDebugCallStack => Type -> [(PrimRep, Int)]
resultSize ty = result
  where
    result       = result_reps `zip` result_slots
    result_slots = fmap (slotCount . primRepSize) result_reps
    result_reps  = typePrimRep ty

-- | Ensure that the set of identifiers has valid 'RuntimeRep's. This function
-- returns a no-op when 'csRuntimeAssert' in 'StgToJSConfig' is False.
verifyRuntimeReps :: HasDebugCallStack => [Id] -> G JStat
verifyRuntimeReps xs = do
  runtime_assert <- csRuntimeAssert <$> getSettings
  if not runtime_assert
    then pure mempty
    else mconcat <$> mapM verifyRuntimeRep xs
  where
    verifyRuntimeRep i = do
      i' <- varsForId i
      pure $ go i' (idVt i)
    go js         (VoidV:vs) = go js vs
    go (j1:j2:js) (LongV:vs) = v "h$verify_rep_long" [j1,j2] <> go js vs
    go (j1:j2:js) (AddrV:vs) = v "h$verify_rep_addr" [j1,j2] <> go js vs
    go (j:js)     (v:vs)     = ver j v                       <> go js vs
    go []         []         = mempty
    go _          _          = pprPanic "verifyRuntimeReps: inconsistent sizes" (ppr xs)
    ver j PtrV    = v "h$verify_rep_heapobj" [j]
    ver j IntV    = v "h$verify_rep_int"     [j]
    ver j DoubleV = v "h$verify_rep_double"  [j]
    ver j ArrV    = v "h$verify_rep_arr"     [j]
    ver _ _       = mempty
    v f as = ApplStat (var f) as

-- | Given a set of 'Id's, bind each 'Id' to the appropriate data fields in N
-- registers. This assumes these data fields have already been populated in the
-- registers. For the empty, singleton, and binary case use register 1, for any
-- more use as many registers as necessary.
loadLiveFun :: [Id] -> G JStat
loadLiveFun l = do
   l' <- concat <$> mapM identsForId l
   case l' of
     []  -> return mempty
     -- set the ident to d1 field of register 1
     [v] -> return (v ||= r1 .^ closureField1_)
     -- set the idents to d1 and d2 fields of register 1
     [v1,v2] -> return $ mconcat
                        [ v1 ||= r1 .^ closureField1_
                        , v2 ||= r1 .^ closureField2_
                        ]
     -- and so on
     (v:vs)  -> do
       d <- freshIdent
       let l'' = mconcat . zipWith (loadLiveVar $ toJExpr d) [(1::Int)..] $ vs
       return $ mconcat
               [ v ||= r1 .^ closureField1_
               , d ||= r1 .^ closureField2_
               , l''
               ]
  where
        loadLiveVar d n v = let ident = TxtI (dataFieldName n)
                            in  v ||= SelExpr d ident

-- | Pop a let-no-escape frame off the stack
popLneFrame :: Bool -> Int -> ExprCtx -> G JStat
popLneFrame inEntry size ctx = do
  -- calculate the new stack size
  let ctx' = ctxLneShrinkStack ctx size

  let gen_id_slot (i,n) = do
        ids <- identsForId i
        let !id_n = ids !! (n-1)
        pure (id_n, SlotId i n)

  is <- mapM gen_id_slot (ctxLneFrameVars ctx')

  let skip = if inEntry then 1 else 0 -- pop the frame header
  popSkipI skip is

-- | Generate an updated given an 'Id'
genUpdFrame :: UpdateFlag -> Id -> G JStat
genUpdFrame u i
  | isReEntrant u   = pure mempty
  | isOneShotBndr i = maybeBh
  | isUpdatable u   = updateThunk
  | otherwise       = maybeBh
  where
    isReEntrant ReEntrant = True
    isReEntrant _         = False
    maybeBh = do
      settings <- getSettings
      assertRtsStat (return $ bhSingleEntry settings)

-- | Blackhole single entry
--
-- Overwrite a single entry object with a special thunk that behaves like a
-- black hole (throws a JS exception when entered) but pretends to be a thunk.
-- Useful for making sure that the object is not accidentally entered multiple
-- times
--
bhSingleEntry :: StgToJSConfig -> JStat
bhSingleEntry _settings = mconcat
  [ r1 .^ closureEntry_  |= var "h$blackholeTrap"
  , r1 .^ closureField1_ |= undefined_
  , r1 .^ closureField2_ |= undefined_
  ]

genStaticRefsRhs :: CgStgRhs -> G CIStatic
genStaticRefsRhs lv = genStaticRefs (stgRhsLive lv)

-- fixme, update to new way to compute static refs dynamically
genStaticRefs :: LiveVars -> G CIStatic
genStaticRefs lv
  | isEmptyDVarSet sv = return (CIStaticRefs [])
  | otherwise         = do
      unfloated <- State.gets gsUnfloated
      let xs = filter (\x -> not (elemUFM x unfloated ||
                                  definitelyUnliftedType (idType x)))
                      (dVarSetElems sv)
      CIStaticRefs . catMaybes <$> mapM getStaticRef xs
  where
    sv = liveStatic lv

    getStaticRef :: Id -> G (Maybe FastString)
    getStaticRef = fmap (fmap itxt . listToMaybe) . identsForId

-- | Reorder the things we need to push to reuse existing stack values as much
-- as possible True if already on the stack at that location
optimizeFree
  :: HasDebugCallStack
  => Int
  -> [Id]
  -> G [(Id,Int,Bool)] -- ^ A list of stack slots.
                       -- -- Id: stored on the slot
                       -- -- Int: the part of the value that is stored
                       -- -- Bool: True when the slot already contains a value
optimizeFree offset ids = do
  -- this line goes wrong                               vvvvvvv
  let -- ids' = concat $ map (\i -> map (i,) [1..varSize . uTypeVt . idType $ i]) ids
      idSize :: Id -> Int
      idSize i = sum $ map varSize (typeVt . idType $ i)
      ids' = concatMap (\i -> map (i,) [1..idSize i]) ids
      -- 1..varSize] . uTypeVt . idType $ i]) (typeVt ids)
      l    = length ids'
  slots <- drop offset . take l . (++repeat SlotUnknown) <$> getSlots
  let slm                = M.fromList (zip slots [0..])
      (remaining, fixed) = partitionEithers $
         map (\inp@(i,n) -> maybe (Left inp) (\j -> Right (i,n,j,True))
            (M.lookup (SlotId i n) slm)) ids'
      takenSlots         = S.fromList (fmap (\(_,_,x,_) -> x) fixed)
      freeSlots          = filter (`S.notMember` takenSlots) [0..l-1]
      remaining'         = zipWith (\(i,n) j -> (i,n,j,False)) remaining freeSlots
      allSlots           = L.sortBy (compare `on` \(_,_,x,_) -> x) (fixed ++ remaining')
  return $ map (\(i,n,_,b) -> (i,n,b)) allSlots

-- | Allocate local closures
allocCls :: Maybe JStat -> [(Id, CgStgRhs)] -> G JStat
allocCls dynMiddle xs = do
   (stat, dyn) <- partitionEithers <$> mapM toCl xs
   ac <- allocDynAll True dynMiddle dyn
   pure (mconcat stat <> ac)
  where
    -- left = static, right = dynamic
    toCl :: (Id, CgStgRhs)
         -> G (Either JStat (Ident,JExpr,[JExpr],CostCentreStack))
    -- statics
    {- making zero-arg constructors static is problematic, see #646
       proper candidates for this optimization should have been floated
       already
      toCl (i, StgRhsCon cc con []) = do
      ii <- identForId i
      Left <$> (return (decl ii) <> allocCon ii con cc []) -}
    toCl (i, StgRhsCon cc con _mui _ticjs [a] _typ) | isUnboxableCon con = do
      ii <- identForId i
      ac <- allocCon ii con cc =<< genArg a
      pure (Left (decl ii <> ac))

    -- dynamics
    toCl (i, StgRhsCon cc con _mu _ticks ar _typ) =
      -- fixme do we need to handle unboxed?
      Right <$> ((,,,) <$> identForId i
                       <*> varForDataConWorker con
                       <*> concatMapM genArg ar
                       <*> pure cc)
    toCl (i, cl@(StgRhsClosure _ext cc _upd_flag _args _body _typ)) =
      let live = stgLneLiveExpr cl
      in  Right <$> ((,,,) <$> identForId i
                       <*> varForEntryId i
                       <*> concatMapM varsForId live
                       <*> pure cc)

-- fixme CgCase has a reps_compatible check here
-- | Consume Stg case statement and generate a case statement. See also
-- 'genAlts'
genCase :: HasDebugCallStack
        => ExprCtx
        -> Id
        -> CgStgExpr
        -> AltType
        -> [CgStgAlt]
        -> LiveVars
        -> G (JStat, ExprResult)
genCase ctx bnd e at alts l
  -- For:      unpackCStringAppend# "some string"# str
  -- Generate: h$appendToHsStringA(str, "some string")
  --
  -- The latter has a faster decoding loop.
  --
  -- Since #23270 and 7e0c8b3bab30, literals strings aren't STG atoms and we
  -- need to match the following instead:
  --
  --    case "some string"# of b {
  --      DEFAULT -> unpackCStringAppend# b str
  --    }
  --
  -- Wrinkle: it doesn't kick in when literals are floated out to the top level.
  --
  | StgLit (LitString bs) <- e
  , [GenStgAlt DEFAULT _ rhs] <- alts
  , StgApp i args <- rhs
  , getUnique i == unpackCStringAppendIdKey
  , [StgVarArg b',x] <- args
  , bnd == b'
  , d <- utf8DecodeByteString bs
  , [top] <- concatMap typex_expr (ctxTarget ctx)
  = do
      prof <- csProf <$> getSettings
      let profArg = if prof then [jCafCCS] else []
      a <- genArg x
      return ( top |= app "h$appendToHsStringA" (toJExpr d : a ++ profArg)
             , ExprInline Nothing
             )

  | snd $ isInlineExpr (ctxEvaluatedIds ctx) e = do
      bndi <- identsForId bnd
      let ctx' = ctxSetTop bnd
                  $ ctxSetTarget (assocIdExprs bnd (map toJExpr bndi))
                  $ ctx
      (ej, r) <- genExpr ctx' e
      let d = case r of
                ExprInline d0 -> d0
                ExprCont -> pprPanic "genCase: expression was not inline"
                                     (pprStgExpr panicStgPprOpts e)

      (aj, ar) <- genAlts (ctxAssertEvaluated bnd ctx) bnd at d alts
      (saveCCS,restoreCCS) <- ifProfilingM $ do
        ccsVar <- freshIdent
        pure ( ccsVar ||= toJExpr jCurrentCCS
             , toJExpr jCurrentCCS |= toJExpr ccsVar
             )
      return ( mconcat
          [ mconcat (map decl bndi)
          , saveCCS
          , ej
          , restoreCCS
          , aj
          ]
        , ar
         )
  | otherwise = do
      rj       <- genRet (ctxAssertEvaluated bnd ctx) bnd at alts l
      let ctx' = ctxSetTop bnd
                  $ ctxSetTarget (assocIdExprs bnd (map toJExpr [R1 ..]))
                  $ ctx
      (ej, _r) <- genExpr ctx' e
      return (rj <> ej, ExprCont)

genRet :: HasDebugCallStack
       => ExprCtx
       -> Id
       -> AltType
       -> [CgStgAlt]
       -> LiveVars
       -> G JStat
genRet ctx e at as l = freshIdent >>= f
  where
    allRefs :: [Id]
    allRefs =  S.toList . S.unions $ fmap (exprRefs emptyUFM . alt_rhs) as
    lneLive :: Int
    lneLive    = maximum $ 0 : catMaybes (map (ctxLneBindingStackSize ctx) allRefs)
    ctx'       = ctxLneShrinkStack ctx lneLive
    lneVars    = map fst $ ctxLneFrameVars ctx'
    isLne i    = ctxIsLneBinding ctx i || ctxIsLneLiveVar ctx' i
    nonLne     = filter (not . isLne) (dVarSetElems l)

    f :: Ident -> G JStat
    f r@(TxtI ri)    =  do
      pushLne  <- pushLneFrame lneLive ctx
      saveCCS  <- ifProfilingM $ push [jCurrentCCS]
      free     <- optimizeFree 0 nonLne
      pushRet  <- pushRetArgs free (toJExpr r)
      fun'     <- fun free
      sr       <- genStaticRefs l -- srt
      prof     <- profiling
      emitClosureInfo $
        ClosureInfo r
                    (CIRegs 0 altRegs)
                    ri
                    (fixedLayout . reverse $
                       map (stackSlotType . fst3) free
                       ++ if prof then [ObjV] else map stackSlotType lneVars)
                    CIStackFrame
                    sr
      emitToplevel $ jFunction r [] fun'
      return (pushLne <> saveCCS <> pushRet)
    fst3 ~(x,_,_)  = x

    altRegs :: HasDebugCallStack => [VarType]
    altRegs = case at of
      PrimAlt ptc    -> [primRepVt ptc]
      MultiValAlt _n -> idVt e
      _              -> [PtrV]

    -- special case for popping CCS but preserving stack size
    pop_handle_CCS :: [(JExpr, StackSlot)] -> G JStat
    pop_handle_CCS [] = return mempty
    pop_handle_CCS xs = do
      -- grab the slots from 'xs' and push
      addSlots (map snd xs)
      -- move the stack pointer into the stack by ''length xs + n'
      a <- adjSpN (length xs)
      -- now load from the top of the stack
      return (loadSkip 0 (map fst xs) <> a)

    fun free = resetSlots $ do
      decs          <- declVarsForId e
      load          <- flip assignAll (map toJExpr [R1 ..]) . map toJExpr <$> identsForId e
      loadv         <- verifyRuntimeReps [e]
      ras           <- loadRetArgs free
      rasv          <- verifyRuntimeReps (map (\(x,_,_)->x) free)
      restoreCCS    <- ifProfilingM . pop_handle_CCS $ pure (jCurrentCCS, SlotUnknown)
      rlne          <- popLneFrame False lneLive ctx'
      rlnev         <- verifyRuntimeReps lneVars
      (alts, _altr) <- genAlts ctx' e at Nothing as
      return $ decs <> load <> loadv <> ras <> rasv <> restoreCCS <> rlne <> rlnev <> alts <>
               returnStack

-- | Consume an Stg case alternative and generate the corresponding alternative
-- in JS land. If one alternative is a continuation then we must normalize the
-- other alternatives. See 'Branch' and 'normalizeBranches'.
genAlts :: HasDebugCallStack
        => ExprCtx        -- ^ lhs to assign expression result to
        -> Id             -- ^ id being matched
        -> AltType        -- ^ type
        -> Maybe [JExpr]  -- ^ if known, fields in datacon from earlier expression
        -> [CgStgAlt]     -- ^ the alternatives
        -> G (JStat, ExprResult)
genAlts ctx e at me alts = do
  (st, er) <- case at of

    PolyAlt -> case alts of
      [alt] -> (branch_stat &&& branch_result) <$> mkAlgBranch ctx e alt
      _     -> panic "genAlts: multiple polyalt"

    PrimAlt _tc
      | [GenStgAlt _ bs expr] <- alts
      -> do
        ie       <- varsForId e
        dids     <- mconcat <$> mapM declVarsForId bs
        bss      <- concatMapM varsForId bs
        (ej, er) <- genExpr ctx expr
        return (dids <> assignAll bss ie <> ej, er)

    PrimAlt tc
      -> do
        ie <- varsForId e
        (r, bss) <- normalizeBranches ctx <$>
           mapM (isolateSlots . mkPrimIfBranch ctx [primRepVt tc]) alts
        setSlots []
        return (mkSw ie bss, r)

    MultiValAlt n
      | [GenStgAlt _ bs expr] <- alts
      -> do
        eids     <- varsForId e
        l        <- loadUbxTup eids bs n
        (ej, er) <- genExpr ctx expr
        return (l <> ej, er)

    AlgAlt tc
      | [_alt] <- alts
      , isUnboxedTupleTyCon tc
      -> panic "genAlts: unexpected unboxed tuple"

    AlgAlt _tc
      | Just es <- me
      , [GenStgAlt (DataAlt dc) bs expr] <- alts
      , not (isUnboxableCon dc)
      -> do
        bsi <- mapM identsForId bs
        (ej, er) <- genExpr ctx expr
        return (declAssignAll (concat bsi) es <> ej, er)

    AlgAlt _tc
      | [alt] <- alts
      -> do
        Branch _ s r <- mkAlgBranch ctx e alt
        return (s, r)

    AlgAlt _tc
      | [alt,_] <- alts
      , DataAlt dc <- alt_con alt
      , isBoolDataCon dc
      -> do
        i <- varForId e
        nbs <- normalizeBranches ctx <$>
            mapM (isolateSlots . mkAlgBranch ctx e) alts
        case nbs of
          (r, [Branch _ s1 _, Branch _ s2 _]) -> do
            let s = if   dataConTag dc == 2
                    then IfStat i s1 s2
                    else IfStat i s2 s1
            setSlots []
            return (s, r)
          _ -> error "genAlts: invalid branches for Bool"

    AlgAlt _tc -> do
        ei <- varForId e
        (r, brs) <- normalizeBranches ctx <$>
            mapM (isolateSlots . mkAlgBranch ctx e) alts
        setSlots []
        return (mkSwitch (ei .^ "f" .^ "a") brs, r)

    _ -> pprPanic "genAlts: unhandled case variant" (ppr (at, length alts))

  ver <- verifyMatchRep e at
  pure (ver <> st, er)

-- | If 'StgToJSConfig.csRuntimeAssert' is set, then generate an assertion that
-- asserts the pattern match is valid, e.g., the match is attempted on a
-- Boolean, a Data Constructor, or some number.
verifyMatchRep :: HasDebugCallStack => Id -> AltType -> G JStat
verifyMatchRep x alt = do
  runtime_assert <- csRuntimeAssert <$> getSettings
  if not runtime_assert
    then pure mempty
    else case alt of
      AlgAlt tc -> do
        ix <- varsForId x
        pure $ ApplStat (var "h$verify_match_alg") (ValExpr(JStr(mkFastString (renderWithContext defaultSDocContext (ppr tc)))):ix)
      _ -> pure mempty

-- | A 'Branch' represents a possible branching path of an Stg case statement,
-- i.e., a possible code path from an 'StgAlt'
data Branch a = Branch
  { branch_expr   :: a
  , branch_stat   :: JStat
  , branch_result :: ExprResult
  }
  deriving (Eq,Functor)

-- | If one branch ends in a continuation but another is inline, we need to
-- adjust the inline branch to use the continuation convention
normalizeBranches :: ExprCtx
                  -> [Branch a]
                  -> (ExprResult, [Branch a])
normalizeBranches ctx brs
    | all (==ExprCont) (fmap branch_result brs) =
        (ExprCont, brs)
    | branchResult (fmap branch_result brs) == ExprCont =
        (ExprCont, map mkCont brs)
    | otherwise =
        (ExprInline Nothing, brs)
  where
    mkCont b = case branch_result b of
      ExprInline{} -> b { branch_stat   = branch_stat b <> assignAll jsRegsFromR1
                                                                     (concatMap typex_expr $ ctxTarget ctx)
                        , branch_result = ExprCont
                        }
      _ -> b

-- | Load an unboxed tuple. "Loading" means getting all 'Idents' from the input
-- ID's, declaring them as variables in JS land and binding them, in order, to
-- 'es'.
loadUbxTup :: [JExpr] -> [Id] -> Int -> G JStat
loadUbxTup es bs _n = do
  bs' <- concatMapM identsForId bs
  return $ declAssignAll bs' es

mkSw :: [JExpr] -> [Branch (Maybe [JExpr])] -> JStat
mkSw [e] cases = mkSwitch e (fmap (fmap (fmap head)) cases)
mkSw es cases  = mkIfElse es cases

-- | Switch for pattern matching on constructors or prims
mkSwitch :: JExpr -> [Branch (Maybe JExpr)] -> JStat
mkSwitch e cases
  | [Branch (Just c1) s1 _] <- n
  , [Branch _ s2 _] <- d
  = IfStat (InfixExpr StrictEqOp e c1) s1 s2

  | [Branch (Just c1) s1 _, Branch _ s2 _] <- n
  , null d
  = IfStat (InfixExpr StrictEqOp e c1) s1 s2

  | null d
  = SwitchStat e (map addBreak (init n)) (branch_stat (last n))

  | [Branch _ d0 _] <- d
  = SwitchStat e (map addBreak n) d0

  | otherwise = panic "mkSwitch: multiple default cases"
  where
    addBreak (Branch (Just c) s _) = (c, mconcat [s, BreakStat Nothing])
    addBreak _                     = panic "mkSwitch: addBreak"
    (n,d) = L.partition (isJust . branch_expr) cases

-- | if/else for pattern matching on things that js cannot switch on
-- the list of branches is expected to have the default alternative
-- first, if it exists
mkIfElse :: [JExpr] -> [Branch (Maybe [JExpr])] -> JStat
mkIfElse e s = go (L.reverse s)
    where
      go = \case
        [Branch _ s _]              -> s -- only one 'nothing' allowed
        (Branch (Just e0) s _ : xs) -> IfStat (mkEq e e0) s (go xs)
        [] -> panic "mkIfElse: empty expression list"
        _  -> panic "mkIfElse: multiple DEFAULT cases"

-- | Wrapper to contruct sequences of (===), e.g.,
--
-- > mkEq [l0,l1,l2] [r0,r1,r2] = (l0 === r0) && (l1 === r1) && (l2 === r2)
--
mkEq :: [JExpr] -> [JExpr] -> JExpr
mkEq es1 es2
  | length es1 == length es2 = foldl1 (InfixExpr LAndOp) (zipWith (InfixExpr StrictEqOp) es1 es2)
  | otherwise                = panic "mkEq: incompatible expressions"

mkAlgBranch :: ExprCtx   -- ^ toplevel id for the result
            -> Id        -- ^ datacon to match
            -> CgStgAlt  -- ^ match alternative with binders
            -> G (Branch (Maybe JExpr))
mkAlgBranch top d alt
  | DataAlt dc <- alt_con alt
  , isUnboxableCon dc
  , [b] <- alt_bndrs alt
  = do
    idd  <- varForId d
    fldx <- identsForId b
    case fldx of
      [fld] -> do
        (ej, er) <- genExpr top (alt_rhs alt)
        return (Branch Nothing (mconcat [fld ||= idd, ej]) er)
      _ -> panic "mkAlgBranch: invalid size"

  | otherwise
  = do
    cc       <- caseCond (alt_con alt)
    idd      <- varForId d
    b        <- loadParams idd (alt_bndrs alt)
    (ej, er) <- genExpr top (alt_rhs alt)
    return (Branch cc (b <> ej) er)

-- | Generate a primitive If-expression
mkPrimIfBranch :: ExprCtx
               -> [VarType]
               -> CgStgAlt
               -> G (Branch (Maybe [JExpr]))
mkPrimIfBranch top _vt alt =
  (\ic (ej,er) -> Branch ic ej er) <$> ifCond (alt_con alt) <*> genExpr top (alt_rhs alt)

-- fixme are bool things always checked correctly here?
ifCond :: AltCon -> G (Maybe [JExpr])
ifCond = \case
  DataAlt da -> return $ Just [toJExpr (dataConTag da)]
  LitAlt l   -> Just <$> genLit l
  DEFAULT    -> return Nothing

caseCond :: AltCon -> G (Maybe JExpr)
caseCond = \case
  DEFAULT    -> return Nothing
  DataAlt da -> return $ Just (toJExpr $ dataConTag da)
  LitAlt l   -> genLit l >>= \case
    [e] -> pure (Just e)
    es  -> pprPanic "caseCond: expected single-variable literal" (ppr $ satJExpr Nothing <$> es)

-- fixme use single tmp var for all branches
-- | Load parameters from constructor
loadParams :: JExpr -> [Id] -> G JStat
loadParams from args = do
  as <- concat <$> zipWithM (\a u -> map (,u) <$> identsForId a) args use
  return $ case as of
    []                 -> mempty
    [(x,u)]            -> loadIfUsed (from .^ closureField1_) x  u
    [(x1,u1),(x2,u2)]  -> mconcat
                            [ loadIfUsed (from .^ closureField1_) x1 u1
                            , loadIfUsed (from .^ closureField2_) x2 u2
                            ]
    ((x,u):xs)         -> mconcat
                            [ loadIfUsed (from .^ closureField1_) x u
                            , jVar (\d -> mconcat [ d |= from .^ closureField2_
                                                  , loadConVarsIfUsed d xs
                                                  ])
                            ]
  where
    use = repeat True -- fixme clean up
    loadIfUsed fr tgt True = tgt ||= fr
    loadIfUsed  _ _   _    = mempty

    loadConVarsIfUsed fr cs = mconcat $ zipWith f cs [(1::Int)..]
      where f (x,u) n = loadIfUsed (SelExpr fr (TxtI (dataFieldName n))) x u

-- | Determine if a branch will end in a continuation or not. If not the inline
-- branch must be normalized. See 'normalizeBranches'
-- NB. not a Monoid
branchResult :: HasDebugCallStack => [ExprResult] -> ExprResult
branchResult = \case
  []                   -> panic "branchResult: empty list"
  [e]                  -> e
  (ExprCont:_)         -> ExprCont
  (_:es)
    | elem ExprCont es -> ExprCont
    | otherwise        -> ExprInline Nothing

-- | Push return arguments onto the stack. The 'Bool' tracks whether the value
-- is already on the stack or not, used in 'StgToJS.Stack.pushOptimized'.
pushRetArgs :: HasDebugCallStack => [(Id,Int,Bool)] -> JExpr -> G JStat
pushRetArgs free fun = do
  rs <- mapM (\(i,n,b) -> (\es->(es!!(n-1),b)) <$> genIdArg i) free
  pushOptimized (rs++[(fun,False)])

-- | Load the return arguments then pop the stack frame
loadRetArgs :: HasDebugCallStack => [(Id,Int,Bool)] -> G JStat
loadRetArgs free = do
  ids <- mapM (\(i,n,_b) -> (!! (n-1)) <$> genIdStackArgI i) free
  popSkipI 1 ids

-- | allocate multiple, possibly mutually recursive, closures
allocDynAll :: Bool -> Maybe JStat -> [(Ident,JExpr,[JExpr],CostCentreStack)] -> G JStat
{-
XXX remove use of template and enable in-place init again
allocDynAll haveDecl middle [(to,entry,free,cc)]
  | isNothing middle && to `notElem` (free ^.. template) = do
      ccs <- ccsVarJ cc
      return $ allocDynamic s haveDecl to entry free ccs -}
allocDynAll haveDecl middle cls = do
  settings <- getSettings
  let
    middle' = fromMaybe mempty middle

    decl_maybe i e
      | haveDecl  = toJExpr i |= e
      | otherwise = i ||= e

    makeObjs :: G JStat
    makeObjs =
      fmap mconcat $ forM cls $ \(i,f,_,cc) -> do
      ccs <- maybeToList <$> costCentreStackLbl cc
      pure $ mconcat
        [ decl_maybe i $ if csInlineAlloc settings
            then ValExpr (jhFromList $ [ (closureEntry_ , f)
                                       , (closureField1_, null_)
                                       , (closureField2_, null_)
                                       , (closureMeta_  , zero_)
                                       ]
                             ++ fmap (\cid -> ("cc", ValExpr (JVar cid))) ccs)
            else ApplExpr (var "h$c") (f : fmap (ValExpr . JVar) ccs)
        ]

    fillObjs = mconcat $ map fillObj cls
    fillObj (i,_,es,_)
      | csInlineAlloc settings || length es > 24 =
          case es of
            []      -> mempty
            [ex]    -> toJExpr i .^ closureField1_ |= toJExpr ex
            [e1,e2] -> mconcat
                        [ toJExpr i .^ closureField1_ |= toJExpr e1
                        , toJExpr i .^ closureField2_ |= toJExpr e2
                        ]
            (ex:es)  -> mconcat
                        [ toJExpr i .^ closureField1_ |= toJExpr ex
                        , toJExpr i .^ closureField2_ |= toJExpr (jhFromList (zip (map dataFieldName [1..]) es))
                        ]
      | otherwise = case es of
            []      -> mempty
            [ex]    -> toJExpr i .^ closureField1_ |= ex
            [e1,e2] -> mconcat
                        [ toJExpr i .^ closureField1_ |= e1
                        , toJExpr i .^ closureField2_ |= e2
                        ]
            (ex:es)  -> mconcat
                        [ toJExpr i .^ closureField1_ |= ex
                        , toJExpr i .^ closureField2_ |= fillFun es
                        ]

    fillFun [] = null_
    fillFun es = ApplExpr (allocData (length es)) es

    checkObjs | csAssertRts settings  = mconcat $
                map (\(i,_,_,_) -> ApplStat (ValExpr (JVar (TxtI "h$checkObj"))) [toJExpr i]) cls
              | otherwise = mempty

  objs <- makeObjs
  pure $ mconcat [objs, middle', fillObjs, checkObjs]

-- | Generate a primop. This function wraps around the real generator
-- 'GHC.StgToJS.genPrim', handling the 'ExprCtx' and all arguments before
-- generating the primop.
genPrimOp :: ExprCtx -> PrimOp -> [StgArg] -> Type -> G (JStat, ExprResult)
genPrimOp ctx op args t = do
  as <- concatMapM genArg args
  prof <- csProf <$> getSettings
  bound <- csBoundsCheck <$> getSettings
  -- fixme: should we preserve/check the primreps?
  return $ case genPrim prof bound t op (concatMap typex_expr $ ctxTarget ctx) as of
             PrimInline s -> (s, ExprInline Nothing)
             PRPrimCall s -> (s, ExprCont)
