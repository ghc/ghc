{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}

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

import GHC.JS.Syntax
import GHC.JS.Make

import GHC.StgToJS.Apply
import GHC.StgToJS.Arg
import GHC.StgToJS.FFI
import GHC.StgToJS.Heap
import GHC.StgToJS.Monad
import GHC.StgToJS.DataCon
import GHC.StgToJS.Types
import GHC.StgToJS.Literal
import GHC.StgToJS.Prim
import GHC.StgToJS.Profiling
import GHC.StgToJS.Regs
import GHC.StgToJS.StgUtils
import GHC.StgToJS.CoreUtils
import GHC.StgToJS.Utils

import GHC.Types.CostCentre
import GHC.Types.Tickish
import GHC.Types.Var.Set
import GHC.Types.Id
import GHC.Types.Unique.Set
import GHC.Types.Unique.FM
import GHC.Types.RepType

import GHC.Stg.Syntax
import GHC.Stg.Utils

import GHC.Builtin.PrimOps

import GHC.Core
import GHC.Core.TyCon
import GHC.Core.DataCon
import GHC.Core.Type hiding (typeSize)

import GHC.Utils.Misc
import GHC.Utils.Monad
import GHC.Utils.Panic
import GHC.Utils.Outputable (ppr, renderWithContext, defaultSDocContext)
import qualified GHC.Utils.Monad.State.Strict as State
import qualified GHC.Data.ShortText as ST
import qualified GHC.Data.List.SetOps as ListSetOps

import Data.Ord
import Data.Monoid
import Data.Maybe
import Data.Function
import Data.Either
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad
import Control.Arrow ((&&&))

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
    -> genExpr (ctx { ctxSrcSpan = Just span} ) e
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
     ctx' = clearCtxStack ctx

     assign :: Id -> CgStgRhs -> G (Maybe JStat)
     assign b (StgRhsClosure _ _ccs {-[the_fv]-} _upd [] expr)
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
           tgts <- genIdsI b
           the_fvjs <- genIds the_fv
           case (tgts, the_fvjs) of
             ([tgt], [the_fvj]) -> return $ Just
               (tgt ||= ApplExpr (var ("h$c_sel_" <> ST.pack sel_tag)) [the_fvj])
             _ -> panic "genBind.assign: invalid size"
     assign b (StgRhsClosure _ext _ccs _upd [] expr)
       | snd (isInlineExpr (ctxEval ctx) expr) = do
           d   <- declIds b
           tgt <- genIds b
           let ctx' = ctx { ctxTarget = alignIdExprs b tgt }
           (j, _) <- genExpr ctx' expr
           return (Just (d <> j))
     assign _b StgRhsCon{} = return Nothing
     assign  b r           = genEntry ctx' b r >> return Nothing

     addEvalRhs c [] = c
     addEvalRhs c ((b,r):xs)
       | StgRhsCon{} <- r                       = addEvalRhs (addEval b c) xs
       | (StgRhsClosure _ _ ReEntrant _ _) <- r = addEvalRhs (addEval b c) xs
       | otherwise                              = addEvalRhs c xs

genBindLne :: HasDebugCallStack
           => ExprCtx
           -> CgStgBinding
           -> G (JStat, ExprCtx)
genBindLne ctx bndr = do
  vis  <- map (\(x,y,_) -> (x,y)) <$>
            optimizeFree oldFrameSize (newLvs++map fst updBinds)
  declUpds <- mconcat <$> mapM (fmap (||= null_) . jsIdI . fst) updBinds
  let newFrameSize = oldFrameSize + length vis
      ctx' = ctx
        { ctxLne        = addListToUniqSet (ctxLne ctx) bound
        , ctxLneFrameBs = addListToUFM (ctxLneFrameBs ctx) (map (,newFrameSize) bound)
        , ctxLneFrame   = ctxLneFrame ctx ++ vis
        }
  mapM_ (uncurry $ genEntryLne ctx') binds
  return (declUpds, ctx')
  where
    oldFrame     = ctxLneFrame ctx
    oldFrameSize = length oldFrame
    isOldLv i    = i `elementOfUniqSet` ctxLne ctx ||
                   i `elem` map fst oldFrame
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
genEntryLne ctx i rhs@(StgRhsClosure _ext _cc update args body) =
  resetSlots $ do
  let payloadSize = length frame
      frame       = ctxLneFrame ctx
      myOffset    =
        maybe (panic "genEntryLne: updatable binder not found in let-no-escape frame")
              ((payloadSize-) . fst)
              (L.find ((==i) . fst . snd) (zip [0..] frame))
      bh | isUpdatable update =
             jVar (\x -> mconcat
              [ x |= ApplExpr (var "h$bh_lne") [Sub sp (toJExpr myOffset), toJExpr (payloadSize+1)]
              , IfStat x (ReturnStat x) mempty
              ])
         | otherwise = mempty
  lvs  <- popLneFrame True payloadSize ctx
  body <- genBody ctx i R1 args body
  ei@(TxtI eii)   <- jsEntryIdI i
  sr   <- genStaticRefsRhs rhs
  let f = JFunc [] (bh <> lvs <> body)
  emitClosureInfo $
    ClosureInfo eii
                (CIRegs 0 $ concatMap idVt args)
                (eii <> ", " <> ST.pack (renderWithContext defaultSDocContext (ppr i)))
                (fixedLayout . reverse $
                    map (stackSlotType . fst) (ctxLneFrame ctx))
                CIStackFrame
                sr
  emitToplevel (ei ||= toJExpr f)
genEntryLne ctx i (StgRhsCon cc con _mu _ticks args) = resetSlots $ do
  let payloadSize = length (ctxLneFrame ctx)
  ei@(TxtI _eii) <- jsEntryIdI i
  -- di <- enterDataCon con
  ii <- makeIdent
  p  <- popLneFrame True payloadSize ctx
  args' <- concatMapM genArg args
  ac    <- allocCon ii con cc args'
  emitToplevel (ei ||= toJExpr (JFunc []
    (mconcat [DeclStat ii, p, ac, r1 |= toJExpr ii, returnStack])))

-- generate the entry function for a local closure
genEntry :: HasDebugCallStack => ExprCtx -> Id -> CgStgRhs -> G ()
genEntry _ _i StgRhsCon {} = return () -- mempty -- error "local data entry"
genEntry ctx i rhs@(StgRhsClosure _ext cc {-_bi live-} upd_flag args body) = resetSlots $ do
  let live = stgLneLiveExpr rhs -- error "fixme" -- probably find live vars in body
  ll    <- loadLiveFun live
  llv   <- verifyRuntimeReps live
  upd   <- genUpdFrame upd_flag i
  body  <- genBody entryCtx i R2 args body
  ei@(TxtI eii) <- jsEntryIdI i
  et    <- genEntryType args
  setcc <- ifProfiling $
             if et == CIThunk
               then enterCostCentreThunk
               else enterCostCentreFun cc
  sr <- genStaticRefsRhs rhs
  emitClosureInfo $ ClosureInfo eii
                                (CIRegs 0 $ PtrV : concatMap idVt args)
                                (eii <> ", " <> ST.pack (renderWithContext defaultSDocContext (ppr i)))
                                (fixedLayout $ map (uTypeVt . idType) live)
                                et
                                sr
  emitToplevel (ei ||= toJExpr (JFunc [] (mconcat [ll, llv, upd, setcc, body])))
  where
    entryCtx = ExprCtx i [] (ctxEval ctx) (ctxLne ctx) emptyUFM [] (ctxSrcSpan ctx)

genEntryType :: HasDebugCallStack => [Id] -> G CIType
genEntryType []   = return CIThunk
genEntryType args0 = do
  args' <- mapM genIdArg args
  return $ CIFun (length args) (length $ concat args')
  where
    args = filter (not . isRuntimeRepKindedTy . idType) args0

genBody :: HasDebugCallStack
         => ExprCtx
         -> Id
         -> StgReg
         -> [Id]
         -> CgStgExpr
         -> G JStat
genBody ctx i startReg args e = do
  la <- loadArgs startReg args
  lav <- verifyRuntimeReps args
  let ids :: [TypedExpr]
      ids = -- take (resultSize args $ idType i) (map toJExpr $ enumFrom R1)
            reverse . fst $
            foldl' (\(rs, vs) (rep, size) ->
                       let (vs0, vs1) = splitAt size vs
                       in  (TypedExpr rep vs0:rs,vs1))
                   ([], map toJExpr $ enumFrom R1)
                   (resultSize args $ idType i)
  (e, _r) <- genExpr (ctx { ctxTarget = ids }) e
  return $ la <> lav <> e <> returnStack

-- find the result type after applying the function to the arguments
resultSize :: HasDebugCallStack => [Id] -> Type -> [(PrimRep, Int)]
resultSize xxs@(_:xs) t
  | t' <- unwrapType t
  , Just (_mult, fa, fr) <- splitFunTy_maybe t' -- isFunTy t' =
  , Just (tc, ys) <- splitTyConApp_maybe fa
  , isUnboxedTupleTyCon tc =
      resultSize xxs (mkVisFunTysMany (dropRuntimeRepArgs ys) fr)
  | t' <- unwrapType t
  , Just (_mult, _fa, fr) <- splitFunTy_maybe t' = -- isFunTy t' =
      resultSize xs fr
  | otherwise = [(LiftedRep, 1)] -- possibly newtype family, must be boxed
resultSize [] t
  | isRuntimeRepKindedTy t' = []
  | isRuntimeRepTy t' = []
  | Nothing <- isLiftedType_maybe t' = [(LiftedRep, 1)]
  | otherwise = fmap (\p -> (p, slotCount (primRepSize p))) (typePrimReps t)
  where
    t' = unwrapType t

loadArgs :: HasDebugCallStack => StgReg -> [Id] -> G JStat
loadArgs start args = do
  args' <- concatMapM genIdArgI args
  return (declAssignAll args' (fmap toJExpr [start..]))

verifyRuntimeReps :: HasDebugCallStack => [Id] -> G JStat
verifyRuntimeReps xs = do
  runtime_assert <- csRuntimeAssert <$> getSettings
  if not runtime_assert
    then pure mempty
    else mconcat <$> mapM verifyRuntimeRep xs
  where
    verifyRuntimeRep i = do
      i' <- genIds i
      pure $ go i' (idVt i)
    go js         (VoidV:vs) = go js vs
    go (j1:j2:js) (LongV:vs) = v "h$verify_rep_long" [j1,j2] <> go js vs
    go (j1:j2:js) (AddrV:vs) = v "h$verify_rep_addr" [j1,j2] <> go js vs
    go (j:js)     (v:vs)     = ver j v                       <> go js vs
    go []         []         = mempty
    go _          _          = pprPanic "verifyRuntimeReps: inconsistent sizes" (ppr xs)
    ver j PtrV    = v "h$verify_rep_heapobj" [j]
    ver j IntV    = v "h$verify_rep_int"     [j]
    ver j RtsObjV = v "h$verify_rep_rtsobj"  [j]
    ver j DoubleV = v "h$verify_rep_double"  [j]
    ver j ArrV    = v "h$verify_rep_arr"     [j]
    ver _ _       = mempty
    v f as = ApplStat (var f) as

loadLiveFun :: [Id] -> G JStat
loadLiveFun l = do
   l' <- concat <$> mapM genIdsI l
   case l' of
     []  -> return mempty
     [v] -> return (v ||= r1 .^ closureField1_)
     [v1,v2] -> return $ mconcat
                        [ v1 ||= r1 .^ closureField1_
                        , v2 ||= r1 .^ closureField2_
                        ]
     (v:vs)  -> do
       d <- makeIdent
       let l'' = mconcat . zipWith (loadLiveVar $ toJExpr d) [(1::Int)..] $ vs
       return $ mconcat
               [ v ||= r1 .^ closureField1_
               , d ||= r1 .^ closureField2_
               , l''
               ]
  where
        loadLiveVar d n v = let ident = TxtI (dataFieldName n)
                            in  DeclStat v `mappend` (toJExpr v |= SelExpr d ident)

popLneFrame :: Bool -> Int -> ExprCtx -> G JStat
popLneFrame inEntry size ctx
  | l < size  = panic $ "popLneFrame: let-no-escape frame too short: " ++
                        show l ++ " < " ++ show size
  | otherwise = popSkipI skip
                  =<< mapM (\(i,n) -> (,SlotId i n) <$> genIdsIN i n)
                           (take size $ ctxLneFrame ctx)
  where
    skip = if inEntry then 1 else 0 -- pop the frame header
    l    = length (ctxLneFrame ctx)

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
                                  isLiftedType_maybe (idType x) == Just False))
                      (dVarSetElems sv)
      CIStaticRefs . catMaybes <$> mapM getStaticRef xs
  where
    sv = liveStatic lv

-- reorder the things we need to push to reuse existing stack values as much as possible
-- True if already on the stack at that location
optimizeFree :: HasDebugCallStack => Int -> [Id] -> G [(Id,Int,Bool)]
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

addEval :: Id -> ExprCtx -> ExprCtx
addEval i ctx = ctx { ctxEval = addOneToUniqSet (ctxEval ctx) i }

-- allocate local closures
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
      ii <- jsIdI i
      Left <$> (return (decl ii) <> allocCon ii con cc []) -}
    toCl (i, StgRhsCon cc con _mui _ticjs [a]) | isUnboxableCon con = do
      ii <- jsIdI i
      ac <- allocCon ii con cc =<< genArg a
      pure (Left (DeclStat ii <> ac))

    -- dynamics
    toCl (i, StgRhsCon cc con _mu _ticks ar) =
      -- fixme do we need to handle unboxed?
      Right <$> ((,,,) <$> jsIdI i
                       <*> enterDataCon con
                       <*> concatMapM genArg ar
                       <*> pure cc)
    toCl (i, cl@(StgRhsClosure _ext cc _upd_flag _args _body)) =
      let live = stgLneLiveExpr cl
      in  Right <$> ((,,,) <$> jsIdI i
                       <*> jsEntryId i
                       <*> concatMapM genIds live
                       <*> pure cc)

-- fixme CgCase has a reps_compatible check here
genCase :: HasDebugCallStack
        => ExprCtx
        -> Id
        -> CgStgExpr
        -> AltType
        -> [CgStgAlt]
        -> LiveVars
        -> G (JStat, ExprResult)
genCase ctx bnd e at alts l
  | snd (isInlineExpr (ctxEval ctx) e) = withNewIdent $ \ccsVar -> do
      bndi <- genIdsI bnd
      let ctx' = ctx
                  { ctxTop    = bnd
                  , ctxTarget = alignIdExprs bnd (map toJExpr bndi)
                  }
      (ej, r) <- genExpr ctx' e
      let d = case r of
                ExprInline d0 -> d0
                ExprCont -> pprPanic "genCase: expression was not inline"
                                     (pprStgExpr panicStgPprOpts e)

          ww = mempty -- if snd (isInlineExpr emptyUniqSet e) then mempty else [j| h$log('danger will robinson'); |]
      (aj, ar) <- genAlts (addEval bnd ctx) bnd at d alts
      saveCCS <- ifProfiling (toJExpr ccsVar |= toJExpr jCurrentCCS)
      restoreCCS <- ifProfiling (toJExpr jCurrentCCS |= toJExpr ccsVar)
      return ( mconcat
          [ DeclStat ccsVar
          , mconcat (map DeclStat bndi)
          , saveCCS
          , ww
          , ej
          , restoreCCS
          , aj
          ]
        , ar
         )
  | otherwise = do
      rj       <- genRet (addEval bnd ctx) bnd at alts l
      let ctx' = ctx
                  { ctxTop    = bnd
                  , ctxTarget = alignIdExprs bnd (map toJExpr [R1 ..])
                  }
      (ej, _r) <- genExpr ctx' e
      return (rj <> ej, ExprCont)

genRet :: HasDebugCallStack
       => ExprCtx
       -> Id
       -> AltType
       -> [CgStgAlt]
       -> LiveVars
       -> G JStat
genRet ctx e at as l = withNewIdent f
  where
    allRefs :: [Id]
    allRefs =  S.toList . S.unions $ fmap (exprRefs emptyUFM . alt_rhs) as
    lneLive :: Int
    lneLive    = maximum $ 0 : map (fromMaybe 0 . lookupUFM (ctxLneFrameBs ctx)) allRefs
    ctx'       = adjustCtxStack lneLive ctx
    lneVars    = map fst $ take lneLive (ctxLneFrame ctx)
    isLne i    = i `elem` lneVars || i `elementOfUniqSet` ctxLne ctx
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
        ClosureInfo ri
                    (CIRegs 0 altRegs)
                    ri
                    (fixedLayout . reverse $
                       map (stackSlotType . fst3) free
                       ++ if prof then [ObjV] else map stackSlotType lneVars)
                    CIStackFrame
                    sr
      emitToplevel $ r ||= toJExpr (JFunc [] fun')
      return (pushLne <> saveCCS <> pushRet)
    fst3 ~(x,_,_)  = x

    altRegs :: HasDebugCallStack => [VarType]
    altRegs = case at of
      PrimAlt ptc    -> [primRepVt ptc]
      MultiValAlt _n -> idVt e
      _              -> [PtrV]

    fun free = resetSlots $ do
      decs          <- declIds e
      load          <- flip assignAll (map toJExpr [R1 ..]) . map toJExpr <$> genIdsI e
      loadv         <- verifyRuntimeReps [e]
      ras           <- loadRetArgs free
      rasv          <- verifyRuntimeReps (map (\(x,_,_)->x) free)
      restoreCCS    <- ifProfilingM $ popUnknown [jCurrentCCS]
      rlne          <- popLneFrame False lneLive ctx'
      rlnev         <- verifyRuntimeReps (map fst $ take lneLive (ctxLneFrame ctx'))
      (alts, _altr) <- genAlts ctx' e at Nothing as
      return $ decs <> load <> loadv <> ras <> rasv <> restoreCCS <> rlne <> rlnev <> alts <>
               returnStack

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
        ie       <- genIds e
        dids     <- mconcat <$> mapM declIds bs
        bss      <- concatMapM genIds bs
        (ej, er) <- genExpr ctx expr
        return (dids <> assignAllEqual bss ie <> ej, er)

    PrimAlt tc
      -> do
        ie <- genIds e
        (r, bss) <- normalizeBranches ctx <$>
           mapM (isolateSlots . mkPrimIfBranch ctx [primRepVt tc]) alts
        setSlots []
        return (mkSw ie bss, r)

    MultiValAlt n
      | [GenStgAlt _ bs expr] <- alts
      -> do
        eids     <- genIds e
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
        bsi <- mapM genIdsI bs
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
        i <- jsId e
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

    -- FIXME: add all alts

    AlgAlt _tc -> do
        ei <- jsId e
        (r, brs) <- normalizeBranches ctx <$>
            mapM (isolateSlots . mkAlgBranch ctx e) alts
        setSlots []
        return (mkSwitch (ei .^ "f" .^ "a") brs, r)

    _ -> pprPanic "genAlts: unhandled case variant" (ppr (at, length alts))

  ver <- verifyMatchRep e at
  pure (ver <> st, er)

verifyMatchRep :: HasDebugCallStack => Id -> AltType -> G JStat
verifyMatchRep x alt = do
  runtime_assert <- csRuntimeAssert <$> getSettings
  if not runtime_assert
    then pure mempty
    else case alt of
      AlgAlt tc -> do
        ix <- genIds x
        pure $ ApplStat (var "h$verify_match_alg") (ValExpr(JStr(ST.pack (renderWithContext defaultSDocContext (ppr tc)))):ix)
      _ -> pure mempty

data Branch a = Branch
  { branch_expr   :: a
  , branch_stat   :: JStat
  , branch_result :: ExprResult
  }
  deriving (Eq,Ord,Functor)

-- if one branch ends in a continuation but another is inline,
-- we need to adjust the inline branch to use the continuation convention
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
      ExprInline{} -> b { branch_stat   = branch_stat b <> assignAll (map toJExpr $ enumFrom R1)
                                                                     (concatMap typex_expr $ ctxTarget ctx)
                        , branch_result = ExprCont
                        }
      _ -> b

loadUbxTup :: [JExpr] -> [Id] -> Int -> G JStat
loadUbxTup es bs _n = do
  bs' <- concatMapM genIdsI bs
  return $ declAssignAll bs' es

mkSw :: [JExpr] -> [Branch (Maybe [JExpr])] -> JStat
mkSw [e] cases = mkSwitch e (fmap (fmap (fmap head)) cases)
mkSw es cases  = mkIfElse es cases

-- switch for pattern matching on constructors or prims
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

-- if/else for pattern matching on things that js cannot switch on
mkIfElse :: [JExpr] -> [Branch (Maybe [JExpr])] -> JStat
mkIfElse e s = go (L.sortOn Down s)
    where
      go = \case
        [Branch _ s _]              -> s -- only one 'nothing' allowed
        (Branch (Just e0) s _ : xs) -> IfStat (mkEq e e0) s (go xs)
        [] -> panic "mkIfElse: empty expression list"
        _  -> panic "mkIfElse: multiple DEFAULT cases"

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
    idd  <- jsId d
    fldx <- genIdsI b
    case fldx of
      [fld] -> do
        (ej, er) <- genExpr top (alt_rhs alt)
        return (Branch Nothing (mconcat [fld ||= idd, ej]) er)
      _ -> panic "mkAlgBranch: invalid size"

  | otherwise
  = do
    cc       <- caseCond (alt_con alt)
    idd      <- jsId d
    b        <- loadParams idd (alt_bndrs alt)
    (ej, er) <- genExpr top (alt_rhs alt)
    return (Branch cc (b <> ej) er)

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
    es  -> pprPanic "caseCond: expected single-variable literal" (ppr es)

-- load parameters from constructor
-- fixme use single tmp var for all branches
loadParams :: JExpr -> [Id] -> G JStat
loadParams from args = do
  as <- concat <$> zipWithM (\a u -> map (,u) <$> genIdsI a) args use
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

-- not a Monoid
branchResult :: HasDebugCallStack => [ExprResult] -> ExprResult
branchResult = \case
  []                   -> panic "branchResult: empty list"
  [e]                  -> e
  (ExprCont:_)         -> ExprCont
  (_:es)
    | elem ExprCont es -> ExprCont
    | otherwise        -> ExprInline Nothing

adjustCtxStack :: Int -> ExprCtx -> ExprCtx
adjustCtxStack n ctx
  | l < n     = panic $ "adjustCtxStack: let-no-escape stack too short: " ++
                        show l ++ " < " ++ show n
  | otherwise = ctx { ctxLneFrame = take n (ctxLneFrame ctx) }
  where
    l = length (ctxLneFrame ctx)

clearCtxStack :: ExprCtx -> ExprCtx
clearCtxStack ctx = ctx
  { ctxLneFrameBs = emptyUFM
  , ctxLneFrame   = []
  }

pushRetArgs :: HasDebugCallStack => [(Id,Int,Bool)] -> JExpr -> G JStat
pushRetArgs free fun = do
  rs <- mapM (\(i,n,b) -> (\es->(es!!(n-1),b)) <$> genIdArg i) free
  pushOptimized (rs++[(fun,False)])

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

    makeObjs :: G JStat
    makeObjs =
      fmap mconcat $ forM cls $ \(i,f,_,cc) -> do
      ccs <- maybeToList <$> costCentreStackLbl cc
      pure $ mconcat
        [ dec i
        , toJExpr i |= if csInlineAlloc settings
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
      | csInlineAlloc settings || length es > 24 = -- FIXME (Jeff, 2022/03): the call to length means `es`
                                                   -- should be something other than
                                                   -- a list. Also why is 24
                                                   -- important? And 24 should be a
                                                   -- constant such as `fooThreshold`
          case es of
            []      -> mempty
            [ex]    -> toJExpr i .^ closureField1_ |= toJExpr ex
            [e1,e2] -> mconcat
                        [ toJExpr i .^ closureField1_ |= toJExpr e1
                        , toJExpr i .^ closureField2_ |= toJExpr e2
                        ]
            (ex:es)  -> mconcat
                        [ toJExpr i .^ closureField1_ |= toJExpr ex
                        , toJExpr i .^ closureField2_ |= toJExpr (jhFromList (zip dataFieldNames es))
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

    dec i | haveDecl  = DeclStat i
          | otherwise = mempty
    checkObjs | csAssertRts settings  = mconcat $
                map (\(i,_,_,_) -> ApplStat (ValExpr (JVar (TxtI "h$checkObj"))) [toJExpr i] {-[j| h$checkObj(`i`); |]-}) cls
              | otherwise = mempty

  objs <- makeObjs
  pure $ mconcat [objs, middle', fillObjs, checkObjs]

genPrimOp :: ExprCtx -> PrimOp -> [StgArg] -> Type -> G (JStat, ExprResult)
genPrimOp ctx op args t = do
  as <- concatMapM genArg args
  prof <- csProf <$> getSettings
  -- fixme: should we preserve/check the primreps?
  return $ case genPrim prof t op (concatMap typex_expr $ ctxTarget ctx) as of
             PrimInline s -> (s, ExprInline Nothing)
             PRPrimCall s -> (s, ExprCont)
