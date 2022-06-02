{-# LANGUAGE LambdaCase #-}

module GHC.StgToJS.Arg
  ( genArg
  , genStaticArg
  , genIdArg
  , genIdArgI
  , genIdStackArgI
  , allocConStatic
  , allocUnboxedConStatic
  , allocateStaticList
  )
where

import GHC.Prelude

import GHC.JS.Syntax
import GHC.JS.Make

import GHC.StgToJS.DataCon
import GHC.StgToJS.Types
import GHC.StgToJS.Monad
import GHC.StgToJS.Literal
import GHC.StgToJS.CoreUtils
import GHC.StgToJS.Profiling

import GHC.Builtin.Types
import GHC.Stg.Syntax
import GHC.Core.DataCon

import GHC.Types.CostCentre
import GHC.Types.Unique.FM
import GHC.Types.Id

import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic
import qualified GHC.Utils.Monad.State.Strict as State

genStaticArg :: HasDebugCallStack => StgArg -> G [StaticArg]
genStaticArg (StgLitArg l) = map StaticLitArg <$> genStaticLit l
genStaticArg a@(StgVarArg i) = do
  unFloat <- State.gets gsUnfloated
  case lookupUFM unFloat i of
    Nothing -> reg
    Just expr -> unfloated expr
   where
     r = uTypeVt . stgArgType $ a
     reg
       | isVoid r            =
           return []
       | i == trueDataConId  =
           return [StaticLitArg (BoolLit True)]
       | i == falseDataConId =
           return [StaticLitArg (BoolLit False)]
       | isMultiVar r        =
           map (\(TxtI t) -> StaticObjArg t) <$> mapM (jsIdIN i) [1..varSize r] -- this seems wrong, not an obj?
       | otherwise           = (\(TxtI it) -> [StaticObjArg it]) <$> jsIdI i

     unfloated :: CgStgExpr -> G [StaticArg]
     unfloated (StgLit l) = map StaticLitArg <$> genStaticLit l
     unfloated (StgConApp dc _n args _)
       | isBoolDataCon dc || isUnboxableCon dc =
           (:[]) . allocUnboxedConStatic dc . concat <$> mapM genStaticArg args -- fixme what is allocunboxedcon?
       | null args = (\(TxtI t) -> [StaticObjArg t]) <$> jsIdI (dataConWorkId dc)
       | otherwise = do
           as       <- concat <$> mapM genStaticArg args
           (TxtI e) <- enterDataConI dc
           return [StaticConArg e as]
     unfloated x = pprPanic "genArg: unexpected unfloated expression" (pprStgExpr panicStgPprOpts x)

genArg :: HasDebugCallStack => StgArg -> G [JExpr]
genArg (StgLitArg l) = genLit l
genArg a@(StgVarArg i) = do
  unFloat <- State.gets gsUnfloated
  case lookupUFM unFloat i of
    Nothing -> reg
    Just expr -> unfloated expr
   where
     -- if our argument is a joinid, it can be an unboxed tuple
     r :: HasDebugCallStack => VarType
     r = uTypeVt . stgArgType $ a
     reg
       | isVoid r     = return []
       | i == trueDataConId  = return [true_]
       | i == falseDataConId = return [false_]
       | isMultiVar r = mapM (jsIdN i) [1..varSize r]
       | otherwise    = (:[]) <$> jsId i

     unfloated :: HasDebugCallStack => CgStgExpr -> G [JExpr]
     unfloated = \case
      StgLit l -> genLit l
      StgConApp dc _n args _
       | isBoolDataCon dc || isUnboxableCon dc
       -> (:[]) . allocUnboxedCon dc . concat <$> mapM genArg args
       | null args -> (:[]) <$> jsId (dataConWorkId dc)
       | otherwise -> do
           as <- concat <$> mapM genArg args
           e  <- enterDataCon dc
           cs <- getSettings
           return [allocDynamicE cs e as Nothing] -- FIXME: ccs
      x -> pprPanic "genArg: unexpected unfloated expression" (pprStgExpr panicStgPprOpts x)

genIdArg :: HasDebugCallStack => Id -> G [JExpr]
genIdArg i = genArg (StgVarArg i)

genIdArgI :: HasDebugCallStack => Id -> G [Ident]
genIdArgI i
  | isVoid r     = return []
  | isMultiVar r = mapM (jsIdIN i) [1..varSize r]
  | otherwise    = (:[]) <$> jsIdI i
  where
    r = uTypeVt . idType $ i


genIdStackArgI :: HasDebugCallStack => Id -> G [(Ident,StackSlot)]
genIdStackArgI i = zipWith f [1..] <$> genIdArgI i
  where
    f :: Int -> Ident -> (Ident,StackSlot)
    f n ident = (ident, SlotId i n)


allocConStatic :: HasDebugCallStack => Ident -> CostCentreStack -> DataCon -> [StgArg] -> G ()
allocConStatic (TxtI to) cc con args = do
  as <- mapM genStaticArg args
  cc' <- costCentreStackLbl cc
  allocConStatic' cc' (concat as)
  where
    allocConStatic' :: HasDebugCallStack => Maybe Ident -> [StaticArg] -> G ()
    allocConStatic' cc' []
      | isBoolDataCon con && dataConTag con == 1 =
           emitStatic to (StaticUnboxed $ StaticUnboxedBool False) cc'
      | isBoolDataCon con && dataConTag con == 2 =
           emitStatic to (StaticUnboxed $ StaticUnboxedBool True) cc'
      | otherwise = do
           (TxtI e) <- enterDataConI con
           emitStatic to (StaticData e []) cc'
    allocConStatic' cc' [x]
      | isUnboxableCon con =
        case x of
          StaticLitArg (IntLit i)    ->
            emitStatic to (StaticUnboxed $ StaticUnboxedInt i) cc'
          StaticLitArg (BoolLit b)   ->
            emitStatic to (StaticUnboxed $ StaticUnboxedBool b) cc'
          StaticLitArg (DoubleLit d) ->
            emitStatic to (StaticUnboxed $ StaticUnboxedDouble d) cc'
          _                          ->
            pprPanic "allocConStatic: invalid unboxed literal" (ppr x)
    allocConStatic' cc' xs =
           if con == consDataCon
              then case args of
                (a0:a1:_) -> flip (emitStatic to) cc' =<< allocateStaticList [a0] a1
                _         -> panic "allocConStatic: invalid args for consDataCon"
              else do
                (TxtI e) <- enterDataConI con
                emitStatic to (StaticData e xs) cc'

allocUnboxedConStatic :: DataCon -> [StaticArg] -> StaticArg
allocUnboxedConStatic con = \case
  []
    | isBoolDataCon con && dataConTag con == 1
    -> StaticLitArg (BoolLit False)
    | isBoolDataCon con && dataConTag con == 2
    -> StaticLitArg (BoolLit True)
  [a@(StaticLitArg (IntLit _i))]    -> a
  [a@(StaticLitArg (DoubleLit _d))] -> a
  _ -> pprPanic "allocUnboxedConStatic: not an unboxed constructor" (ppr con)


allocateStaticList :: [StgArg] -> StgArg -> G StaticVal
allocateStaticList xs a@(StgVarArg i)
  | isDataConId_maybe i == Just nilDataCon = listAlloc xs Nothing
  | otherwise = do
      unFloat <- State.gets gsUnfloated
      case lookupUFM unFloat i of
        Just (StgConApp dc _n [h,t] _)
          | dc == consDataCon -> allocateStaticList (h:xs) t
        _ -> listAlloc xs (Just a)
  where
    listAlloc :: [StgArg] -> Maybe StgArg -> G StaticVal
    listAlloc xs Nothing  = do
      as <- concat . reverse <$> mapM genStaticArg xs
      return (StaticList as Nothing)
    listAlloc xs (Just r) = do
      as <- concat . reverse <$> mapM genStaticArg xs
      r' <- genStaticArg r
      case r' of
        [StaticObjArg ri] -> return (StaticList as (Just ri))
        _                 ->
          pprPanic "allocateStaticList: invalid argument (tail)" (ppr (xs, r))
allocateStaticList _ _ = panic "allocateStaticList: unexpected literal in list"

