{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.StgToJS.Args
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Jeffrey Young  <jeffrey.young@iohk.io>
--                Luite Stegeman <luite.stegeman@iohk.io>
--                Sylvain Henry  <sylvain.henry@iohk.io>
--                Josh Meredith  <josh.meredith@iohk.io>
-- Stability   :  experimental
--
--  Code generation of application arguments
-----------------------------------------------------------------------------

module GHC.StgToJS.Arg
  ( genArg
  , genIdArg
  , genIdArgI
  , genIdStackArgI
  , allocConStatic
  , jsStaticArgs
  )
where

import GHC.Prelude

import GHC.JS.JStg.Syntax
import GHC.JS.Ident
import GHC.JS.Make

import GHC.StgToJS.DataCon
import GHC.StgToJS.Types
import GHC.StgToJS.Monad
import GHC.StgToJS.Literal
import GHC.StgToJS.Utils
import GHC.StgToJS.Profiling
import GHC.StgToJS.Ids

import GHC.Builtin.Types
import GHC.Stg.Syntax
import GHC.Core.DataCon

import GHC.Types.CostCentre
import GHC.Types.Unique.FM
import GHC.Types.Id

import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic
import qualified Control.Monad.Trans.State.Strict as State

{-
Note [ Unboxable Literals Optimization ]
~~~~~~~~~~~~~~~~~~

Boxable types in the JS backend are represented as heap objects. See Note
[StgToJS design] in GHC.StgToJS.hs for more details. Some types, such as Int8
do not benefit from not being wrapped in an object in the JS runtime. This optimization
detects such types and changes the code generator to generate a more efficient
representation. The change is minor and saves one level on indirection. Instead
of generating a wrapper object with a field for the value's payload, such as:

// a JS object for an Int8
var anInt8 = { d1 = <Int8# payload>
             , f  : info table / entry function which would scrutinize the payload
             }

we instead generate:

// notice, no wrapper object. This representation is essentially an Int8# in the JS backend
var anInt8 = <Int8# payload>

This optimization fires when the follow invariants hold:
  1. The value in question has a Type which has a single data constructor
  2. The data constructor holds a single field that is monomorphic
  3. The value in question is distinguishable from a THUNK using the JavaScript typeof operator.

From the haskell perspective this means that:
  1. An Int8# is always a JavaScript 'number', never a JavaScript object.
  2. An Int8 is either a JavaScript 'number' _or_ a JavaScript object depending on
     its use case and this optimization.

How is this sound?
~~~~~~~~~~~~~~~~~~

Normally this optimization would violate the guarantees of call-by-need, however
we are able to statically detect whether the type in question will be a THUNK or
not during code gen because the JS backend is consuming STG and we can check
during runtime with the typeof operator. Similarly we can check at runtime using
JavaScript's introspection operator `typeof`. Thus, when we know the value in
question will not be a THUNK we can safely elide the wrapping object, which
unboxes the value in the JS runtime. For example, an Int8 contains an Int8#
which has the JavaScript type 'number'. A THUNK of type Int8 would have a
JavaScript type 'object', so using 'typeof' allows us to check if we have
something that is definitely evaluated (i.e., a 'number') or something else. If
it is an 'object' then we may need to enter it to begin its evaluation. Consider
a type which has a 'ThreadId#' field; such as type would not be subject to this
optimization because it has to be represented as a JavaScript 'object' and thus
cannot be unboxed in this way. Another (edge) case is Int64#. Int64# is
similarly not unboxable in this way because Int64# does not fit in one
JavaScript variable and thus requires an 'object' for its representation in the
JavaScript runtime.

-}

-- | Generate JS code for static arguments
genStaticArg :: HasDebugCallStack => StgArg -> G [StaticArg]
genStaticArg a = case a of
  StgLitArg l -> map StaticLitArg <$> genStaticLit l
  StgVarArg i -> do
    unFloat <- State.gets gsUnfloated
    case lookupUFM unFloat i of
      Nothing -> reg
      Just expr -> unfloated expr
     where
       r = primOrVoidRepToJSRep $ stgArgRep1 a
       reg
         | isVoid r            =
             return []
         | i == trueDataConId  =
             return [StaticLitArg (BoolLit True)]
         | i == falseDataConId =
             return [StaticLitArg (BoolLit False)]
         | isMultiVar r        =
             map (\(identFS -> t) -> StaticObjArg t) <$> mapM (identForIdN i) [1..varSize r] -- this seems wrong, not an obj?
         | otherwise           = (\(identFS -> it) -> [StaticObjArg it]) <$> identForId i

       unfloated :: CgStgExpr -> G [StaticArg]
       unfloated (StgLit l) = map StaticLitArg <$> genStaticLit l
       unfloated (StgConApp dc _n args _)
         | isBoolDataCon dc || isUnboxableCon dc =
             (:[]) . allocUnboxedConStatic dc . concat <$> mapM genStaticArg args -- fixme what is allocunboxedcon?
         | null args = (\(identFS -> t) -> [StaticObjArg t]) <$> identForId (dataConWorkId dc)
         | otherwise = do
             as       <- concat <$> mapM genStaticArg args
             e <- identFS <$> identForDataConWorker dc
             return [StaticConArg e as]
       unfloated x = pprPanic "genArg: unexpected unfloated expression" (pprStgExpr panicStgPprOpts x)

-- | Generate JS code for an StgArg
genArg :: HasDebugCallStack => StgArg -> G [JStgExpr]
genArg a = case a of
  StgLitArg l -> genLit l
  StgVarArg i -> do
    unFloat <- State.gets gsUnfloated
    case lookupUFM unFloat i of
      Just expr -> unfloated expr
      Nothing
       | isVoid r            -> return []
       | i == trueDataConId  -> return [true_]
       | i == falseDataConId -> return [false_]
       | isMultiVar r        -> mapM (varForIdN i) [1..varSize r]
       | otherwise           -> (:[]) <$> varForId i

   where
     -- if our argument is a joinid, it can be an unboxed tuple
     r :: HasDebugCallStack => JSRep
     r = primOrVoidRepToJSRep $ stgArgRep1 a

     unfloated :: HasDebugCallStack => CgStgExpr -> G [JStgExpr]
     unfloated = \case
      StgLit l -> genLit l
      StgConApp dc _n args _
       | isBoolDataCon dc || isUnboxableCon dc
       -> (:[]) . allocUnboxedCon dc . concat <$> mapM genArg args
       | null args -> (:[]) <$> varForId (dataConWorkId dc)
       | otherwise -> do
           as <- concat <$> mapM genArg args
           e  <- varForDataConWorker dc
           inl_alloc <- csInlineAlloc <$> getSettings
           return [allocDynamicE inl_alloc e as Nothing]
      x -> pprPanic "genArg: unexpected unfloated expression" (pprStgExpr panicStgPprOpts x)

-- | Generate a Var as JStgExpr
genIdArg :: HasDebugCallStack => Id -> G [JStgExpr]
genIdArg i = genArg (StgVarArg i)

-- | Generate an Id as an Ident
genIdArgI :: HasDebugCallStack => Id -> G [Ident]
genIdArgI i
  | isVoid r     = return []
  | isMultiVar r = mapM (identForIdN i) [1..varSize r]
  | otherwise    = (:[]) <$> identForId i
  where
    r = unaryTypeJSRep . idType $ i

-- | Generate IDs for stack arguments. See 'StgToJS.Expr.loadRetArgs' for use case
genIdStackArgI :: HasDebugCallStack => Id -> G [(Ident,StackSlot)]
genIdStackArgI i = zipWith f [1..] <$> genIdArgI i
  where
    f :: Int -> Ident -> (Ident,StackSlot)
    f n ident = (ident, SlotId i n)

-- | Allocate Static Constructors
allocConStatic :: HasDebugCallStack => Ident -> CostCentreStack -> DataCon -> [StgArg] -> G ()
allocConStatic (identFS -> to) cc con args = do
  as <- mapM genStaticArg args
  cc' <- costCentreStackLbl cc
  allocConStatic' cc' (concat as)
  where
    -- see Note [ Unboxable Literals Optimization ] for the purpose of these
    -- checks
    allocConStatic' :: HasDebugCallStack => Maybe Ident -> [StaticArg] -> G ()
    allocConStatic' cc' []
      | isBoolDataCon con && dataConTag con == 1 =
           emitStatic to (StaticUnboxed $ StaticUnboxedBool False) cc'
      | isBoolDataCon con && dataConTag con == 2 =
           emitStatic to (StaticUnboxed $ StaticUnboxedBool True) cc'
      | otherwise = do
           e <- identFS <$> identForDataConWorker con
           emitStatic to (StaticApp SAKData e []) cc'
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
                e <- identFS <$> identForDataConWorker con
                emitStatic to (StaticApp SAKData e xs) cc'

-- | Allocate unboxed constructors
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


-- | Allocate Static list
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

-- | Generate JS code corresponding to a static arg
jsStaticArg :: StaticArg -> JStgExpr
jsStaticArg = \case
  StaticLitArg l      -> toJExpr l
  StaticObjArg t      -> global t
  StaticConArg c args ->
    allocDynamicE False (global c) (map jsStaticArg args) Nothing

-- | Generate JS code corresponding to a list of static args
jsStaticArgs :: [StaticArg] -> JStgExpr
jsStaticArgs = ValExpr . JList . map jsStaticArg

