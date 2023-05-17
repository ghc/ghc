{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module GHC.StgToJS.Utils
  ( assignCoerce1
  , assignToExprCtx
  , fixedLayout
  , assocIdExprs
  -- * Unboxable datacon
  , isUnboxableCon
  , isUnboxable
  , isBoolDataCon
  -- * JSRep
  , slotCount
  , varSize
  , typeSize
  , isVoid
  , isMultiVar
  , idJSRep
  , typeJSRep
  , unaryTypeJSRep
  , primRepToJSRep
  , stackSlotType
  , primRepSize
  , mkArityTag
  -- * References and Ids
  , exprRefs
  , hasExport
  , collectTopIds
  , collectIds
  -- * Live variables
  , LiveVars
  , liveStatic
  , liveVars
  , stgRhsLive
  , stgExprLive
  , isUpdatableRhs
  , stgLneLive'
  , stgLneLiveExpr
  , isInlineExpr
  )
where

import GHC.Prelude

import GHC.StgToJS.Types
import GHC.StgToJS.ExprCtx

import GHC.JS.Unsat.Syntax
import GHC.JS.Make
import GHC.JS.Transform

import GHC.Core.DataCon
import GHC.Core.TyCo.Rep hiding (typeSize)
import GHC.Core.TyCon
import GHC.Core.Type hiding (typeSize)

import GHC.Stg.Syntax

import GHC.Tc.Utils.TcType

import GHC.Builtin.Names
import GHC.Builtin.PrimOps (PrimOp(SeqOp), primOpIsReallyInline)

import GHC.Types.RepType
import GHC.Types.Var
import GHC.Types.Var.Set
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.Unique.FM
import GHC.Types.Unique.Set
import GHC.Types.ForeignCall
import GHC.Types.TyThing
import GHC.Types.Name

import GHC.Utils.Misc
import GHC.Utils.Outputable hiding ((<>))
import GHC.Utils.Panic

import qualified Data.Bits as Bits
import qualified Data.Foldable as F
import qualified Data.Set      as S
import qualified Data.List     as L
import Data.Set (Set)
import Data.Monoid


assignToTypedExprs :: [TypedExpr] -> [JExpr] -> JStat
assignToTypedExprs tes es =
  assignAllEqual (concatMap typex_expr tes) es

assignTypedExprs :: [TypedExpr] -> [TypedExpr] -> JStat
assignTypedExprs tes es =
  let prim_tes = concatMap typex_expr tes
      prim_es  = concatMap typex_expr es
        -- extract the JExprs, effectively unarising a RuntimeRep thing to
        -- multiple VarType-repped things (e.g., AddrRep takes two VarType-regs)
  in assertPpr (equalLength prim_tes prim_es)
               (ppr (map typex_typ tes) $$ ppr (map typex_typ es))
               (assignAllEqual prim_tes prim_es)

assignToExprCtx :: ExprCtx -> [JExpr] -> JStat
assignToExprCtx ctx es = assignToTypedExprs (ctxTarget ctx) es

-- | Assign first expr only (if it exists), performing coercions between some
-- PrimReps (e.g. StablePtr# and Addr#).
assignCoerce1 :: [TypedExpr] -> [TypedExpr] -> JStat
assignCoerce1 [x] [y] = assignCoerce x y
assignCoerce1 []  []  = mempty
assignCoerce1 _x _y   = pprPanic "assignCoerce1"
                          (vcat [ text "lengths do not match"
                                -- FIXME: Outputable instance removed until JStg replaces JStat
                                -- , ppr x
                                -- , ppr y
                                ])

-- | Assign p2 to p1 with optional coercion
assignCoerce :: TypedExpr -> TypedExpr -> JStat
-- Coercion between StablePtr# and Addr#
assignCoerce (TypedExpr AddrRep [a_val, a_off]) (TypedExpr (BoxedRep (Just Unlifted)) [sptr]) = mconcat
    [ a_val |= var "h$stablePtrBuf"
    , a_off |= sptr
    ]
assignCoerce (TypedExpr (BoxedRep (Just Unlifted)) [sptr]) (TypedExpr AddrRep [_a_val, a_off]) =
  sptr |= a_off
assignCoerce p1 p2 = assignTypedExprs [p1] [p2]


--------------------------------------------------------------------------------
--                        Core Utils
--------------------------------------------------------------------------------

-- | can we unbox C x to x, only if x is represented as a Number
isUnboxableCon :: DataCon -> Bool
isUnboxableCon dc
  | [t] <- dataConRepArgTys dc
  , [t1] <- typeJSRep (scaledThing t)
  = isUnboxable t1 &&
    dataConTag dc == 1 &&
    length (tyConDataCons $ dataConTyCon dc) == 1
  | otherwise = False

-- | one-constructor types with one primitive field represented as a JS Number
-- can be unboxed
isUnboxable :: JSRep -> Bool
isUnboxable DoubleV = True
isUnboxable IntV    = True -- includes Char#
isUnboxable _       = False

-- | Number of slots occupied by a PrimRep
data SlotCount
  = NoSlot
  | OneSlot
  | TwoSlots
  deriving (Show,Eq,Ord)

instance Outputable SlotCount where
  ppr = text . show

-- | Return SlotCount as an Int
slotCount :: SlotCount -> Int
slotCount = \case
  NoSlot   -> 0
  OneSlot  -> 1
  TwoSlots -> 2


-- | Number of slots occupied by a value with the given JSRep
varSize :: JSRep -> Int
varSize = slotCount . jsRepSlots

jsRepSlots :: JSRep -> SlotCount
jsRepSlots VoidV = NoSlot
jsRepSlots LongV = TwoSlots -- hi, low
jsRepSlots AddrV = TwoSlots -- obj/array, offset
jsRepSlots _     = OneSlot

typeSize :: Type -> Int
typeSize t = sum . map varSize . typeJSRep $ t

isVoid :: JSRep -> Bool
isVoid VoidV = True
isVoid _     = False

isMultiVar :: JSRep -> Bool
isMultiVar v = case jsRepSlots v of
  NoSlot   -> False
  OneSlot  -> False
  TwoSlots -> True

idJSRep :: HasDebugCallStack => Id -> [JSRep]
idJSRep = typeJSRep . idType

typeJSRep :: HasDebugCallStack => Type -> [JSRep]
typeJSRep t = map primRepToJSRep (typePrimRep t)

-- only use if you know it's not an unboxed tuple
unaryTypeJSRep :: HasDebugCallStack => UnaryType -> JSRep
unaryTypeJSRep ut = primRepToJSRep (typePrimRep1 ut)

primRepToJSRep :: HasDebugCallStack => PrimRep -> JSRep
primRepToJSRep VoidRep      = VoidV
primRepToJSRep (BoxedRep _) = PtrV
primRepToJSRep IntRep       = IntV
primRepToJSRep Int8Rep      = IntV
primRepToJSRep Int16Rep     = IntV
primRepToJSRep Int32Rep     = IntV
primRepToJSRep WordRep      = IntV
primRepToJSRep Word8Rep     = IntV
primRepToJSRep Word16Rep    = IntV
primRepToJSRep Word32Rep    = IntV
primRepToJSRep Int64Rep     = LongV
primRepToJSRep Word64Rep    = LongV
primRepToJSRep AddrRep      = AddrV
primRepToJSRep FloatRep     = DoubleV
primRepToJSRep DoubleRep    = DoubleV
primRepToJSRep (VecRep{})   = error "primRepToJSRep: vector types are unsupported"

dataConType :: DataCon -> Type
dataConType dc = idType (dataConWrapId dc)

isBoolDataCon :: DataCon -> Bool
isBoolDataCon dc = isBoolTy (dataConType dc)

-- standard fixed layout: payload types
-- payload starts at .d1 for heap objects, entry closest to Sp for stack frames
fixedLayout :: [JSRep] -> CILayout
fixedLayout vts = CILayoutFixed (sum (map varSize vts)) vts

-- 2-var values might have been moved around separately, use DoubleV as substitute
-- ObjV is 1 var, so this is no problem for implicit metadata
stackSlotType :: Id -> JSRep
stackSlotType i
  | OneSlot <- jsRepSlots otype = otype
  | otherwise                   = DoubleV
  where otype = unaryTypeJSRep (idType i)

idPrimReps :: Id -> [PrimRep]
idPrimReps = typePrimReps . idType

typePrimReps :: Type -> [PrimRep]
typePrimReps = typePrimRep . unwrapType

primRepSize :: PrimRep -> SlotCount
primRepSize p = jsRepSlots (primRepToJSRep p)

-- | Associate the given values to each RrimRep in the given order, taking into
-- account the number of slots per PrimRep
assocPrimReps :: [PrimRep] -> [JExpr] -> [(PrimRep, [JExpr])]
assocPrimReps []     _  = []
assocPrimReps (r:rs) vs = case (primRepSize r,vs) of
  (NoSlot,   xs)     -> (r,[])    : assocPrimReps rs xs
  (OneSlot,  x:xs)   -> (r,[x])   : assocPrimReps rs xs
  (TwoSlots, x:y:xs) -> (r,[x,y]) : assocPrimReps rs xs
  err                -> pprPanic "assocPrimReps" (ppr $ map (satJExpr Nothing) <$> err)

-- | Associate the given values to the Id's PrimReps, taking into account the
-- number of slots per PrimRep
assocIdPrimReps :: Id -> [JExpr] -> [(PrimRep, [JExpr])]
assocIdPrimReps i = assocPrimReps (idPrimReps i)

-- | Associate the given JExpr to the Id's PrimReps, taking into account the
-- number of slots per PrimRep
assocIdExprs :: Id -> [JExpr] -> [TypedExpr]
assocIdExprs i es = fmap (uncurry TypedExpr) (assocIdPrimReps i es)

mkArityTag :: Int -> Int -> Int
mkArityTag arity registers = arity Bits..|. (registers `Bits.shiftL` 8)

--------------------------------------------------------------------------------
--                        Stg Utils
--------------------------------------------------------------------------------

s :: a -> Set a
s = S.singleton

l :: (a -> Set Id) -> [a] -> Set Id
l = F.foldMap

-- | collect Ids that this binding refers to
--   (does not include the bindees themselves)
-- first argument is Id -> StgExpr map for unfloated arguments
bindingRefs :: UniqFM Id CgStgExpr -> CgStgBinding -> Set Id
bindingRefs u = \case
  StgNonRec _ rhs -> rhsRefs u rhs
  StgRec bs       -> l (rhsRefs u . snd) bs

rhsRefs :: UniqFM Id CgStgExpr -> CgStgRhs -> Set Id
rhsRefs u = \case
  StgRhsClosure _ _ _ _ body _       -> exprRefs u body
  StgRhsCon _ccs d _mu _ticks args _ -> l s [ i | AnId i <- dataConImplicitTyThings d] <> l (argRefs u) args

exprRefs :: UniqFM Id CgStgExpr -> CgStgExpr -> Set Id
exprRefs u = \case
  StgApp f args             -> s f <> l (argRefs u) args
  StgConApp d _n args _     -> l s [ i | AnId i <- dataConImplicitTyThings d] <> l (argRefs u) args
  StgOpApp _ args _         -> l (argRefs u) args
  StgLit {}                 -> mempty
  StgCase expr _ _ alts     -> exprRefs u expr <> mconcat (fmap (altRefs u) alts)
  StgLet _ bnd expr         -> bindingRefs u bnd <> exprRefs u expr
  StgLetNoEscape _ bnd expr -> bindingRefs u bnd <> exprRefs u expr
  StgTick _ expr            -> exprRefs u expr

altRefs :: UniqFM Id CgStgExpr -> CgStgAlt -> Set Id
altRefs u alt = exprRefs u (alt_rhs alt)

argRefs :: UniqFM Id CgStgExpr -> StgArg -> Set Id
argRefs u = \case
  StgVarArg id
    | Just e <- lookupUFM u id -> exprRefs u e
    | otherwise                -> s id
  _ -> mempty

hasExport :: CgStgBinding -> Bool
hasExport bnd =
  case bnd of
    StgNonRec b e -> isExportedBind b e
    StgRec bs     -> any (uncurry isExportedBind) bs
  where
    isExportedBind _i (StgRhsCon _cc con _ _ _ _) =
      getUnique con == staticPtrDataConKey
    isExportedBind _ _ = False

collectTopIds :: CgStgBinding -> [Id]
collectTopIds (StgNonRec b _) = [b]
collectTopIds (StgRec bs) = let xs = map (zapFragileIdInfo . fst) bs
                            in  seqList xs `seq` xs

collectIds :: UniqFM Id CgStgExpr -> CgStgBinding -> [Id]
collectIds unfloated b =
  let xs = map zapFragileIdInfo .
           filter acceptId $ S.toList (bindingRefs unfloated b)
  in  seqList xs `seq` xs
  where
    acceptId i = all ($ i) [not . isForbidden] -- fixme test this: [isExported[isGlobalId, not.isForbidden]
    -- the GHC.Prim module has no js source file
    isForbidden i
      | Just m <- nameModule_maybe (getName i) = m == gHC_PRIM
      | otherwise = False

-----------------------------------------------------
-- Live vars
--
-- TODO: should probably be moved into GHC.Stg.LiveVars

type LiveVars = DVarSet

liveStatic :: LiveVars -> LiveVars
liveStatic = filterDVarSet isGlobalId

liveVars :: LiveVars -> LiveVars
liveVars = filterDVarSet (not . isGlobalId)

stgBindLive :: CgStgBinding -> [(Id, LiveVars)]
stgBindLive = \case
  StgNonRec b rhs -> [(b, stgRhsLive rhs)]
  StgRec bs       -> map (\(b,rhs) -> (b, stgRhsLive rhs)) bs

stgBindRhsLive :: CgStgBinding -> LiveVars
stgBindRhsLive b =
  let (bs, ls) = unzip (stgBindLive b)
  in  delDVarSetList (unionDVarSets ls) bs

stgRhsLive :: CgStgRhs -> LiveVars
stgRhsLive = \case
  StgRhsClosure _ _ _ args e _ -> delDVarSetList (stgExprLive True e) args
  StgRhsCon _ _ _ _ args _     -> unionDVarSets (map stgArgLive args)

stgArgLive :: StgArg -> LiveVars
stgArgLive = \case
  StgVarArg occ -> unitDVarSet occ
  StgLitArg {}  -> emptyDVarSet

stgExprLive :: Bool -> CgStgExpr -> LiveVars
stgExprLive includeLHS = \case
  StgApp occ args -> unionDVarSets (unitDVarSet occ : map stgArgLive args)
  StgLit {}       -> emptyDVarSet
  StgConApp _dc _n args _tys -> unionDVarSets (map stgArgLive args)
  StgOpApp _op args _ty      -> unionDVarSets (map stgArgLive args)
  StgCase e b _at alts
    | includeLHS -> el `unionDVarSet` delDVarSet al b
    | otherwise  -> delDVarSet al b
    where
      al = unionDVarSets (map stgAltLive alts)
      el = stgExprLive True e
  StgLet _ b e         -> delDVarSetList (stgBindRhsLive b `unionDVarSet` stgExprLive True e) (bindees b)
  StgLetNoEscape _ b e -> delDVarSetList (stgBindRhsLive b `unionDVarSet` stgExprLive True e) (bindees b)
  StgTick _ti e        -> stgExprLive True e

stgAltLive :: CgStgAlt -> LiveVars
stgAltLive alt =
  delDVarSetList (stgExprLive True (alt_rhs alt)) (alt_bndrs alt)

bindees :: CgStgBinding -> [Id]
bindees = \case
  StgNonRec b _e -> [b]
  StgRec bs      -> map fst bs

isUpdatableRhs :: CgStgRhs -> Bool
isUpdatableRhs (StgRhsClosure _ _ u _ _ _) = isUpdatable u
isUpdatableRhs _                           = False

stgLneLive' :: CgStgBinding -> [Id]
stgLneLive' b = filter (`notElem` bindees b) (stgLneLive b)

stgLneLive :: CgStgBinding -> [Id]
stgLneLive (StgNonRec _b e) = stgLneLiveExpr e
stgLneLive (StgRec bs)      = L.nub $ concatMap (stgLneLiveExpr . snd) bs

stgLneLiveExpr :: CgStgRhs -> [Id]
stgLneLiveExpr rhs = dVarSetElems (liveVars $ stgRhsLive rhs)
-- stgLneLiveExpr (StgRhsClosure _ _ _ _ e) = dVarSetElems (liveVars (stgExprLive e))
-- stgLneLiveExpr StgRhsCon {}              = []

-- | returns True if the expression is definitely inline
isInlineExpr :: UniqSet Id -> CgStgExpr -> (UniqSet Id, Bool)
isInlineExpr v = \case
  StgApp i args
    -> (emptyUniqSet, isInlineApp v i args)
  StgLit{}
    -> (emptyUniqSet, True)
  StgConApp{}
    -> (emptyUniqSet, True)
  StgOpApp (StgFCallOp f _) _ _
    -> (emptyUniqSet, isInlineForeignCall f)
  StgOpApp (StgPrimOp SeqOp) [StgVarArg e] t
    -> (emptyUniqSet, e `elementOfUniqSet` v || isStrictType t)
  StgOpApp (StgPrimOp op) _ _
    -> (emptyUniqSet, primOpIsReallyInline op)
  StgOpApp (StgPrimCallOp _c) _ _
    -> (emptyUniqSet, True)
  StgCase e b _ alts
    ->let (_ve, ie)   = isInlineExpr v e
          v'          = addOneToUniqSet v b
          (vas, ias)  = unzip $ map (isInlineExpr v') (fmap alt_rhs alts)
          vr          = L.foldl1' intersectUniqSets vas
      in (vr, (ie || b `elementOfUniqSet` v) && and ias)
  StgLet _ b e
    -> isInlineExpr (inspectInlineBinding v b) e
  StgLetNoEscape _ _b e
    -> isInlineExpr v e
  StgTick  _ e
    -> isInlineExpr v e

inspectInlineBinding :: UniqSet Id -> CgStgBinding -> UniqSet Id
inspectInlineBinding v = \case
  StgNonRec i r -> inspectInlineRhs v i r
  StgRec bs     -> foldl' (\v' (i,r) -> inspectInlineRhs v' i r) v bs

inspectInlineRhs :: UniqSet Id -> Id -> CgStgRhs -> UniqSet Id
inspectInlineRhs v i = \case
  StgRhsCon{}                       -> addOneToUniqSet v i
  StgRhsClosure _ _ ReEntrant _ _ _ -> addOneToUniqSet v i
  _                                 -> v

isInlineForeignCall :: ForeignCall -> Bool
isInlineForeignCall (CCall (CCallSpec _ cconv safety)) =
  not (playInterruptible safety) &&
  not (cconv /= JavaScriptCallConv && playSafe safety)

isInlineApp :: UniqSet Id -> Id -> [StgArg] -> Bool
isInlineApp v i = \case
  _ | isJoinId i -> False
  [] -> isUnboxedTupleType (idType i) ||
                     isStrictType (idType i) ||
                     i `elementOfUniqSet` v

  [StgVarArg a]
    | DataConWrapId dc <- idDetails i
    , isNewTyCon (dataConTyCon dc)
    , isStrictType (idType a) || a `elementOfUniqSet` v || isStrictId a
    -> True
  _ -> False
