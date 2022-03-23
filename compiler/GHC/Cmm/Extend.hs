{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -Wno-typed-holes #-}
-----------------------------------------------------------------------------
--
-- Cmm extend MachOp bitwidths
--
-- (c) Andreas Klebinger 2022
--
-- The point of this pass is to replace sub-word operations with operations
-- which are at least n-bits wide. Either for performance reasons (x86) or because
-- the smaller ones don't exist (x64)
--
-- The mind width always has to be bigger than the word size.
--
-----------------------------------------------------------------------------

module GHC.Cmm.Extend (
        cmmExtendMachOps,
 ) where

import GHC.Prelude

import GHC.Cmm.Dataflow
import GHC.Cmm.Utils
import GHC.Cmm
import GHC.Utils.Misc

import GHC.Utils.Panic
import GHC.Platform

import Data.Maybe
import Data.Bifunctor
import GHC.Types.Unique.Supply
import GHC.Cmm.Dataflow.Block
import GHC.Utils.Panic.Plain
import Foreign.Storable (Storable(alignment))
import GHC.Utils.Trace
import GHC.Utils.Outputable
import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dataflow.Block (blockJoinList)

import GHC.Cmm.Ppr


-- TODO: Stuff
-- MO_X_MulMayOflo -> Likely needs to reimplemented as a Machop -> CmmExpr transformation.



-- | Information about content in registers

-- Facts are states at some point of program execution.
type ExtFacts = ()
-- Fact base is per-block
type ExtFactBase = FactBase ()
type ExtLattice = DataflowLattice ()
type ExtRewriteFun = CmmBlock -> ExtFactBase -> UniqSM (CmmBlock, ExtFactBase)

extLattice :: ExtLattice
extLattice = DataflowLattice () (\_old _new -> NotChanged ())

-- Summarizes the state of a value.
-- If we store a int8 in a int16 register we would get:
-- ValInfo { valSignificantWidth = 8, valActualWidth = 16, valExt = SigExt}
data ValInfo
    = ValInfo
    { valSignificantWidth :: Width -- ^ Significant value bits. These are determined by the *input* program and are not modified.
    , valActualWidth :: Width  -- ^ Bit's currently used to store the value and operated on.
    , valExt :: ExtInfo -- High bit info. If we extend how is it done.
    }

instance Outputable ValInfo where
    ppr ValInfo { valSignificantWidth, valActualWidth, valExt } =
        text "ValInfo" <> parens (ppr valSignificantWidth <+> text "as" <+> ppr valActualWidth <> text "_" <> ppr valExt)


type ValReq = ValInfo
data ExtOpts
  = ExtOpts
  { eo_minWidth :: !Width
  , eo_platform :: Platform }

isSignExtended :: a -> CmmReg -> Bool
isSignExtended state reg = undefined

isZeroExtended :: a -> CmmReg -> Bool
isZeroExtended state reg = undefined

makeExactWidthVal :: Width -> ValInfo
makeExactWidthVal w = ValInfo w w GarbageExt




cmmExtendMachOps :: Platform -> CmmDecl -> UniqSM CmmDecl
cmmExtendMachOps platform dataDecl@(CmmData{}) = return dataDecl
cmmExtendMachOps platform (CmmProc h g_entry regs graph) = do
  graph' <- cmmExtendGraph platform W32 graph
  return (CmmProc h g_entry regs graph')

----
cmmExtendGraph :: Platform -> Width -> CmmGraph -> UniqSM CmmGraph
cmmExtendGraph platform min_width graph =
  fst <$> rewriteCmmGraph extLattice (extRewrite extOpts) graph mapEmpty
  where
    extOpts = ExtOpts { eo_minWidth = min_width, eo_platform = platform }

rewriteCmmGraph :: ExtLattice -> ExtRewriteFun -> CmmGraph -> ExtFactBase -> (UniqSM (CmmGraph,ExtFactBase))
rewriteCmmGraph =

  rewriteCmmFwd


extRewrite :: ExtOpts -> CmmBlock -> ExtFactBase -> UniqSM (CmmBlock, ExtFactBase)
extRewrite opts block facts = do
  let (entry_node,middle_block,exit_node) = blockSplit block
      middle_nodes = blockToList middle_block
      block_facts = getFact extLattice (entryLabel entry_node) facts

  middle_nodes' <- concat <$> mapM (\n -> fst <$> extMiddleExitNode opts block_facts n) middle_nodes :: (UniqSM [CmmNode O O])
  exit_node' <- head . fst <$> extMiddleExitNode opts block_facts exit_node

  let block' = blockJoinList entry_node middle_nodes' exit_node'

  return (block',facts)

extMiddleExitNode :: forall x. ExtOpts -> ExtFacts -> CmmNode O x -> UniqSM ([CmmNode O x], ExtFacts)
extMiddleExitNode opts facts node = do
    exprUpdate $ mapExp (extSubExprs opts facts) node
  -- case node of
  --   CmmComment{} -> doNothing
  --   CmmUnwind unwinds -> do
  --     unwinds' <- mapM (secondM (extMExpr facts)) unwinds
  --     exprUpdate (CmmUnwind unwinds')
  --   CmmTick{} -> doNothing

  --   -- We could widen local registers as well here, to avoid truncating them again later.
  --   -- But that would mean rewriting all their occurences.
  --   CmmAssign reg expr -> do
  --     let expr_w = makeExactWidthVal $ cmmExprWidth platform expr
  --         expr' = extExpr opts facts (Just $ expr_w) expr
  --     exprUpdate (CmmAssign reg expr')

  --   CmmStore mem val align -> do
  --     let mem' = extSubExprs opts facts mem
  --         val' = extSubExprs opts facts val
  --     exprUpdate (CmmStore mem' val' align)

  --   CmmUnsafeForeignCall target results args -> do
  --     exprUpdate $ CmmUnsafeForeignCall
  --       target
  --       results
  --       (map (extSubExprs opts facts) args)

  --   CmmBranch{} -> doNothing

  --   CmmCondBranch pred t f l ->
  --     exprUpdate $ CmmCondBranch (extSubExprs opts facts pred) t f l

  --   CmmSwitch expr targets ->
  --     exprUpdate $ CmmSwitch (extSubExprs opts facts expr) targets

  --   CmmCall {} -> exprUpdate $ mapExp (extSubExprs opts facts) node
  --   CmmForeignCall {} -> exprUpdate $ mapExp (extSubExprs opts facts) node

    where


      platform = eo_platform opts

      extMExpr :: ExtFacts -> Maybe CmmExpr -> UniqSM (Maybe CmmExpr)
      extMExpr _facts Nothing = return Nothing
      extMExpr facts (Just expr) = return $ Just $ extExpr opts facts Nothing expr

      exprUpdate :: CmmNode O x -> UniqSM ([CmmNode O x], ExtFacts)
      exprUpdate node = return ([node],facts)

      -- Return node as-is
      doNothing :: UniqSM ([CmmNode O x], ExtFacts)
      doNothing = return ([node],facts)

-- Extend operations within the given expression. But return an expression of the exact same width as the
-- one given at the end.
extSubExprs :: ExtOpts -> ExtFacts -> CmmExpr -> CmmExpr
extSubExprs opts facts expr =
  -- We want to end up with the same widths inside the expressions used in statements so we give fixed widths here.
  let tar_width = makeExactWidthVal (cmmExprWidth (eo_platform opts) expr)
  in extExpr opts facts (Just tar_width) expr

extExpr :: ExtOpts
        -> ExtFacts
        -> Maybe ValReq
        -> CmmExpr
        -> CmmExpr
extExpr opts facts tar_req expr =
  pprTrace "extExpr" (ppr tar_req <+> pdoc platform expr) $
  case expr of
    CmmLit l
        | Just req <- tar_req
        -> extLit req l
        | otherwise -> expr
    CmmLoad l_e l_ty l_alignment -> mk_ext_op tar_req (CmmLoad l_e' l_ty l_alignment)
      where
        -- We don't change address calculation width, hence Nothing.
        l_e' = extExpr opts facts Nothing l_e
    CmmReg _r -> mk_ext_op tar_req expr
    CmmMachOp op args ->
        mk_ext_op tar_req $ extMachOp opts facts tar_req op args

    CmmStackSlot{} -> mk_ext_op tar_req expr
    CmmRegOff _reg _off -> mk_ext_op tar_req expr
    where
      platform = eo_platform opts


extMachOp :: ExtOpts -> ExtFacts -> Maybe ValReq -> MachOp -> [CmmExpr] -> CmmExpr
extMachOp opts facts tar_req op args
    -- Do we need to extend this machop?
    | Just (op',reqs) <- machOpInputReq opts tar_req op
    = mk_ext_op tar_req $
        CmmMachOp op' $ zipWith (extExpr opts facts) (map Just reqs) args

    -- If not just make sure the args are in proper form.
    | otherwise
    = let   platform = eo_platform opts
            arg_widths = machOpArgReps platform op
            arg_reqs = map (Just . makeExactWidthVal) arg_widths
      in
            CmmMachOp op $ zipWith (extExpr opts facts) arg_reqs args




-- | Extend a literal. We should however never extend label references
-- or vector literals.
extLit :: ValReq
       -> CmmLit
       -> CmmExpr
extLit tarReq@(ValInfo { valActualWidth, valSignificantWidth, valExt })  lit
  | valActualWidth == valSignificantWidth = CmmLit lit
  | otherwise =
  case lit of
    CmmInt i w
        | SignExt <- valExt
        -> CmmLit $ CmmInt i valActualWidth
        | GarbageExt <- valExt
        -> CmmLit $ CmmInt i valActualWidth
        | otherwise
        -> mk_ext_op (Just tarReq) (CmmLit lit)

    -- None of these should ever be extended
    CmmFloat _ w -> panic "extLit - CmmFloat"
    CmmVec _lits -> panic "extLit - CmmVec"
    CmmLabel{} -> panic "extLit - CmmLabel"
    CmmLabelOff{} -> panic "extLit - CmmLabelOff"
    CmmLabelDiffOff l1 l2 off w -> panic "extLit - CmmLabelDiffOff"
    CmmBlock{} -> panic "extLit - CmmLabelDiffOff"
    CmmHighStackMark -> panic "extLit - CmmLabelDiffOff"




-- | Widen the expr to the required number of bits using a extension
-- machop.
mk_ext_op :: Maybe ValReq -> CmmExpr -> CmmExpr
mk_ext_op tar_req src_expr
  | Nothing <- tar_req
  = src_expr
  | Just req <- tar_req
  , ValInfo { valActualWidth, valSignificantWidth, valExt } <- req
  = if valActualWidth == valSignificantWidth -- Nothing to do
        then src_expr
        else
          let mop = assert (valSignificantWidth <= valActualWidth) $
                      case valExt of
                      ZeroExt       -> MO_UU_Conv valSignificantWidth valActualWidth
                      SignExt       -> MO_SS_Conv valSignificantWidth valActualWidth
                      GarbageExt    -> MO_XX_Conv valSignificantWidth valActualWidth
          in  CmmMachOp mop [src_expr]








-- zero extend 8->16:

-- 0xFF -> 0x00FF

-------------------------------------------------------------------------------
-- MachOp information.
-------------------------------------------------------------------------------

-- | Information about high bits.
data ExtInfo
  = SignExt     -- ^ High bits sign extended.
  | ZeroExt     -- ^ High bits zero extended.
  | GarbageExt  -- ^ High bits garbage.
  deriving Show

instance Outputable ExtInfo where
    ppr SignExt = text "SS"
    ppr ZeroExt = text "UU"
    ppr GarbageExt = text "XX"



-- | If we use (x `op16` y) to implement (x `op8` y) what are the high bits of the result like?
machOpResultExt :: [ExtInfo] -> MachOp -> ExtInfo
machOpResultExt _arg_info op =
  case op of
    -- Integer operations (insensitive to signed/unsigned)
    MO_Add{}  -> GarbageExt
    MO_Sub{}  -> GarbageExt
    MO_Eq  {}  -> GarbageExt
    MO_Ne  {}  -> GarbageExt
    MO_Mul {}  -> GarbageExt                -- low word of multiply

    -- Signed multiply/divide
    MO_S_MulMayOflo {}  -> GarbageExt
           -- nonzero if signed multiply overflows
    MO_S_Quot {}  -> GarbageExt             -- signed / (same semantics as IntQuotOp)
    MO_S_Rem  {}  -> GarbageExt             -- signed % (same semantics as IntRemOp)
    MO_S_Neg  {}  -> GarbageExt             -- unary -

    -- Unsigned multiply/divide
    MO_U_MulMayOflo {}  -> GarbageExt       -- nonzero if unsigned multiply overflows
    MO_U_Quot {}  -> GarbageExt             -- unsigned / (same semantics as WordQuotOp)
    MO_U_Rem  {}  -> GarbageExt             -- unsigned % (same semantics as WordRemOp)

    -- Signed comparisons
    MO_S_Ge {}  -> GarbageExt
    MO_S_Le {}  -> GarbageExt
    MO_S_Gt {}  -> GarbageExt
    MO_S_Lt {}  -> GarbageExt

    -- Unsigned comparisons
    MO_U_Ge {}  -> GarbageExt
    MO_U_Le {}  -> GarbageExt
    MO_U_Gt {}  -> GarbageExt
    MO_U_Lt {}  -> GarbageExt

    -- Floating point arithmetic
    MO_F_Add  {}  -> GarbageExt
    MO_F_Sub  {}  -> GarbageExt
    MO_F_Neg  {}  -> GarbageExt
    MO_F_Mul  {}  -> GarbageExt
    MO_F_Quot {}  -> GarbageExt

    -- Floating point comparison
    MO_F_Eq {}  -> GarbageExt
    MO_F_Ne {}  -> GarbageExt
    MO_F_Ge {}  -> GarbageExt
    MO_F_Le {}  -> GarbageExt
    MO_F_Gt {}  -> GarbageExt
    MO_F_Lt {}  -> GarbageExt

    -- Bitwise operations.  Not all of these may be supported
    -- at all sizes, and only integral Widths are valid.
    MO_And   {}  -> GarbageExt
    MO_Or    {}  -> GarbageExt
    MO_Xor   {}  -> GarbageExt
    MO_Not   {}  -> GarbageExt

    -- Shifts. The shift amount must be in [0,widthInBits).
    MO_Shl   {}  -> GarbageExt
    MO_U_Shr {}  -> GarbageExt
    MO_S_Shr {}  -> GarbageExt

    -- Conversions.  Some of these will be NOPs.
    -- Floating-point conversions use the signed variant.
    MO_SF_Conv {}  -> GarbageExt
    MO_FS_Conv {}  -> GarbageExt
    MO_SS_Conv {}  -> GarbageExt
    MO_UU_Conv {}  -> GarbageExt
    MO_XX_Conv {}  -> GarbageExt
                                  -- contents of upper bits when extending;
                                  -- narrowing is simply truncation; the only
                                  -- expectation is that we can recover the
                                  -- original value by applying the opposite
                                  -- MO_XX_Conv, e.g.,
                                  --   MO_XX_CONV W64 W8 (MO_XX_CONV W8 W64 x)
                                  -- is equivalent to just x.
    MO_FF_Conv {}  -> GarbageExt

    -- Vector element insertion and extraction operations
    MO_V_Insert  {}  -> GarbageExt
    MO_V_Extract {}  -> GarbageExt

    -- Integer vector operations
    MO_V_Add {}  -> GarbageExt
    MO_V_Sub {}  -> GarbageExt
    MO_V_Mul {}  -> GarbageExt

    -- Signed vector multiply/divide
    MO_VS_Quot {}  -> GarbageExt
    MO_VS_Rem  {}  -> GarbageExt
    MO_VS_Neg  {}  -> GarbageExt

    -- Unsigned vector multiply/divide
    MO_VU_Quot {}  -> GarbageExt
    MO_VU_Rem  {}  -> GarbageExt

    -- Floating point vector element insertion and extraction operations
    MO_VF_Insert  {}  -> GarbageExt   -- Insert scalar into vector
    MO_VF_Extract {}  -> GarbageExt   -- Extract scalar from vector

    -- Floating point vector operations
    MO_VF_Add  {}  -> GarbageExt
    MO_VF_Sub  {}  -> GarbageExt
    MO_VF_Neg  {}  -> GarbageExt      -- unary negation
    MO_VF_Mul  {}  -> GarbageExt
    MO_VF_Quot {}  -> GarbageExt

    -- Alignment check (for -falignment-sanitisation)
    MO_AlignmentCheck {}  -> GarbageExt



-- Input requirements for arguments.
-- If we use (x `op16` y) to implement (x `op8` y) what are the requirements on the x/y high bits for this to
-- be valid.
-- Returns nothing if we can't or shouldn't widen the operation.
machOpInputReq :: ExtOpts -> Maybe ValReq -> MachOp -> Maybe (MachOp, [ValReq])
machOpInputReq opts tar_req op
    -- Can the operator be widened? If not do nothing
    | Nothing <- m_orig_width
    = Nothing
    -- Is it already wide enough? If not do nothing
    | Just orig_op_width <- m_orig_width
    , orig_op_width >= min_width
    = Nothing

    | Just orig_op_width <- m_orig_width -- Otherwise return the new operator to use!
    = let   mkExtReq ext = ValInfo
                                { valSignificantWidth = orig_op_width
                                , valActualWidth = min_width
                                , valExt = ext }

            todo = undefined
            platform = eo_platform opts
            g1 op = Just (op min_width, [mkExtReq GarbageExt])
            s1 op = Just (op min_width, [mkExtReq SignExt])
            u1 op = Just (op min_width, [mkExtReq ZeroExt])
            g2 op = Just (op min_width, [mkExtReq GarbageExt,mkExtReq GarbageExt])
            s2 op = Just (op min_width, [mkExtReq SignExt,mkExtReq SignExt])
            u2 = Just [ZeroExt,ZeroExt]

    in
    case op of
        MO_Add    r         -> g2 MO_Add
        MO_Sub    r         -> s2 MO_Sub
        MO_Eq     r         -> s2 MO_Eq
        MO_Ne     r         -> s2 MO_Ne
        MO_Mul    r         -> s2 MO_Mul
        MO_S_MulMayOflo r   -> Nothing
        MO_S_Quot r         -> s2 MO_S_Quot
        MO_S_Rem  r         -> s2 MO_S_Rem
        MO_S_Neg  r         -> s1 MO_S_Neg
        MO_U_MulMayOflo r   -> Nothing
        MO_U_Quot r         -> s2 MO_U_Quot
        MO_U_Rem  r         -> s2 MO_U_Rem

        MO_S_Ge r           -> s2 MO_S_Ge
        MO_S_Le r           -> s2 MO_S_Le
        MO_S_Gt r           -> s2 MO_S_Gt
        MO_S_Lt r           -> s2 MO_S_Lt

        MO_U_Ge r           -> s2 MO_U_Ge
        MO_U_Le r           -> s2 MO_U_Le
        MO_U_Gt r           -> s2 MO_U_Gt
        MO_U_Lt r           -> s2 MO_U_Lt

        MO_F_Add r          -> Nothing
        MO_F_Sub r          -> Nothing
        MO_F_Mul r          -> Nothing
        MO_F_Quot r         -> Nothing
        MO_F_Neg r          -> Nothing
        MO_F_Eq  r          -> Nothing
        MO_F_Ne  r          -> Nothing
        MO_F_Ge  r          -> Nothing
        MO_F_Le  r          -> Nothing
        MO_F_Gt  r          -> Nothing
        MO_F_Lt  r          -> Nothing

        MO_And   r          -> s2 MO_And
        MO_Or    r          -> s2 MO_Or
        MO_Xor   r          -> s2 MO_Xor
        MO_Not   r          -> s2 MO_Not

         -- TODO: shifts
        MO_Shl   r          -> Nothing
        MO_U_Shr r          -> Nothing
        MO_S_Shr r          -> Nothing

        MO_SS_Conv from _   -> Nothing
        MO_UU_Conv from _   -> Nothing
        MO_XX_Conv from _   -> Nothing
        MO_SF_Conv from _   -> Nothing
        MO_FS_Conv from _   -> Nothing
        MO_FF_Conv from _   -> Nothing

        MO_V_Insert  l r    -> Nothing
        MO_V_Extract l r    -> Nothing

        MO_V_Add _ r        -> Nothing
        MO_V_Sub _ r        -> Nothing
        MO_V_Mul _ r        -> Nothing

        MO_VS_Quot _ r      -> Nothing
        MO_VS_Rem  _ r      -> Nothing
        MO_VS_Neg  _ r      -> Nothing

        MO_VU_Quot _ r      -> Nothing
        MO_VU_Rem  _ r      -> Nothing

        MO_VF_Insert  l r   -> Nothing
        MO_VF_Extract l r   -> Nothing

        MO_VF_Add  _ r      -> Nothing
        MO_VF_Sub  _ r      -> Nothing
        MO_VF_Mul  _ r      -> Nothing
        MO_VF_Quot _ r      -> Nothing
        MO_VF_Neg  _ r      -> Nothing

        MO_AlignmentCheck align r -> g1 (MO_AlignmentCheck align)

    where
        min_width
            | Just req <- tar_req
            = assert (valActualWidth req >= eo_minWidth opts)
                    max (eo_minWidth opts) (valActualWidth req)
            | otherwise
            = eo_minWidth opts
        m_orig_width = machOpWidth op
