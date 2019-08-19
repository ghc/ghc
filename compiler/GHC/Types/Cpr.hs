{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

-- | Types for the Constructed Product Result lattice. "GHC.Core.Opt.CprAnal"
-- and "GHC.Core.Opt.WorkWrap.Utils" are its primary customers via 'GHC.Types.Id.idCprInfo'.
module GHC.Types.Cpr (
    Cpr, topCpr, botCpr, conCpr, asConCpr, seqCpr,
    TerminationFlag (..), topTermFlag, botTermFlag,
    Termination, topTerm, botTerm, whnfTerm, recFunTerm, conTerm, lubTerm,
    CprType (..), topCprType, botCprType, conCprType, pruneDeepCpr,
    pruneDeepTerm, markConCprType, lubCprType, applyCprTy,
    abstractCprTy, abstractCprTyNTimes, ensureCprTyArity, trimCprTy, forceCprTy,
    forceTerm, bothCprType, cprTransformDataConSig, argCprTypesFromStrictSig,
    CprSig (..), mkCprSig, mkCprSigForArity, topCprSig, seqCprSig
  ) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Types.Basic
import GHC.Types.Demand
import GHC.Core.DataCon
import GHC.Core.TyCon (isDataTyCon)
import GHC.Utils.Outputable
import GHC.Utils.Binary
import Data.Tuple (swap)

--------------
-- * Levitated

data Levitated a
  = Bot
  | Levitate a
  | Top
  deriving Eq

seqLevitated :: (a -> ()) -> Levitated a -> ()
seqLevitated seq_a (Levitate a) = seq_a a
seqLevitated _     _            = ()

lubLevitated :: (a -> a -> Levitated a) -> Levitated a -> Levitated a -> Levitated a
lubLevitated _     Bot          l            = l
lubLevitated _     l            Bot          = l
lubLevitated _     Top          _            = Top
lubLevitated _     _            Top          = Top
lubLevitated lub_a (Levitate a) (Levitate b) = lub_a a b

---------------
-- * KnownShape

data KnownShape r = Con !ConTag [r]
  deriving Eq

seqKnownShape :: (r -> ()) -> KnownShape r -> ()
seqKnownShape seq_r (Con _ args) = foldr (seq . seq_r) () args

lubKnownShape :: (r -> r -> r) -> KnownShape r -> KnownShape r -> Levitated (KnownShape r)
lubKnownShape lub_r (Con t1 args1) (Con t2 args2)
  | t1 == t2, args1 `equalLength` args2
  = Levitate (Con t1 (zipWith lub_r args1 args2))
lubKnownShape _ _ _
  = Top

pruneKnownShape :: (Int -> r -> r) -> Int -> KnownShape r -> Levitated (KnownShape r)
pruneKnownShape _       0     _            = Top
pruneKnownShape prune_r depth (Con t args) = Levitate (Con t   (map (prune_r (depth - 1)) args))

----------------
-- * Termination

data TerminationFlag
  = Terminates
  | MightDiverge
  deriving Eq

topTermFlag :: TerminationFlag
topTermFlag = MightDiverge

botTermFlag :: TerminationFlag
botTermFlag = Terminates

lubTermFlag :: TerminationFlag -> TerminationFlag -> TerminationFlag
lubTermFlag MightDiverge _            = MightDiverge
lubTermFlag _            MightDiverge = MightDiverge
lubTermFlag Terminates   Terminates   = Terminates

lubTermFlags :: [TerminationFlag] -> TerminationFlag
lubTermFlags = foldl' lubTermFlag botTermFlag

type Termination' = (TerminationFlag, Levitated (KnownShape Termination))

-- | Normalises wrt. to some non-syntactic equalities, making sure there is only
-- one bottom and top.
liftTermination' :: TerminationFlag -> Levitated (KnownShape Termination) -> Levitated Termination'
liftTermination' Terminates   Bot = Bot
liftTermination' MightDiverge Top = Top
liftTermination' tm           l   = Levitate (tm, l)

newtype Termination
  = Termination (Levitated Termination')
  deriving (Eq, Binary)

topTerm :: Termination
topTerm = Termination Top

botTerm :: Termination
botTerm = Termination Bot

-- | Terminates rapidly to WHNF.
whnfTerm :: Termination
whnfTerm = shallowTerm Terminates

shallowTerm :: TerminationFlag -> Termination
shallowTerm tm
  | tm == topTermFlag = topTerm
  | otherwise         = Termination (Levitate (tm, Top))

deepTerm :: TerminationFlag -> Termination
deepTerm tm
  | tm == botTermFlag = botTerm
  | otherwise         = Termination (Levitate (tm, Bot))

-- | The initial termination of a recursive function in fixed-point iteration.
-- We assume a recursive call 'MightDiverge', but are optimistic about all
-- nested termination information. I.e., we assume that evaluating returned
-- tuple components 'Terminates' rapidly.
recFunTerm :: Termination
recFunTerm = deepTerm MightDiverge

-- Smart contructor for @Termination tm (Just (Con t fs))@ that respects the
-- non-syntactic equalities of @Termination@.
conTerm :: TerminationFlag -> ConTag -> [Termination] -> Termination
conTerm tm t fs
  | all (== topTerm) fs                    = shallowTerm tm
  | all (== botTerm) fs, tm == botTermFlag = botTerm
  | otherwise                              = Termination (Levitate (tm, (Levitate (Con t fs))))

lubTerm :: Termination -> Termination -> Termination
lubTerm (Termination l1) (Termination l2)
  = Termination (lubLevitated lub_pairs l1 l2)
  where
    lub_pairs (tm1, l_sh1) (tm2, l_sh2) =
      liftTermination' (lubTermFlag tm1 tm2)
                       (lubLevitated (lubKnownShape lubTerm) l_sh1 l_sh2)

pruneDeepTerm :: Int -> Termination -> Termination
pruneDeepTerm depth (Termination (Levitate (tm, Levitate sh)))
  = Termination (liftTermination' tm (pruneKnownShape pruneDeepTerm depth sh))
pruneDeepTerm _     term                              = term

splitTermination :: Termination -> Termination'
-- Basically the inverse to liftTermination', I guess?!
splitTermination (Termination Top)                = (topTermFlag, Top)
splitTermination (Termination Bot)                = (botTermFlag, Bot)
splitTermination (Termination (Levitate (tm, l))) = (tm, l)

seqTerm :: Termination -> ()
seqTerm (Termination l) = seqLevitated seq_term' l
  where
    seq_term' (tm, l) = tm `seq` seqLevitated (seqKnownShape seqTerm) l

-- Reasons for design:
--  * We want to share between Cpr and Termination, so KnownShape
--  * Cpr is different from Termination in that we give up once one result
--    isn't constructed
--  * That is: For Termination we might or might not have nested info,
--    independent of termination of the current level. This is why Maybe
--    So, i.e. when we return a function (or newtype there-of) we'd have
--    something like @Termination Terminates Nothing@. We know evaluation
--    terminates, but we don't have any information on shape.
--    In fact, it's the same as
--  * Factoring Termination this way (i.e., TerminationFlag x shape) means less
--    duplication
-- Alternative: Interleave everything. Looks like this:
-- data Blub
--   = NoCpr Termination
--   | Cpr TerminationFlag (KnownShape Blub)
--   | BotBlub
--  + More compact
--  + No Maybe (well, not here, still in Termination)
--  + Easier to handle in WW: Termination and Cpr encode compatible shape info
--    by construction
--  - Harder to understand: NoCpr means we can still have Termination info
--  - Spreads Termination stuff between two lattices
-- ... Probably not such a good idea, after all.

--------
-- * Cpr

-- | The constructed product result lattice.
--
-- @
--      Top
--       |
--   Levitate shape
--       |
--      Bot
-- @
--
-- where @shape@ lifts the same lattice over 'KnownShape'.
newtype Cpr = Cpr (Levitated (KnownShape Cpr))
  deriving (Eq, Binary)

lubCpr :: Cpr -> Cpr -> Cpr
lubCpr (Cpr l1) (Cpr l2) = Cpr (lubLevitated (lubKnownShape lubCpr) l1 l2)

topCpr :: Cpr
topCpr = Cpr Top

botCpr :: Cpr
botCpr = Cpr Bot

conCpr :: ConTag -> [Cpr] -> Cpr
conCpr t args = Cpr (Levitate (Con t args))

trimCpr :: Cpr -> Cpr
trimCpr (Cpr (Levitate Con{})) = topCpr
trimCpr cpr                    = cpr

pruneDeepCpr :: Int -> Cpr -> Cpr
pruneDeepCpr depth (Cpr (Levitate sh)) = Cpr (pruneKnownShape pruneDeepCpr depth sh)
pruneDeepCpr _     cpr                 = cpr

asConCpr :: Termination -> Cpr -> Maybe (ConTag, [Termination], [Cpr])
asConCpr term (Cpr (Levitate (Con t cprs)))
  | Terminates <- tm = Just (t, terms, cprs)
  where
    (tm, l_sh) = splitTermination term
    terms = case l_sh of
      Bot                     -> zipWith const (repeat botTerm) cprs
      Levitate (Con t2 terms) -> ASSERT( t == t2 )
                                 ASSERT( cprs `equalLength` terms )
                                 terms
      _                       -> zipWith const (repeat topTerm) cprs
asConCpr _ _         = Nothing

seqCpr :: Cpr -> ()
seqCpr (Cpr l) = seqLevitated (seqKnownShape seqCpr) l

------------
-- * CprType

-- TODO: Maybe formulate CprType like this?
-- data CprType' = TerminatingCall CprType | Ret Cpr Termination
--   deriving Eq
-- data CprType = CT (Levitated CprType')
--   deriving Eq

-- | The abstract domain \(A_t\) from the original 'CPR for Haskell' paper.
data CprType
  = CprType
  { ct_args :: ![ArgStr]    -- ^ Assumptions about how strict incoming value
                            --   arguments are used, before returning the
                            --   'ct_cpr' and 'ct_term'
  , ct_cpr  :: !Cpr         -- ^ 'Cpr' eventually unleashed when applied to
                            --   'ct_args' arguments
  , ct_term :: !Termination -- ^ 'Termination' unleashed when applied to
                            --   'ct_args' arguments
  }

instance Eq CprType where
  a == b =  ct_cpr a  == ct_cpr b
         && ct_term a == ct_term b
         && (ct_args a == ct_args b || isTopCprType a)

isTopCprType :: CprType -> Bool
isTopCprType (CprType _ cpr term) = cpr == topCpr && term == topTerm

-- | Is this ultimately 'botCprType' when applied to enough arguments?
isUltimatelyBotCprType :: CprType -> Bool
isUltimatelyBotCprType (CprType _ cpr term) = cpr == botCpr && term == botTerm

topCprType :: CprType
topCprType = CprType [] topCpr topTerm

botCprType :: CprType
botCprType = CprType [] botCpr botTerm

extractArgCprAndTermination :: [CprType] -> [(Cpr, Termination)]
extractArgCprAndTermination = map go
  where
    go (CprType [] cpr term) = (cpr, term)
    -- we didn't give it enough arguments, so terminates rapidly
    go _                    = (topCpr, whnfTerm)

conCprType :: ConTag -> [CprType] -> CprType
conCprType con_tag args = CprType [] (conCpr con_tag cprs) (conTerm Terminates con_tag terms)
  where
    (cprs, terms) = unzip (extractArgCprAndTermination args)

markConCprType :: ConTag -> [CprType] -> CprType -> CprType
markConCprType con_tag args ty = ASSERT( null (ct_args ty) ) ty { ct_cpr = conCpr con_tag cprs }
  where
    cprs = map fst (extractArgCprAndTermination args)

lubCprType :: CprType -> CprType -> CprType
lubCprType ty1@(CprType n1 cpr1 term1) ty2@(CprType n2 cpr2 term2)
  -- Note that argument demands are handled contravariantly, hence glbArgStr
  -- The arity of bottom types can be extended arbitrarily (by strTop).
  | isUltimatelyBotCprType ty1 && (n1 `leLength` n2) = glbArgs ty2 n1
  | isUltimatelyBotCprType ty2 && (n2 `leLength` n1) = glbArgs ty1 n2
  -- There might be non-bottom CPR types with mismatching arities.
  -- Consider test DmdAnalGADTs. We want to return topCpr in these cases.
  -- But at the same time, we have to preserve strictness obligations wrt.
  -- Termination. Returning topCprType is a safe default.
  | n1 `equalLength` n2
  = CprType (zipWith glbArgStr n1 n2) (lubCpr cpr1 cpr2) (lubTerm term1 term2)
  | otherwise
  = topCprType
  where
    glbArgs ty args = ty { ct_args = zipWith glbArgStr (ct_args ty) (args ++ repeat strTop) }

applyCprTy :: CprType -> (ArgStr, CprType)
applyCprTy ty@(CprType [] _ _)
  | ty == botCprType = (strTop, botCprType)
  | otherwise        = (strBot, topCprType)
applyCprTy (CprType (a:args) cpr term)
  = (a, CprType args cpr term)

abstractCprTy :: ArgStr -> CprType -> CprType
abstractCprTy = abstractCprTyNTimes . (:[])

abstractCprTyNTimes :: [ArgStr] -> CprType -> CprType
abstractCprTyNTimes n ty@(CprType m cpr term)
  | isTopCprType ty = topCprType
  | otherwise       = CprType (n++m) cpr term

ensureCprTyArity :: Arity -> CprType -> CprType
ensureCprTyArity n ty@(CprType m _ _)
  | m `lengthIs` n = ty
  | otherwise      = topCprType

trimCprTy :: CprType -> CprType
trimCprTy (CprType arty cpr term) = CprType arty (trimCpr cpr) term

-- | Forces possibly deep 'Termination' info of a 'CprType' according to
-- incoming 'ArgStr'. If there's any possibility that this 'MightDiverge',
-- return that.
forceCprTy :: ArgStr -> CprType -> (TerminationFlag, CprType)
-- TODO: This doesn't consider strict fields yet, I think
forceCprTy arg_str ty = force_term_ty (toStrDmd arg_str) ty
  where
    force_term_ty :: (Str (), StrDmd) -> CprType -> (TerminationFlag, CprType)
    force_term_ty (Lazy, _) ty = (botTermFlag, ty)
    force_term_ty (_, str) (CprType [] cpr term) = (flag, CprType [] cpr term')
      where
        (flag, term') = forceTerm (Str str) term
    force_term_ty (_, str) ty = (flag, abstractCprTy arg ty_forced)
      where
        (arg, ty_applied) = applyCprTy ty
        (flag, ty_forced) = force_term_ty (swap (peelStrCall str)) ty_applied

forceTerm :: ArgStr -> Termination -> (TerminationFlag, Termination)
forceTerm arg_str (Termination l) = (flag, Termination l')
  where
    (flag, l') = force_term' arg_str l

    force_term' :: ArgStr -> Levitated Termination' -> (TerminationFlag, Levitated Termination')
    force_term' _                  Bot                   = (botTermFlag, Bot) -- everything Terminates anyway
    force_term' Lazy               t                     = (botTermFlag, t) -- lazy = not forced = Terminates
    force_term' (Str (SCall _))    t                     = (topTermFlag, t) -- we have no info about more incoming arguments
    force_term' (Str HyperStr)     _                     = (topTermFlag, Bot) -- dito; botTerm is already handled above
    force_term' (Str HeadStr)      (Levitate (tm, l_sh)) = (tm, liftTermination' botTermFlag l_sh)
    force_term' (Str HeadStr)      Top                   = force_term' (Str HeadStr) (Levitate (topTermFlag, Top))
    force_term' (Str (SProd args)) (Levitate (tm, l_sh)) = (tm `lubTermFlag` flag, liftTermination' botTermFlag l_sh')
      where
        (flag, l_sh') = force_shape fIRST_TAG args l_sh
    force_term' (Str (SProd args)) Top                   = force_term' (Str (SProd args)) (Levitate (topTermFlag, Top))

    force_shape :: ConTag -> [ArgStr] -> Levitated (KnownShape Termination) -> (TerminationFlag, Levitated (KnownShape Termination))
    force_shape t arg_strs Top = force_shape t arg_strs (Levitate (Con t (repeat topTerm)))
    force_shape t arg_strs (Levitate (Con t' arg_terms))
      | t == t' = (lubTermFlags flags, sh')
      where
        (flags, shs') = unzip (zipWith forceTerm arg_strs arg_terms)
        sh' -- Yuck, we should have smart constructor for that
          | all (== topTerm) shs' = Top
          | all (== botTerm) shs' = Bot
          | otherwise             = Levitate (Con t shs')
    force_shape _ _        sh = (botTermFlag, sh) -- We don't currently record strictness in Sums, so no need to force.

-- | 'lubTerm's the given outer @TerminationFlag@ on the @CprType@s 'ct_term'.
bothCprType :: CprType -> TerminationFlag -> CprType
-- deepTerm because we only want to affect the WHNF layer.
-- If tm = Terminates, it's just 'id'.
-- If tm = MightDiverge, it will only set the WHNF layer to MightDiverge,
-- leaving nested termination info (e.g. on product components) intact.
bothCprType ct tm = ct { ct_term = ct_term ct `lubTerm` deepTerm tm }

seqCprType :: CprType -> ()
seqCprType (CprType args cpr term) = seqArgStrList args `seq` seqCpr cpr `seq` seqTerm term

--------------
-- * CprSig

-- | The arity of the wrapped 'CprType' is the arity at which it is safe
-- to unleash. See Note [Understanding DmdType and StrictSig] in "GHC.Types.Demand"
newtype CprSig = CprSig { getCprSig :: CprType }
  deriving (Eq, Binary)

-- | Turns a 'CprType' computed for the particular 'Arity' into a 'CprSig'
-- unleashable at that arity. See Note [Understanding DmdType and StrictSig] in
-- "GHC.Types.Demand"
mkCprSigForArity :: Arity -> CprType -> CprSig
mkCprSigForArity arty ty = CprSig (ensureCprTyArity arty ty)

topCprSig :: CprSig
topCprSig = CprSig topCprType

mkCprSig :: Arity -> Termination -> Cpr -> CprSig
mkCprSig arty term cpr = CprSig (CprType (replicate arty strTop) cpr term)

seqCprSig :: CprSig -> ()
seqCprSig (CprSig sig) = seqCprType sig `seq` ()

-- | Get a 'CprType' for a 'DataCon', given 'CprType's for its fields.
cprTransformDataConSig :: DataCon -> [CprType] -> CprType
cprTransformDataConSig con args
  | isDataTyCon tycon     -- Real data types only; that is,
                          -- not unboxed tuples or newtypes
  , null (dataConExTyCoVars con)  -- No existentials
  , wkr_arity > 0
  , wkr_arity <= mAX_CPR_SIZE
  , args `lengthIs` wkr_arity
  -- , pprTrace "cprTransformDataConSig" (ppr con <+> ppr wkr_arity <+> ppr args) True
  = abstractCprTyNTimes arg_strs $ conCprType (dataConTag con) args
  | otherwise -- TODO: Refl binds a coercion. What about these? can we CPR them? I don't see why we couldn't.
  = topCprType
  where
    tycon     = dataConTyCon con
    wkr_arity = dataConRepArity con
    arg_strs  = take wkr_arity (repeat strTop) -- worker is all lazy
    -- Note how we don't handle unlifted args here. That's OK by the let/app
    -- invariant, which specifies that the things we forget to force are ok for
    -- speculation, so exactly what we mean by Terminates.

    mAX_CPR_SIZE :: Arity
    mAX_CPR_SIZE = 10
    -- We do not treat very big tuples as CPR-ish:
    --      a) for a start we get into trouble because there aren't
    --         "enough" unboxed tuple types (a tiresome restriction,
    --         but hard to fix),
    --      b) more importantly, big unboxed tuples get returned mainly
    --         on the stack, and are often then allocated in the heap
    --         by the caller.  So doing CPR for them may in fact make
    --         things worse.

-- | Produces 'CprType's the termination info of which match the given
-- strictness signature. Examples:
--
--   - A head-strict demand @S@ would translate to @#@, a
--   - A tuple demand @S(S,L)@ would translate to @#(#,*)@
--   - A call demand @C(S)@ would translate to @strTop -> #(#,*)@
argCprTypesFromStrictSig :: StrictSig -> [CprType]
argCprTypesFromStrictSig sig = arg_tys
  where
    arg_strs = map getStrDmd (fst (splitStrictSig sig))
    arg_tys  = zipWith ((snd .) . forceCprTy) arg_strs (repeat topCprType)

---------------
-- * Outputable

instance Outputable a => Outputable (Levitated a) where
  ppr Bot = char '⊥'
  ppr Top = char 'T'
  ppr (Levitate a) = ppr a

instance Outputable r => Outputable (KnownShape r) where
  ppr (Con t fs) = int t <> pprFields fs

pprFields :: Outputable r => [r] -> SDoc
pprFields fs = parens (pprWithCommas ppr fs)

instance Outputable TerminationFlag where
  ppr MightDiverge = char '*'
  ppr Terminates   = char '#'

instance Outputable Termination where
  ppr (Termination l) = case l of
    Top                           -> char '*'
    Bot                           -> text "#.."
    Levitate (tm, Top)            -> ppr tm
    Levitate (tm, Bot)            -> ppr tm <> text "(#..)"
    Levitate (tm, Levitate shape) -> ppr tm <> ppr shape

instance Outputable Cpr where
  ppr (Cpr l) = case l of
    Top            -> char '-'
    Bot            -> char 'b'
    Levitate shape -> ppr shape

instance Outputable CprType where
  ppr (CprType arty cpr term) = ppr arty <+> ppr cpr <+> ppr term

-- | Only print the CPR result
instance Outputable CprSig where
  ppr (CprSig ty) = ppr (ct_term ty) <+> ppr (ct_cpr ty)

-----------
-- * Binary

instance Binary a => Binary (Levitated a) where
  put_ bh Bot          = putByte bh 0
  put_ bh (Levitate a) = do { putByte bh 1; put_ bh a }
  put_ bh Top          = putByte bh 2
  get  bh = do
    h <- getByte bh
    case h of
      0 -> return Bot
      1 -> Levitate <$> get bh
      2 -> return Top
      _ -> pprPanic "Binary Levitated: Invalid tag" (int (fromIntegral h))

instance Binary r => Binary (KnownShape r) where
  -- Note that the ConTag is 1-indexed
  put_ bh (Con t fs)   = do { put_ bh t; put_ bh fs}
  get  bh = Con <$> get bh <*> get bh

instance Binary TerminationFlag where
  put_ bh MightDiverge = put_ bh False
  put_ bh Terminates   = put_ bh True
  get  bh = do
    b <- get bh
    if b
      then pure Terminates
      else pure MightDiverge

instance Binary CprType where
  put_ bh (CprType args cpr term) = do
    put_ bh args
    put_ bh cpr
    put_ bh term
  get  bh = CprType <$> get bh <*> get bh <*> get bh
