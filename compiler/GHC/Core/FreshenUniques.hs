{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

module GHC.Core.FreshenUniques ( freshenUniques ) where

import GHC.Prelude

import GHC.Core hiding (mkLet)
import GHC.Core.Seq
import GHC.Core.Utils
import GHC.Core.Coercion hiding (substCo, substCoVar, substCoVarBndr) --(CvSubstEnv, coercionKind, coercionLKind, mkCoVarCo, coVarKindsTypesRole, mkCoercionType, mkNomReflCo, mkGReflCo, mkTyConAppCo, mkAppCo, mkForAllCo, mkFunCo, mkAxiomInstCo, mkUnivCo)
import GHC.Core.Type (TvSubstEnv, mkAppTy, mkTyConApp, mkCastTy)
import GHC.Core.Subst (IdSubstEnv)
import GHC.Types.Var.Env (isEmptyVarEnv, emptyVarEnv, lookupVarEnv, delVarEnv, extendVarEnv)
import GHC.Types.Var.Set
import GHC.Core.TyCo.Rep
import GHC.Core.TyCo.FVs
import GHC.Core.FVs

import GHC.Types.Name hiding (varName)
import GHC.Types.Var
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.Tickish
import GHC.Types.Unique
import GHC.Data.Pair
import GHC.Data.SmallArray
import Data.Word

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Misc
import GHC.Data.Maybe

import GHC.IO hiding (liftIO)
import System.IO
import GHC.Exts

import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class

import GHC.Utils.Trace
_ = pprTrace -- Tired of commenting out the import all the time

-- | A simple wrapper around 'SmallArray' implementing Array Doubling.
data GrowableArray a = GA !Int !(SmallMutableArray RealWorld a)

newGrowableArray :: IO (GrowableArray a)
newGrowableArray = GA 0 <$> IO (newSmallArray 2 (error "newGrowableArray"))

insertGrowableArray :: GrowableArray a -> Int -> a -> IO (GrowableArray a)
insertGrowableArray (GA size arr) idx a = do
  capacity <- IO (getSizeSmallArray arr)
  arr' <-
    if size < capacity
      then pure arr
      else IO (resizeSmallArray arr (size*2) (error "pushBack"))
  IO (copySmallArray arr' idx arr' (idx+1) (size-idx))
  IO (writeSmallArray arr' idx a)
  pure $! GA (size+1) arr'

readGrowableArray :: GrowableArray a -> Int -> IO a
readGrowableArray (GA _ arr) i = IO $ readSmallArray arr i

writeGrowableArray :: GrowableArray a -> Int -> a -> IO ()
writeGrowableArray (GA _ arr) i a = IO $ writeSmallArray arr i a

growableArrayElems :: GrowableArray a -> IO [a]
growableArrayElems arr@(GA n _) = traverse (readGrowableArray arr) [0..n-1]

type Key = Word64
type Prefix = Key
type Mask = Key
type Bitmap = Word64

data AMT a
  = Empty
  | Leaf !Key !a
  | Inner !Prefix !Mask !Bitmap !(GrowableArray (AMT a))
  | Full !Prefix !Mask !(SmallMutableArray RealWorld (AMT a))

-- * Bit twiddling

bitsPerSubKey :: Int
bitsPerSubKey = 6

subkeyMask :: Mask
subkeyMask = bit bitsPerSubKey - 1 -- 6 bits => 63 = 0b0111111

-- | Does the key @i@ match the prefix @p@ (up to but not including
-- Mask @m@)?
match :: Key -> Prefix -> Mask -> Ordering
match i p m
  | clz == 0  = EQ
  | otherwise = compare (shift i) (shift p)
  where
    clz = countLeadingZeros m
    -- shifting right by >= `finiteBitSize m` is undefined
    shift w = unsafeShiftR w (finiteBitSize m - clz)
{-# INLINE match #-}

-- |
-- >>> longestCommonPrefix 0 1
-- (0,63)
-- >>> longestCommonPrefix 128 1
-- (0,4032)
-- >>> longestCommonPrefix 128 129
-- (128,63)
-- >>> longestCommonPrefix 4096 4224
-- (4096,4032)
longestCommonPrefix :: Prefix -> Prefix -> (Prefix, Mask)
longestCommonPrefix p1 p2 = (lcp, mask)
  where
    diff = p1 `xor` p2
    firstDivergence = log2 diff
    -- `firstDivergence` is the index of the most significant
    -- diverging bit counting from the right (e.g. the least
    -- significant side).
    -- In order not to run into complicated cases requiring
    -- inefficient merging of inner nodes, we floor the
    -- `firstDivergence` to the next multiple of `bitsPerSubKey`,
    -- which then is the number of bits to shift `mask0` by.
    floorToNextMultipleOf n i = (i `div` n) * n
    shift = floorToNextMultipleOf bitsPerSubKey firstDivergence
    mask = unsafeShiftL subkeyMask shift
    -- We can get the lcp by masking with all bits above the mask
    lcp = p1 .&. complement (bit (shift + bitsPerSubKey) - 1)
{-# INLINE longestCommonPrefix #-}

-- | Given a 'Word64' \(w\) this function computes an 'Int' \(b\) such that
-- \(2^b <= w < 2^(b+1)\). Note the exceptional case for @log2 0@.
--
-- >>> log2 8
-- 3
-- >>> log2 42
-- 5
-- >>> log2 0
-- -1
log2 :: Word64 -> Int
log2 w = finiteBitSize w - 1 - countLeadingZeros w
{-# INLINE log2 #-}

-- | Masks the given 'Key' with the 'Mask' and shifts the result to the right
-- according to the number of trailing zeros in the mask.
-- Effectively, this reads out the masked bits and \'converts\' them into a
-- number.
--
-- >>> maskedBitsUnshifted 0b10101010 0b00011100 == 0b00000010
-- True
maskedBitsUnshifted :: Key -> Mask -> Int
maskedBitsUnshifted k m =
  fromIntegral (unsafeShiftR (k .&. m) (countTrailingZeros m))
{-# INLINE maskedBitsUnshifted #-}

-- | @sparseIndex bm i@ counts the number of ones in @bm@ up to, and
-- including, the @i@th bit (counting 1-based).
--
-- >>> sparseIndex 0b10110110 0
-- 0
-- >>> sparseIndex 0b10110110 1
-- 0
-- >>> sparseIndex 0b10110110 2
-- 1
-- >>> sparseIndex 0b10110110 3
-- 2
-- >>> sparseIndex 0b10110110 4
-- 2
-- >>> sparseIndex 0b10110110 5
-- 3
sparseIndex :: Bitmap -> Int -> Int
sparseIndex bm i = popCount (bm .&. (bit i - 1))
{-# INLINE sparseIndex #-}

data MismatchReason
  = PrefixDiverges !Bool -- ^ True <=> Key was less than Prefix, False <=> Key was greater than Prefix
  | NotAChild !Int
  deriving (Eq, Show)

throwUnlessM :: e -> Bool -> Except e ()
throwUnlessM ex False = throwE ex
throwUnlessM _ _      = pure ()

-- |
-- Key is larger than prefix:
-- >>> computeChildIndex 0b100 0b000 0b011
-- Left False
--
-- Key is smaller than prefix:
-- >>> computeChildIndex 0b000 0b100 0b011
-- Left True
--
-- Key matches prefix:
-- >>> computeChildIndex 0b000 0b000 0b011
-- Just 0
-- >>> computeChildIndex 0b001 0b000 0b011
-- Just 1
-- >>> computeChildIndex 0b010 0b000 0b011
-- Just 2
-- >>> computeChildIndex 0b011 0b000 0b011
-- Just 3
computeChildIndex :: Key -> Prefix -> Mask -> Either Bool Int
computeChildIndex k p m = runExcept $ do
  let ord = match k p m
  throwUnlessM (ord == LT) (match k p m == EQ)
  pure $! maskedBitsUnshifted k m
{-# INLINE computeChildIndex #-}

-- |
-- >>> computeSparseChildIndex 0b100 0b000 0b011 0
-- Left PrefixDiverges
-- >>> computeSparseChildIndex 0b000 0b100 0b011 0
-- Left PrefixDiverges
-- >>> computeSparseChildIndex 0b000 0b000 0b011 0b0101
-- Right 0
-- >>> computeSparseChildIndex 0b001 0b000 0b011 0b0101
-- Left (NotAChild 1)
-- >>> computeSparseChildIndex 0b010 0b000 0b011 0b0101
-- Right 1
-- >>> computeSparseChildIndex 0b011 0b000 0b011 0b0101
-- Left (NotAChild 3)
computeSparseChildIndex :: Key -> Prefix -> Mask -> Bitmap -> Either MismatchReason Int
computeSparseChildIndex k p m bm = runExcept $ do
  i <- withExcept PrefixDiverges (except (computeChildIndex k p m))
  throwUnlessM (NotAChild i) (testBit bm i)
  pure (sparseIndex bm i)
{-# INLINE computeSparseChildIndex #-}

-- * Operations

emptyAMT :: AMT a
emptyAMT = Empty

lookupAMT :: Key -> AMT a -> IO (Maybe a)
lookupAMT !k = go
  where
    go n = case n of
      Leaf k' v
        | k' == k
        -> pure $ Just v
      Inner p m bm cs
        | Right i <- computeSparseChildIndex k p m bm
        -> readGrowableArray cs i >>= go
      Full p m cs
        | Right i <- computeChildIndex k p m
        -> IO (readSmallArray cs i) >>= go
      _ -> pure Nothing

unsafeFindMax :: AMT a -> IO (Maybe (Key, a))
unsafeFindMax Empty      = pure Nothing
unsafeFindMax (Leaf k v) = pure $ Just (k, v)
unsafeFindMax (Full _ _ cs) = IO (readSmallArray cs (fromIntegral subkeyMask)) >>= unsafeFindMax
unsafeFindMax (Inner _ _ _ cs@(GA sz _)) = readGrowableArray cs (sz-1) >>= unsafeFindMax

lookupLT_AMT :: Key -> AMT a -> IO (Maybe (Key, a))
lookupLT_AMT !k = go Empty
  where
    go def n = case n of -- Precondition: all keys in def are less than k and all keys in n
      Empty -> unsafeFindMax def
      Leaf k' v
        | k' < k
        -> pure $ Just (k', v)
        | otherwise
        -> do
          unsafeFindMax def
      Inner p m bm cs -> case computeSparseChildIndex k p m bm of
        Right sparse_i -> do
          def' <- if sparse_i > 0 then readGrowableArray cs (sparse_i-1) else pure def
          readGrowableArray cs sparse_i >>= go def'
        Left (PrefixDiverges True)  -> unsafeFindMax def -- k < p
        Left (PrefixDiverges False) -> unsafeFindMax n   -- k > p
        Left (NotAChild i) -> do
          let !sparse_i = sparseIndex bm i -- We know that i is not present, but this is the index where we'd insert it
          def' <- if sparse_i > 0 then readGrowableArray cs (sparse_i-1) else pure def
          unsafeFindMax def'
      Full p m cs -> case computeChildIndex k p m of
        Right i -> do
          def' <- if i > 0 then IO (readSmallArray cs (i-1)) else pure def
          IO (readSmallArray cs i) >>= go def'
        Left True  -> unsafeFindMax def -- k < p
        Left False -> unsafeFindMax n   -- k > p

link :: Prefix -> AMT a -> Prefix -> AMT a -> IO (AMT a)
link p1 t1 p2 t2 = do
  arr <- newGrowableArray
  arr' <- insertGrowableArray arr 0 c1
  arr'' <- insertGrowableArray arr' 1 c2
  pure $! Inner p m (bit i1 .|. bit i2) arr''
  where
    !(p, m) = longestCommonPrefix p1 p2
    !i1 = maskedBitsUnshifted p1 m
    !i2 = maskedBitsUnshifted p2 m
    !(c1,c2) = if i1 < i2 then (t1, t2) else (t2, t1)
{-# INLINE link #-}

insertInplaceAMT :: Key -> a -> AMT a -> IO (AMT a)
insertInplaceAMT !k v t = case t of
  Empty -> return (Leaf k v)
  Leaf k' _
    | k' == k -> return (Leaf k v)
    | otherwise -> link k (Leaf k v) k' t
  Inner p' m' bm' cs' ->
    case computeSparseChildIndex k p' m' bm' of
      Right si -> do
        child <- readGrowableArray cs' si
        !child' <- insertInplaceAMT k v child
        writeGrowableArray cs' si child'
        return t
      Left (PrefixDiverges _) -> link k (Leaf k v) p' t
      Left (NotAChild i)
        | bm' .|. bit i == complement 0
        , let !si = maskedBitsUnshifted k m'
        -> do
          (GA _ arr) <- insertGrowableArray cs' si (Leaf k v)
          return (Full p' m' arr)
        | let !si = sparseIndex bm' i
        -> do
          cs <- insertGrowableArray cs' si (Leaf k v)
          return (Inner p' m' (bm' .|. bit i) cs)
  Full p' m' cs' -> case computeChildIndex k p' m' of
    Right si -> do
      child <- IO (readSmallArray cs' si)
      !child' <- insertInplaceAMT k v child
      IO (writeSmallArray cs' si child')
      return t
    Left _ -> link k (Leaf k v) p' t

_amtToList :: AMT a -> IO [(Key, a)]
_amtToList n = case n of
  Empty -> pure []
  Leaf k v -> pure [(k, v)]
  Inner _ _ _ cs -> do
    cs' <- growableArrayElems cs
    concat <$> traverse _amtToList cs' -- urgh inefficient
  Full _ _ cs    -> do
    cs' <- traverse (IO . readSmallArray cs) [0..fromIntegral subkeyMask]
    concat <$> traverse _amtToList cs' -- urgh inefficient

amtFromList :: [(Key, a)] -> IO (AMT a)
amtFromList xs = foldM (flip (uncurry insertInplaceAMT)) emptyAMT xs

newtype MUSet a = MUSet (AMT a)

emptyMUSet :: MUSet a
emptyMUSet = MUSet emptyAMT

mkMUSet :: Uniquable a => [a] -> IO (MUSet a)
mkMUSet as = MUSet <$> amtFromList [(fromIntegral $ getKey $ getUnique a, a) | a <- as]

extendMUSet :: Uniquable a => MUSet a -> a -> IO (MUSet a)
extendMUSet (MUSet amt) a = MUSet <$> insertInplaceAMT (fromIntegral $ getKey $ getUnique a) a amt

lookupMUSet :: Uniquable a => MUSet a -> a -> IO (Maybe a)
lookupMUSet (MUSet amt) a = lookupAMT (fromIntegral $ getKey $ getUnique a) amt

elemMUSet :: Uniquable a => a -> MUSet a -> IO Bool
elemMUSet a muset = (not . isNothing) <$> lookupMUSet muset a

----------------------------------------
----------------------------------------
----------------------------------------
----------------------------------------
----------------------------------------
----------------------------------------
----------------------------------------
----------------------------------------

type M a = ReaderT Subst (StateT InScopeSet IO) a

-- | Gives fresh uniques to all 'Var's ocurring in terms of the 'CoreProgram'.
-- It works by bringing all 'Var's into scope at once through calls to
-- 'substBndr'.
freshenUniques :: CoreProgram -> CoreProgram
freshenUniques prog = unsafePerformIO $ evalStateT (runReaderT (freshenTopBinds prog) emptySubst) emptyInScopeSet

freshenTopBinds :: [CoreBind] -> M [CoreBind]
freshenTopBinds binds = do
  -- The scoping semantics of top-level bindings are quite surprising;
  -- All bindings are brought into scope at the beginning. Hence they
  -- mustn't shadow each other.
  -- See also https://gitlab.haskell.org/ghc/ghc/-/issues/19529
  let bs = bindersOfBinds binds
  -- ... hence we bring them all into scope here, without substituting anything.
  in_scope <- liftIO $ mkInScopeSet <$> mkMUSet bs
  lift $ put $! in_scope
  -- And we can be sure that no shadowing has happened so far, hence the assert:
  -- massertPpr (sizeUSet (getInScopeVars in_scope) == length bs)
  --            (hang (text "Non-unique top-level Id(s)!") 2 $
  --              ppr (filter (\grp -> length grp > 1) (List.group bs)))
  local (`setInScope` in_scope) $ do
    binds' <- traverse freshenTopBind binds
    -- (InScope (MUSet amt)) <- lift $ get
    -- vars <- liftIO $ amtToList amt
    -- pprTraceM "freshen" (ppr (length vars))
    return binds'

{-# INLINE samePtr #-}
samePtr :: a -> a -> Bool
samePtr !a !b = case reallyUnsafePtrEquality# a b of
  0# -> False
  _  -> True

{-# INLINE mkNonRec #-}
mkNonRec :: CoreBind -> Id -> CoreExpr -> CoreBind
mkNonRec old_bind new_b new_rhs = case old_bind of
  NonRec old_b old_rhs
    | samePtr old_b new_b, samePtr old_rhs new_rhs
    -> old_bind
  _ -> NonRec new_b new_rhs

mkRec :: CoreBind -> [(Id, CoreExpr)] -> CoreBind
mkRec old_bind new_pairs = case old_bind of
  Rec old_pairs
    | samePtr old_pairs new_pairs
    -> old_bind
  _ -> Rec new_pairs

{-# INLINE mkPair #-}
mkPair :: (a, b) -> a -> b -> (a, b)
mkPair old_p@(old_a, old_b) new_a new_b
  | samePtr old_a new_a, samePtr old_b new_b = old_p
  | otherwise                                = (new_a, new_b)

{-# INLINE mkCons #-}
mkCons :: [a] -> a -> [a] -> [a]
mkCons old_xs new_x new_xs' = case old_xs of
  old_x : old_xs'
    | samePtr old_x new_x, samePtr old_xs' new_xs'
    -> old_xs
  _ -> new_x:old_xs

-- You should get the idea by now:
{-# INLINE mkCoercion #-}
mkCoercion :: CoreExpr -> Coercion -> CoreExpr
mkCoercion old_e new_co = case old_e of Coercion old_co | samePtr old_co new_co -> old_e; _ -> Coercion new_co
{-# INLINE mkType #-}
mkType :: CoreExpr -> Type -> CoreExpr
mkType old_e new_ty = case old_e of Type old_ty | samePtr old_ty new_ty -> old_e; _ -> Type new_ty
{-# INLINE mkTicked #-}
mkTicked :: CoreExpr -> CoreTickish -> CoreExpr -> CoreExpr
mkTicked old_e new_t new_e' = case old_e of Tick old_t old_e' | samePtr old_t new_t, samePtr old_e' new_e' -> old_e; _ -> Tick new_t new_e'
{-# INLINE mkCasted #-}
mkCasted :: CoreExpr -> CoreExpr -> Coercion -> CoreExpr
mkCasted old_e new_e' new_co = case old_e of Cast old_e' old_co | samePtr old_e' new_e', samePtr old_co new_co -> old_e; _ -> Cast new_e' new_co
{-# INLINE mkApp #-}
mkApp :: CoreExpr -> CoreExpr -> CoreExpr -> CoreExpr
mkApp old_e new_f new_a = case old_e of App old_f old_a | samePtr old_f new_f, samePtr old_a new_a -> old_e; _ -> App new_f new_a
{-# INLINE mkLam #-}
mkLam :: CoreExpr -> CoreBndr -> CoreExpr -> CoreExpr
mkLam old_e new_b new_body = case old_e of Lam old_b old_body | samePtr old_b new_b, samePtr old_body new_body -> old_e; _ -> Lam new_b new_body
{-# INLINE mkLet #-}
mkLet :: CoreExpr -> CoreBind -> CoreExpr -> CoreExpr
mkLet old_e new_b new_body = case old_e of Let old_b old_body | samePtr old_b new_b, samePtr old_body new_body -> old_e; _ -> Let new_b new_body
{-# INLINE mkCase #-}
mkCase :: CoreExpr -> CoreExpr -> Id -> Type -> [CoreAlt] -> CoreExpr
mkCase o_e n_scrut n_b n_ty n_alts = case o_e of Case o_scrut o_b o_ty o_alts | smp o_scrut n_scrut, smp o_b n_b, smp o_ty n_ty, smp o_alts n_alts  -> o_e; _ -> Case n_scrut n_b n_ty n_alts
  where smp a b = samePtr a b
{-# INLINE mkAlt #-}
mkAlt :: CoreAlt -> AltCon -> [CoreBndr] -> CoreExpr -> CoreAlt
mkAlt o_a n_con n_bs n_rhs = case o_a of Alt o_con o_bs o_rhs | smp o_con n_con, smp o_bs n_bs, smp o_rhs n_rhs -> o_a; _ -> Alt n_con n_bs n_rhs
  where smp a b = samePtr a b

forSameCheck :: Applicative f => [a] -> (a -> f a) -> f [a]
forSameCheck xs f = go xs
  where
    go xs@[] = pure xs
    go xs@(x:xs') = mkCons xs <$> f x <*> go xs'

zipSameCheck :: [(a, b)] -> [a] -> [b] -> [(a, b)]
zipSameCheck old_ps as bs = go old_ps as bs
  where
    go old_ps@(old_p:old_ps') (a:as) (b:bs) = (mkCons old_ps $! mkPair old_p a b) $! go old_ps' as bs
    go _                      _      _      = []

freshenTopBind :: CoreBind -> M CoreBind
-- Binders are already fresh; see freshenTopBinds above
freshenTopBind bind@(NonRec b rhs) = mkNonRec bind b <$!> freshenExpr rhs
freshenTopBind bind@(Rec pairs) = fmap (mkRec bind) $ forSameCheck pairs $ \p@(b, rhs) -> do
  !rhs' <- freshenExpr rhs
  pure $! mkPair p b rhs'

-- | `wrapSubstFunM f ids k` wraps a `substBndrs`-like function `f` such that
--
--   1. The `InScopeSet` in the state of `M` is taken for the substitution of
--      the binders `ids`.
--   2. The extended `Subst` is available in the continuation `k`
--   3. (But after this function exits, the `Subst` is reset, reader-like, with
--      no trace of `ids`)
--   4. After this function exits, the `InScopeSet` is still extended with `ids`.
wrapSubstFunM :: (Subst -> ids -> IO (Subst, ids)) -> ids -> (ids -> M r) -> M r
wrapSubstFunM f ids k = ReaderT $ \subst -> do
  in_scope <- get
  (!subst', !ids') <- liftIO $ f (subst `setInScope` in_scope) ids
  put $! substInScope subst'
  runReaderT (k ids') subst'
{-# INLINE wrapSubstFunM #-}

withSubstBndrM :: Var -> (Var -> M r) -> M r
withSubstBndrM = wrapSubstFunM substBndr
{-# INLINE withSubstBndrM #-}

withSubstBndrsM :: [Var] -> ([Var] -> M r) -> M r
withSubstBndrsM = wrapSubstFunM substBndrs
{-# INLINE withSubstBndrsM #-}

withSubstRecBndrsM :: [Id] -> ([Id] -> M r) -> M r
withSubstRecBndrsM = wrapSubstFunM substRecBndrs
{-# INLINE withSubstRecBndrsM #-}

-- | The binders of the `CoreBind` are \"in scope\" in the
-- continuation.
freshenLocalBind :: CoreBind -> (CoreBind -> M r) -> M r
freshenLocalBind bind@(NonRec b rhs) k = do
  !rhs' <- freshenExpr rhs
  withSubstBndrM b $ \(!b') -> k $! mkNonRec bind b' rhs'
freshenLocalBind bind@(Rec pairs) k = do
  let (bs, rhss) = unzip pairs
  withSubstRecBndrsM bs $ \(!bs') -> do
    !rhss' <- traverse freshenExpr rhss
    k $! mkRec bind $! zipSameCheck pairs bs' rhss'

freshenExpr :: CoreExpr -> M CoreExpr
-- Quite like substExpr, but we freshen binders unconditionally.
-- So maybe this is more like substExpr, if we had that
freshenExpr e@(Coercion co) = do
  subst <- ask
  mkCoercion e <$!> (liftIO $ substCo subst co)
freshenExpr e@(Type t) = do
  subst <- ask
  mkType e <$!> (liftIO $ substTy subst t)
freshenExpr e@Lit{} = pure e
freshenExpr e@(Var v) = do
  subst <- ask
  !e'@(Var v') <- liftIO $ lookupIdSubst subst v
  if v /= v' then pure e' else pure e
freshenExpr e@(Tick t e') = do
  subst <- ask
  t' <- liftIO $ substTickish subst t
  mkTicked e t' <$!> freshenExpr e'
freshenExpr e@(Cast e' co) = do
  subst <- ask
  co' <- liftIO $ substCo subst co
  flip (mkCasted e) co' <$!> freshenExpr e'
freshenExpr e@(App f a) = do
  !f' <- freshenExpr f
  !a' <- freshenExpr a
  pure $! mkApp e f' a'
freshenExpr e@(Lam b body) = withSubstBndrM b $ \(!b') -> do
  !body' <- freshenExpr body
  pure $! mkLam e b' body'
freshenExpr e@(Let b body) = do
  freshenLocalBind b $ \(!b') -> do
    !body' <- freshenExpr body
    pure $! mkLet e b' body'
freshenExpr e@(Case scrut b ty alts) = do
  !scrut' <- freshenExpr scrut
  withSubstBndrM b $ \(!b') -> do
    subst <- ask
    !ty' <- liftIO $ substTy subst ty
    let do_alt alt@(Alt con bs e) = withSubstBndrsM bs $ \(!bs') ->
          mkAlt alt con bs' <$!> freshenExpr e
    !alts' <- traverse do_alt alts
    pure $ mkCase e scrut' b' ty' alts'

{-
************************************************************************
*                                                                      *
                In-scope sets
*                                                                      *
************************************************************************
-}

-- | A set of variables that are in scope at some point.
--
-- Note that this is a /superset/ of the variables that are currently in scope.
-- See Note [The InScopeSet invariant].
--
-- "Secrets of the Glasgow Haskell Compiler inliner" Section 3.2 provides
-- the motivation for this abstraction.
newtype InScopeSet = InScope (MUSet Var)
        -- Note [Lookups in in-scope set]
        -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        -- We store a VarSet here, but we use this for lookups rather than just
        -- membership tests. Typically the InScopeSet contains the canonical
        -- version of the variable (e.g. with an informative unfolding), so this
        -- lookup is useful.
        -- See, for instance, Note [In-scope set as a substitution].

        -- Note [The InScopeSet invariant]
        -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        -- The InScopeSet must include every in-scope variable, but it may also
        -- include other variables.

        -- Its principal purpose is to provide a set of variables to be avoided
        -- when creating a fresh identifier (fresh in the sense that it does not
        -- "shadow" any in-scope binding). To do this we simply have to find one that
        -- does not appear in the InScopeSet. This is done by the key function
        -- GHC.Types.Var.Env.uniqAway.

        -- See "Secrets of the Glasgow Haskell Compiler inliner" Section 3.2
        -- for more detailed motivation. #20419 has further discussion.


emptyInScopeSet :: InScopeSet
emptyInScopeSet = InScope emptyMUSet

--getInScopeVars ::  InScopeSet -> MUSet Var
--getInScopeVars (InScope vs) = vs

mkInScopeSet :: MUSet Var -> InScopeSet
mkInScopeSet in_scope = InScope in_scope

extendInScopeSet :: InScopeSet -> Var -> IO InScopeSet
extendInScopeSet (InScope in_scope) v
   = InScope <$> extendMUSet in_scope v

_elemInScopeSet :: Var -> InScopeSet -> IO Bool
_elemInScopeSet v (InScope in_scope) = v `elemMUSet` in_scope

lookupInScopeSet :: InScopeSet -> Var -> IO (Maybe Var)
lookupInScopeSet (InScope in_scope) v = lookupMUSet in_scope v

uniqAway :: InScopeSet -> Var -> IO Var
-- It starts with v's current unique, of course, in the hope that it won't
-- have to change, and thereafter uses the successor to the last derived unique
-- found in the in-scope set.
uniqAway in_scope var = do
  mb_other_var <- lookupInScopeSet in_scope var
  case mb_other_var of
    Nothing -> pure var
    Just other_var -> do
      var' <- uniqAway' in_scope var
      pprTraceM "uniqAway" (ppr other_var $$ ppr var $$ ppr var')
      return var'

uniqAway' :: InScopeSet -> Var -> IO Var
-- This one *always* makes up a new variable
uniqAway' in_scope var
  = setVarUnique var <$> unsafeGetFreshLocalUnique in_scope

-- | @unsafeGetFreshUnique in_scope@ finds a unique that is not in-scope in the
-- given 'InScopeSet'. This must be used very carefully since one can very easily
-- introduce non-unique 'Unique's this way. See Note [Local uniques].
unsafeGetFreshLocalUnique :: InScopeSet -> IO Unique
unsafeGetFreshLocalUnique (InScope (MUSet amt)) = do
  -- NB: The lookupLT below stops working correctly when the key is negative.
  -- Fortunately, that is never the case for local uniques, which have tag 'X'.
  mb_uniq <- lookupLT_AMT (fromIntegral $ getKey maxLocalUnique) amt
  pprTraceM "unsafeGetFreshLocalUnique1" (ppr mb_uniq $$ ppr (fromIntegral $ getKey maxLocalUnique :: Word64) $$ ppr (getKey maxLocalUnique))
  case mb_uniq of
    Just (uniq, _)
      | let uniq' = mkLocalUnique (fromIntegral uniq)
      , not $ uniq' `ltUnique` minLocalUnique
      , pprTrace "unsafeGetFreshLocalUnique" (ppr uniq $$ ppr uniq' $$ ppr (incrUnique uniq')) True
      -> pure $! incrUnique uniq'
    _ -> pure $ minLocalUnique

{-
************************************************************************
*                                                                      *
\subsection{Substitutions}
*                                                                      *
************************************************************************
-}

-- | A substitution environment, containing 'Id', 'TyVar', and 'CoVar'
-- substitutions.
--
-- Some invariants apply to how you use the substitution:
--
-- 1. Note [The substitution invariant] in "GHC.Core.TyCo.Subst"
--
-- 2. Note [Substitutions apply only once] in "GHC.Core.TyCo.Subst"
data Subst
  = Subst InScopeSet  -- Variables in scope (both Ids and TyVars) /after/
                      -- applying the substitution
          IdSubstEnv  -- Substitution from NcIds to CoreExprs
          TvSubstEnv  -- Substitution from TyVars to Types
          CvSubstEnv  -- Substitution from CoVars to Coercions

        -- INVARIANT 1: See TyCoSubst Note [The substitution invariant]
        -- This is what lets us deal with name capture properly
        -- It's a hard invariant to check...
        --
        -- INVARIANT 2: The substitution is apply-once; see Note [Apply once] with
        --              Types.TvSubstEnv
        --
        -- INVARIANT 3: See Note [Extending the Subst]

isEmptySubst :: Subst -> Bool
isEmptySubst (Subst _ id_env tv_env cv_env)
  = isEmptyVarEnv id_env && isEmptyVarEnv tv_env && isEmptyVarEnv cv_env

emptySubst :: Subst
emptySubst = Subst emptyInScopeSet emptyVarEnv emptyVarEnv emptyVarEnv

setInScope :: Subst -> InScopeSet -> Subst
setInScope (Subst _ ids tvs cvs) in_scope = Subst in_scope ids tvs cvs

-- | Find the in-scope set: see "GHC.Core.TyCo.Subst" Note [The substitution invariant]
substInScope :: Subst -> InScopeSet
substInScope (Subst in_scope _ _ _) = in_scope

-- | Find the substitution for an 'Id' in the 'Subst'
lookupIdSubst :: Subst -> Id -> IO CoreExpr
lookupIdSubst (Subst in_scope ids _ _) v
  | not (isLocalId v) = pure $ Var v
  | Just e  <- lookupVarEnv ids v = pure e
  | otherwise = do
      mb_v <- lookupInScopeSet in_scope v
      pure $! case mb_v of
        Just v -> Var v
        -- Vital! See Note [Extending the Subst]
        -- If v isn't in the InScopeSet, we panic, because
        -- it's a bad bug and we reallly want to know
        Nothing -> pprPanic "lookupIdSubst" (ppr v)

{-
************************************************************************
*                                                                      *
        Substituting expressions
*                                                                      *
************************************************************************
-}

-- | substExpr applies a substitution to an entire 'CoreExpr'. Remember,
-- you may only apply the substitution /once/:
-- See Note [Substitutions apply only once] in "GHC.Core.TyCo.Subst"
--
-- Do *not* attempt to short-cut in the case of an empty substitution!
-- See Note [Extending the Subst]
substExpr :: Subst -> CoreExpr -> IO CoreExpr
   -- HasDebugCallStack so we can track failures in lookupIdSubst
substExpr subst expr
  = go expr
  where
    go (Var v)         = lookupIdSubst subst v
    go (Type ty)       = Type <$> substTy subst ty
    go (Coercion co)   = Coercion <$> substCo subst co
    go (Lit lit)       = pure $! Lit lit
    go (App fun arg)   = App <$> go fun <*> go arg
    go (Tick tickish e) = mkTick <$> substTickish subst tickish <*> go e
    go (Cast e co)     = Cast <$> go e <*> substCo subst co
       -- Do not optimise even identity coercions
       -- Reason: substitution applies to the LHS of RULES, and
       --         if you "optimise" an identity coercion, you may
       --         lose a binder. We optimise the LHS of rules at
       --         construction time

    go (Lam bndr body) = do
      (subst', bndr') <- substBndr subst bndr
      Lam bndr' <$> substExpr subst' body


    go (Let bind body) = do
      (subst', bind') <- substBind subst bind
      Let bind' <$> substExpr subst' body

    go (Case scrut bndr ty alts) = do
      (subst', bndr') <- substBndr subst bndr
      Case <$> go scrut <*> pure bndr' <*> substTy subst ty <*> traverse (go_alt subst') alts


    go_alt subst (Alt con bndrs rhs) = do
      (subst', bndrs') <- substBndrs subst bndrs
      Alt con bndrs' <$> substExpr subst' rhs

-- | Apply a substitution to an entire 'CoreBind', additionally returning an updated 'Subst'
-- that should be used by subsequent substitutions.
substBind :: Subst -> CoreBind -> IO (Subst, CoreBind)
substBind subst (NonRec bndr rhs) = do
  rhs' <- substExpr subst rhs
  (subst', bndr') <- substBndr subst bndr
  pure (subst', NonRec bndr' rhs')

substBind subst (Rec pairs) = do
  let (bndrs, rhss)    = unzip pairs
  (subst', bndrs') <- substRecBndrs subst bndrs
  rhss' <- traverse (substExpr subst') rhss
  pure (subst', Rec (bndrs' `zip` rhss'))

{-
************************************************************************
*                                                                      *
                Types and Coercions
*                                                                      *
************************************************************************

For types and coercions we just call the corresponding functions in
Type and Coercion, but we have to repackage the substitution, from a
Subst to a Subst.
-}

substTyVarBndr :: Subst -> TyVar -> IO (Subst, TyVar)
substTyVarBndr subst tv = substTyVarBndrUsing substTy subst tv

substCoVarBndr :: Subst -> CoVar -> IO (Subst, CoVar)
substCoVarBndr subst cv = substCoVarBndrUsing substTy subst cv

-- | Substitute a tyvar in a binding position, returning an
-- extended subst and a new tyvar.
-- Use the supplied function to substitute in the kind
substTyVarBndrUsing
  :: (Subst -> Type -> IO Type)  -- ^ Use this to substitute in the kind
  -> Subst -> TyVar -> IO (Subst, TyVar)
substTyVarBndrUsing subst_fn subst@(Subst in_scope ienv tenv cenv) old_var = do
  let
    old_ki = tyVarKind old_var
    no_kind_change = noFreeVarsOfType old_ki -- verify that kind is closed

  new_var <-
    if no_kind_change
      then uniqAway in_scope old_var
      else do
        v <- setTyVarKind old_var <$> subst_fn subst old_ki
        uniqAway in_scope v
        -- The uniqAway part makes sure the new variable is not already in scope
  let
    new_env | no_change = delVarEnv tenv old_var
            | otherwise = extendVarEnv tenv old_var (TyVarTy new_var)
    no_change = no_kind_change && (new_var == old_var)
        -- no_change means that the new_var is identical in
        -- all respects to the old_var (same unique, same kind)
        -- See Note [Extending the Subst]
        --
        -- In that case we don't need to extend the substitution
        -- to map old to new.  But instead we must zap any
        -- current substitution for the variable. For example:
        --      (\x.e) with id_subst = [x |-> e']
        -- Here we must simply zap the substitution for x

  in_scope' <- in_scope `extendInScopeSet` new_var
  pure (Subst in_scope' ienv new_env cenv, new_var)

-- | Substitute a covar in a binding position, returning an
-- extended subst and a new covar.
-- Use the supplied function to substitute in the kind
substCoVarBndrUsing
  :: (Subst -> Type -> IO Type)
  -> Subst -> CoVar -> IO (Subst, CoVar)
substCoVarBndrUsing subst_fn subst@(Subst in_scope ienv tenv cenv) old_var = do
  let
    (_, _, t1, t2, role) = coVarKindsTypesRole old_var
  t1' <- subst_fn subst t1
  t2' <- subst_fn subst t2
  let
    no_kind_change = noFreeVarsOfTypes [t1, t2]

    subst_old_var = mkCoVar (varName old_var) new_var_type

    new_var_type = mkCoercionType role t1' t2'
                  -- It's important to do the substitution for coercions,
                  -- because they can have free type variables
  new_var <- uniqAway in_scope subst_old_var
  let
    no_change      = new_var == old_var && no_kind_change
    new_co         = mkCoVarCo new_var
    new_cenv | no_change = delVarEnv cenv old_var
             | otherwise = extendVarEnv cenv old_var new_co
  in_scope' <- in_scope `extendInScopeSet` new_var
  pure (Subst in_scope' ienv tenv new_cenv, new_var)

-- | Substitute within a 'Type'
-- The substitution has to satisfy the invariants described in
-- Note [The substitution invariant].
substTy :: Subst -> Type  -> IO Type
substTy subst ty
  | isEmptySubst subst = pure ty
  | otherwise             = subst_ty subst ty

subst_ty :: Subst -> Type -> IO Type
-- subst_ty is the main workhorse for type substitution
--
-- Note that the in_scope set is poked only if we hit a forall
-- so it may often never be fully computed
subst_ty subst ty
   = go ty
  where
    go :: Type -> IO Type
    go (TyVarTy tv)      = pure $! substTyVar subst tv
    go (AppTy fun arg)   = do
      !fun' <- go fun
      !arg' <- go arg
      pure $! mkAppTy fun' arg'
                -- The mkAppTy smart constructor is important
                -- we might be replacing (a Int), represented with App
                -- by [Int], represented with TyConApp
    go ty@(TyConApp tc []) = tc `seq` pure ty  -- avoid allocation in this common case
    go (TyConApp !tc tys) = do
      mkTyConApp tc <$!> traverse go tys
                               -- NB: mkTyConApp, not TyConApp.
                               -- mkTyConApp has optimizations.
                               -- See Note [Using synonyms to compress types]
                               -- in GHC.Core.Type
    go ty@(FunTy { ft_mult = mult, ft_arg = arg, ft_res = res }) = do
      !mult' <- go mult
      !arg'  <- go arg
      !res'  <- go res
      pure ty { ft_mult = mult', ft_arg = arg', ft_res = res' }
    go (ForAllTy (Bndr tv vis) ty) = do
      (subst', tv') <- substBndr subst tv
      ty' <- subst_ty subst' ty
      pure $! (ForAllTy $! ((Bndr $! tv') vis)) $! ty'

    go (LitTy !n)        = pure $ LitTy n
    go (CastTy ty co)    = do
      ty' <- go ty
      co' <- subst_co subst co
      pure $! mkCastTy ty' co'
    go (CoercionTy co)   = CoercionTy <$!> subst_co subst co

substTyVar :: Subst -> TyVar -> Type
substTyVar (Subst _ _ tenv _) tv
  = case lookupVarEnv tenv tv of
      Just ty -> ty
      Nothing -> TyVarTy tv

-- | Substitute within a 'Coercion'
-- The substitution has to satisfy the invariants described in
-- Note [The substitution invariant].
substCo :: Subst -> Coercion -> IO Coercion
substCo subst co
  | isEmptySubst subst = pure co
  | otherwise = subst_co subst co

-- | Substitute within a 'Coercion' disabling sanity checks.
-- The problems that the sanity checks in substCo catch are described in
-- Note [The substitution invariant].
-- The goal of #11371 is to migrate all the calls of substCoUnchecked to
-- substCo and remove this function. Please don't use in new code.
substCoUnchecked :: Subst -> Coercion -> IO Coercion
substCoUnchecked subst co
  | isEmptySubst subst = pure co
  | otherwise = subst_co subst co

subst_co :: Subst -> Coercion -> IO Coercion
subst_co subst co
  = go co
  where
    go_ty :: Type -> IO Type
    go_ty = subst_ty subst

    go_mco :: MCoercion -> IO MCoercion
    go_mco MRefl    = pure MRefl
    go_mco (MCo co) = MCo <$!> go co

    go :: Coercion -> IO Coercion
    go (Refl ty)             = mkNomReflCo <$!> (go_ty ty)
    go (GRefl r ty mco)      = do
      !ty'  <- go_ty ty
      !mco' <- go_mco mco
      pure $! mkGReflCo r ty' mco'
    go (TyConAppCo r tc args)= do
      args' <- traverse go args
      args' `seqList` pure $! mkTyConAppCo r tc args'
    go (AppCo co arg)        = do
      !co' <- go co
      !arg' <- go arg
      pure $! mkAppCo co' arg'
    go (ForAllCo tv kind_co co) = do
      (subst', !tv', !kind_co') <- substForAllCoBndrUnchecked subst tv kind_co
      !co' <- subst_co subst' co
      pure $! mkForAllCo tv' kind_co' co'
    go (FunCo r w co1 co2)   = do
      !w'   <- go w
      !co1' <- go co1
      !co2' <- go co2
      pure $! mkFunCo r w' co1' co2'
    go (CoVarCo cv)          = pure $! substCoVar subst cv
    go (AxiomInstCo con ind cos) = do
      mkAxiomInstCo con ind <$!> traverse go cos
    go (UnivCo p r t1 t2)    = do
      !p' <- go_prov p
      !t1' <- go_ty t1
      !t2' <- go_ty t2
      pure $! mkUnivCo p' r t1' t2'
    go (SymCo co)            = mkSymCo <$!> (go co)
    go (TransCo co1 co2)     = do
      !co1' <- go co1
      !co2' <- go co2
      pure $! mkTransCo co1' co2'
    go (NthCo r d co)        =
      mkNthCo r d <$!> (go co)
    go (LRCo lr co)          = mkLRCo lr <$!> (go co)
    go (InstCo co arg)       = do
      !co'  <- go co
      !arg' <- go arg
      pure $! mkInstCo co' arg'
    go (KindCo co)           = mkKindCo <$!> (go co)
    go (SubCo co)            = mkSubCo <$!> (go co)
    go (AxiomRuleCo c cs)    = do
      cs1 <- traverse go cs
      cs1 `seqList` pure $ AxiomRuleCo c cs1
    go (HoleCo h)            = HoleCo <$!> go_hole h

    go_prov (PhantomProv kco)    = PhantomProv <$> (go kco)
    go_prov (ProofIrrelProv kco) = ProofIrrelProv <$> (go kco)
    go_prov p@(PluginProv _)     = pure p
    go_prov p@(CorePrepProv _)   = pure p

    -- See Note [Substituting in a coercion hole]
    go_hole h@(CoercionHole { ch_co_var = cv }) = do
      cv' <- updateVarTypeM go_ty cv
      pure $! h { ch_co_var = cv' }

-- | Like 'substForAllCoBndr', but disables sanity checks.
-- The problems that the sanity checks in substCo catch are described in
-- Note [The substitution invariant].
-- The goal of #11371 is to migrate all the calls of substCoUnchecked to
-- substCo and remove this function. Please don't use in new code.
substForAllCoBndrUnchecked :: Subst -> TyCoVar -> KindCoercion
                           -> IO (Subst, TyCoVar, Coercion)
substForAllCoBndrUnchecked subst
  = substForAllCoBndrUsing False (substCoUnchecked subst) subst

-- See Note [Sym and ForAllCo]
substForAllCoBndrUsing :: Bool  -- apply sym to binder?
                       -> (Coercion -> IO Coercion)  -- transformation to kind co
                       -> Subst -> TyCoVar -> KindCoercion
                       -> IO (Subst, TyCoVar, KindCoercion)
substForAllCoBndrUsing sym sco subst old_var
  | isTyVar old_var = substForAllCoTyVarBndrUsing sym sco subst old_var
  | otherwise       = substForAllCoCoVarBndrUsing sym sco subst old_var

substForAllCoTyVarBndrUsing :: Bool  -- apply sym to binder?
                            -> (Coercion -> IO Coercion)  -- transformation to kind co
                            -> Subst -> TyVar -> KindCoercion
                            -> IO (Subst, TyVar, KindCoercion)
substForAllCoTyVarBndrUsing sym sco (Subst in_scope ienv tenv cenv) old_var old_kind_co = do
  let
    no_kind_change = noFreeVarsOfCo old_kind_co

  new_kind_co <-
    if no_kind_change
      then pure old_kind_co
      else sco old_kind_co

  let
    new_ki1 = coercionLKind new_kind_co
    -- We could do substitution to (tyVarKind old_var). We don't do so because
    -- we already substituted new_kind_co, which contains the kind information
    -- we want. We don't want to do substitution once more. Also, in most cases,
    -- new_kind_co is a Refl, in which case coercionKind is really fast.

  new_var  <- uniqAway in_scope (setTyVarKind old_var new_ki1)
  let
    new_env | no_change && not sym = delVarEnv tenv old_var
            | sym       = extendVarEnv tenv old_var $
                          TyVarTy new_var `CastTy` new_kind_co
            | otherwise = extendVarEnv tenv old_var (TyVarTy new_var)

    no_change = no_kind_change && (new_var == old_var)

  in_scope' <- in_scope `extendInScopeSet` new_var
  pure (Subst in_scope' ienv new_env cenv, new_var, new_kind_co)

substForAllCoCoVarBndrUsing :: Bool  -- apply sym to binder?
                            -> (Coercion -> IO Coercion)  -- transformation to kind co
                            -> Subst -> CoVar -> KindCoercion
                            -> IO (Subst, CoVar, KindCoercion)
substForAllCoCoVarBndrUsing sym sco (Subst in_scope ienv tenv cenv)
                            old_var old_kind_co = do
  let
    no_kind_change = noFreeVarsOfCo old_kind_co
  new_kind_co <-
    if no_kind_change
      then pure old_kind_co
      else sco old_kind_co
  let
    Pair h1 h2 = coercionKind new_kind_co

    new_var_type  | sym       = h2
                  | otherwise = h1
  new_var <- uniqAway in_scope $ mkCoVar (varName old_var) new_var_type
  let
    new_cenv | no_change && not sym = delVarEnv cenv old_var
             | otherwise = extendVarEnv cenv old_var (mkCoVarCo new_var)

    no_change = no_kind_change && (new_var == old_var)
  in_scope' <- in_scope `extendInScopeSet` new_var
  pure (Subst in_scope' ienv tenv new_cenv, new_var, new_kind_co)

substCoVar :: Subst -> CoVar -> Coercion
substCoVar (Subst _ _ _ cenv) cv
  = case lookupVarEnv cenv cv of
      Just co -> co
      Nothing -> CoVarCo cv

{-
************************************************************************
*                                                                      *
\section{IdInfo substitution}
*                                                                      *
************************************************************************
-}

------------------
-- | Substitute into some 'IdInfo' with regard to the supplied new 'Id'.
substIdInfo :: Subst -> Id -> IdInfo -> IO (Maybe IdInfo)
substIdInfo subst new_id info
  | nothing_to_do = pure Nothing
  | otherwise     = do
      new_rules <- unsafeInterleaveIO $ substRuleInfo subst new_id old_rules
      new_unf   <- unsafeInterleaveIO $ substUnfolding subst old_unf
      pure $ Just (info `setRuleInfo` new_rules `setUnfoldingInfo` new_unf)
  where
    old_rules     = ruleInfo info
    old_unf       = realUnfoldingInfo info
    nothing_to_do = isEmptyRuleInfo old_rules && not (hasCoreUnfolding old_unf)

------------------
-- | Substitutes for the 'Id's within an unfolding
-- NB: substUnfolding /discards/ any unfolding without
--     without a Stable source.  This is usually what we want,
--     but it may be a bit unexpected
substUnfolding :: Subst -> Unfolding -> IO Unfolding
        -- Seq'ing on the returned Unfolding is enough to cause
        -- all the substitutions to happen completely

substUnfolding subst df@(DFunUnfolding { df_bndrs = bndrs, df_args = args }) = do
  (subst',bndrs') <- substBndrs subst bndrs
  args'           <- traverse (substExpr subst') args
  pure df { df_bndrs = bndrs', df_args = args' }

substUnfolding subst unf@(CoreUnfolding { uf_tmpl = tmpl, uf_src = src })
        -- Retain an InlineRule!
  | not (isStableSource src)  -- Zap an unstable unfolding, to save substitution work
  = pure NoUnfolding
  | otherwise                 -- But keep a stable one!
  = do
      new_tmpl <- substExpr subst tmpl
      seqExpr new_tmpl `seq` pure unf { uf_tmpl = new_tmpl }

substUnfolding _ unf = pure unf      -- NoUnfolding, OtherCon

------------------
-- | Substitutes for the 'Id's within the 'RuleInfo' given the new function 'Id'
substRuleInfo :: Subst -> Id -> RuleInfo -> IO RuleInfo
substRuleInfo subst new_id (RuleInfo rules rhs_fvs) = do
  let subst_ru_fn = const (idName new_id)
  rules'   <- traverse (substRule subst subst_ru_fn) rules
  rhs_fvs' <- substDVarSet subst rhs_fvs
  pure $ RuleInfo rules' rhs_fvs'

------------------
substRule :: Subst -> (Name -> Name) -> CoreRule -> IO CoreRule

-- The subst_ru_fn argument is applied to substitute the ru_fn field
-- of the rule:
--    - Rules for *imported* Ids never change ru_fn
--    - Rules for *local* Ids are in the IdInfo for that Id,
--      and the ru_fn field is simply replaced by the new name
--      of the Id
substRule _ _ rule@(BuiltinRule {}) = pure rule
substRule subst subst_ru_fn rule@(Rule { ru_bndrs = bndrs, ru_args = args
                                       , ru_fn = fn_name, ru_rhs = rhs
                                       , ru_local = is_local }) = do
  (subst', bndrs') <- substBndrs subst bndrs
  args' <- traverse (substExpr subst') args
  rhs'  <- substExpr subst' rhs
  pure rule { ru_bndrs = bndrs'
            , ru_fn    = if is_local
                           then subst_ru_fn fn_name
                           else fn_name
            , ru_args  = args'
            , ru_rhs   = rhs' }
              -- Do NOT optimise the RHS (previously we did simplOptExpr here)
              -- See Note [Substitute lazily]

------------------
substDVarSet :: Subst -> DVarSet -> IO DVarSet
substDVarSet subst@(Subst _ _ tv_env cv_env) fvs
  = mkDVarSet . fst <$> (foldM subst_fv ([], emptyVarSet) $ dVarSetElems fvs)
  where
  subst_fv :: ([Var], VarSet) -> Var -> IO ([Var], VarSet)
  subst_fv acc fv
     | isTyVar fv
     , let fv_ty = lookupVarEnv tv_env fv `orElse` mkTyVarTy fv
     = pure $! tyCoFVsOfType fv_ty (const True) emptyVarSet $! acc
     | isCoVar fv
     , let fv_co = lookupVarEnv cv_env fv `orElse` mkCoVarCo fv
     = pure $! tyCoFVsOfCo fv_co (const True) emptyVarSet $! acc
     | otherwise
     = do
       fv_expr <- lookupIdSubst subst fv
       pure $! expr_fvs fv_expr isLocalVar emptyVarSet $! acc

------------------
substTickish :: Subst -> CoreTickish -> IO CoreTickish
substTickish subst (Breakpoint ext n ids)
   = Breakpoint ext n <$> traverse do_one ids
 where
    do_one v = getIdFromTrivialExpr <$> lookupIdSubst subst v
substTickish _subst other = pure other

{-
************************************************************************
*                                                                      *
        Substituting binders
*                                                                      *
************************************************************************

Remember that substBndr and friends are used when doing expression
substitution only.  Their only business is substitution, so they
preserve all IdInfo (suitably substituted).  For example, we *want* to
preserve occ info in rules.
-}

-- | Substitutes a 'Var' for another one according to the 'Subst' given, returning
-- the result and an updated 'Subst' that should be used by subsequent substitutions.
-- 'IdInfo' is preserved by this process, although it is substituted into appropriately.
substBndr :: Subst -> Var -> IO (Subst, Var)
substBndr subst bndr
  | isTyVar bndr  = substTyVarBndr subst bndr
  | isCoVar bndr  = substCoVarBndr subst bndr
  | otherwise     = substIdBndr (text "var-bndr") subst subst bndr

-- | Applies 'substBndr' to a number of 'Var's, accumulating a new 'Subst' left-to-right
substBndrs :: Subst -> [Var] -> IO (Subst, [Var])
substBndrs subst [] = pure (subst, [])
substBndrs subst bs@(b:bs') = do
  (!subst', !b') <- substBndr subst b
  (!subst'', !bs'') <- substBndrs subst' bs'
  let !bs''' = mkCons bs b' bs''
  pure (subst'', bs''')

-- | Substitute in a mutually recursive group of 'Id's
substRecBndrs :: Subst -> [Id] -> IO (Subst, [Id])
substRecBndrs subst bndrs = do
  (new_subst, new_bndrs) <- fixIO $ \(~(new_subst, _new_bndrs)) -> do
    let
      go :: Subst -> [Id] -> IO (Subst, [Id])
      go subst [] = pure (subst, [])
      go subst bs@(b:bs') = do
        (subst', b') <- substIdBndr (text "rec-bndr") new_subst subst b
        (subst'', bs'') <- go subst' bs'
        let !bs''' = mkCons bs b' bs''
        pure (subst'', bs''')
    go subst bndrs
  pure (new_subst, new_bndrs)

substIdBndr :: SDoc
            -> Subst            -- ^ Substitution to use for the IdInfo
            -> Subst -> Id      -- ^ Substitution and Id to transform
            -> IO (Subst, Id)      -- ^ Transformed pair
                                -- NB: unfolding may be zapped

substIdBndr _doc rec_subst subst@(Subst in_scope env tvs cvs) old_id = do
  let
    old_ty = idType old_id
    old_w = idMult old_id
    no_type_change = (isEmptyVarEnv tvs && isEmptyVarEnv cvs) ||
                     (noFreeVarsOfType old_ty && noFreeVarsOfType old_w)
  id1 <- uniqAway in_scope old_id      -- id1 is cloned if necessary
  id2 <-
    if no_type_change
      then pure id1
      else updateIdTypeAndMultM (substTy subst) id1
  mb_new_info <- substIdInfo rec_subst id2 (idInfo id2)
  let
        -- new_id has the right IdInfo
        -- The lazy-set is because we're in a loop here, with
        -- rec_subst, when dealing with a mutually-recursive group
    !new_id = maybeModifyIdInfo mb_new_info id2
        -- NB: unfolding info may be zapped

    !new_env | no_change = delVarEnv env old_id
             | otherwise = extendVarEnv env old_id (Var new_id)

    no_change = id1 == old_id
        -- See Note [Extending the Subst]
        -- it's /not/ necessary to check mb_new_info and no_type_change
      -- Extend the substitution if the unique has changed
      -- See the notes with substTyVarBndr for the delVarEnv
  !new_in_scope <- in_scope `extendInScopeSet` new_id
  pure (Subst new_in_scope new_env tvs cvs, new_id)
