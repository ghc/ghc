{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Operations (unfoldrM, drain, postscan, after_, replicate, fold, unfold,
        splitOnSeq) where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Bits (shiftR, shiftL, (.|.), (.&.))
import Data.Word (Word, Word32)
import Fold (Fold(..))
import Foreign.Storable (Storable(..))
import GHC.Types (SPEC(..))
import Array (Array)
import StreamK (IsStream, MonadAsync, adaptState)
import Step (Step(..))
import Unfold (Unfold)
#if defined(FUSION_PLUGIN)
import Fusion.Plugin.Types (Fuse(..))
#endif
import qualified StreamK as K
import qualified StreamD as D
import qualified Serial
import qualified Fold as FL
import qualified Array as A
import qualified Ring as RB
import Prelude hiding (replicate)

{-# INLINE [2] unfoldrM #-}
unfoldrM :: (IsStream t, MonadAsync m) => (b -> m (Maybe (a, b))) -> b -> t m a
unfoldrM = K.unfoldrM

{-# RULES "unfoldrM serial" unfoldrM = unfoldrMSerial #-}
{-# INLINE [2] unfoldrMSerial #-}
unfoldrMSerial :: MonadAsync m => (b -> m (Maybe (a, b))) -> b -> K.Stream m a
unfoldrMSerial = Serial.unfoldrM

{-# INLINE [2] drain #-}
drain :: (Monad m) => K.Stream m a -> m ()
drain m = D.drain $ D.fromStreamK (K.toStream m)
{-# RULES "drain fallback to CPS" [1]
    forall a. D.drain (D.fromStreamK a) = K.drain a #-}

{-# INLINE [1] postscan #-}
postscan :: (IsStream t, Monad m)
    => Fold m a b -> t m a -> t m b
postscan fld m =
    D.fromStreamD $ D.postscanOnce fld $ D.toStreamD m

{-# INLINE [1] replicate #-}
replicate :: (IsStream t, Monad m) => Int -> a -> t m a
replicate n = D.fromStreamD . D.replicate n

{-# INLINE fold_ #-}
fold_ :: Monad m => Fold m a b -> K.Stream m a -> m (b, K.Stream m a)
fold_ fl strm = do
    (b, str) <- D.fold_ fl $ D.toStreamD strm
    return $! (b, D.fromStreamD str)

{-# INLINE fold #-}
fold :: Monad m => Fold m a b -> K.Stream m a -> m b
fold fl strm = do
    (b, _) <- fold_ fl strm
    return $! b

{-# INLINE after_ #-}
after_ :: (IsStream t, Monad m) => m b -> t m a -> t m a
after_ action xs = D.fromStreamD $ D.after_ action $ D.toStreamD xs

{-# INLINE unfold #-}
unfold :: (IsStream t, Monad m) => Unfold m a b -> a -> t m b
unfold unf x = D.fromStreamD $ D.unfold unf x

#if defined(FUSION_PLUGIN)
{-# ANN type SplitOnSeqState Fuse #-}
#endif
data SplitOnSeqState rb rh ck w fs s b x =
      SplitOnSeqInit
    | SplitOnSeqYield b (SplitOnSeqState rb rh ck w fs s b x)
    | SplitOnSeqDone

    | SplitOnSeqWordInit !fs s
    | SplitOnSeqWordLoop !w s !fs
    | SplitOnSeqWordDone Int !fs !w

    | SplitOnSeqReinit (fs -> SplitOnSeqState rb rh ck w fs s b x)

{-# INLINE [1] splitOnSeqD #-}
splitOnSeqD
    :: forall m a b. (MonadIO m, Storable a, Enum a, Eq a)
    => Array a
    -> Fold m a b
    -> D.Stream m a
    -> D.Stream m b
splitOnSeqD patArr (Fold fstep initial done) (D.Stream step state) =
    D.Stream stepOuter SplitOnSeqInit

    where

    patLen = A.length patArr
    maxIndex = patLen - 1
    elemBits = sizeOf (undefined :: a) * 8

    -- For word pattern case
    wordMask :: Word
    wordMask = (1 `shiftL` (elemBits * patLen)) - 1

    elemMask :: Word
    elemMask = (1 `shiftL` elemBits) - 1

    wordPat :: Word
    wordPat = wordMask .&. A.foldl' addToWord 0 patArr

    addToWord wd a = (wd `shiftL` elemBits) .|. fromIntegral (fromEnum a)

    skip = return . Skip

    nextAfterInit nextGen stepRes =
        case stepRes of
            FL.Partial s -> nextGen s
            FL.Done b -> SplitOnSeqYield b (SplitOnSeqReinit nextGen)

    {-# INLINE yieldProceed #-}
    yieldProceed nextGen fs =
        initial >>= skip . SplitOnSeqYield fs . nextAfterInit nextGen

    {-# INLINE [0] stepOuter #-}
    stepOuter _ SplitOnSeqInit = do
        res <- initial
        case res of
            FL.Partial acc -> return $ Skip $ SplitOnSeqWordInit acc state
            FL.Done b -> skip $ SplitOnSeqYield b SplitOnSeqInit

    stepOuter _ (SplitOnSeqYield x next) = return $ Yield x next

    ---------------------------
    -- Checkpoint
    ---------------------------

    stepOuter _ (SplitOnSeqReinit nextGen) =
        initial >>= skip . nextAfterInit nextGen

    -----------------
    -- Done
    -----------------

    stepOuter _ SplitOnSeqDone = return Stop

    ---------------------------
    -- Short Pattern - Shift Or
    ---------------------------

    stepOuter _ (SplitOnSeqWordDone 0 fs _) = do
        r <- done fs
        skip $ SplitOnSeqYield r SplitOnSeqDone
    stepOuter _ (SplitOnSeqWordDone n fs wrd) = do
        let old = elemMask .&. (wrd `shiftR` (elemBits * (n - 1)))
        r <- fstep fs (toEnum $ fromIntegral old)
        case r of
            FL.Partial fs1 -> skip $ SplitOnSeqWordDone (n - 1) fs1 wrd
            FL.Done b -> do
                 let jump c = SplitOnSeqWordDone (n - 1) c wrd
                 yieldProceed jump b

    stepOuter gst (SplitOnSeqWordInit fs st0) =
        go SPEC 0 0 st0

        where

        {-# INLINE go #-}
        go !_ !idx !wrd !st = do
            res <- step (adaptState gst) st
            case res of
                Yield x s -> do
                    let wrd1 = addToWord wrd x
                    if idx == maxIndex
                    then do
                        if wrd1 .&. wordMask == wordPat
                        then do
                            let jump c = SplitOnSeqWordInit c s
                            done fs >>= yieldProceed jump
                        else skip $ SplitOnSeqWordLoop wrd1 s fs
                    else go SPEC (idx + 1) wrd1 s
                Skip s -> go SPEC idx wrd s
                Stop -> do
                    if idx /= 0
                    then skip $ SplitOnSeqWordDone idx fs wrd
                    else do
                        r <- done fs
                        skip $ SplitOnSeqYield r SplitOnSeqDone

    stepOuter gst (SplitOnSeqWordLoop wrd0 st0 fs0) =
        go SPEC wrd0 st0 fs0

        where

        {-# INLINE go #-}
        go !_ !wrd !st !fs = do
            res <- step (adaptState gst) st
            case res of
                Yield x s -> do
                    let jump c = SplitOnSeqWordInit c s
                        wrd1 = addToWord wrd x
                        old = (wordMask .&. wrd)
                                `shiftR` (elemBits * (patLen - 1))
                    r <- fstep fs (toEnum $ fromIntegral old)
                    case r of
                        FL.Partial fs1 -> do
                            if wrd1 .&. wordMask == wordPat
                            then done fs1 >>= yieldProceed jump
                            else go SPEC wrd1 s fs1
                        FL.Done b -> yieldProceed jump b
                Skip s -> go SPEC wrd s fs
                Stop -> skip $ SplitOnSeqWordDone patLen fs wrd

{-# INLINE splitOnSeq #-}
splitOnSeq
    :: (IsStream t, MonadIO m, Storable a, Enum a, Eq a)
    => Array a -> Fold m a b -> t m a -> t m b
splitOnSeq patt f m = D.fromStreamD $ splitOnSeqD patt f (D.toStreamD m)
