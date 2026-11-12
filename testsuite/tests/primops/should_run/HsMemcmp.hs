{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}

-- Exercise 'compareByteArrays#' (lowered to the RTS 'hs_memcmp' by the
-- native and LLVM code generators) and the 'hs_memcmp' C function
-- directly, over pinned and unpinned 'ByteArray#' slices, unaligned
-- offsets, a spread of sizes, unsigned high-bit bytes and
-- first/middle/last differences.

module Main (main) where

import Control.Monad (unless, when)
import Control.Monad.ST (runST)
import GHC.Exts
import GHC.ST (ST (..))
import GHC.Word (Word8 (..))

#ifndef javascript_HOST_ARCH
import Control.Monad (forM_)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Marshal.Alloc (free, mallocBytes)
import Foreign.Ptr (nullPtr, plusPtr)
import Foreign.Storable (poke)
#endif

data BA = BA ByteArray#

data MBA s = MBA (MutableByteArray# s)

newByteArray :: Int -> ST s (MBA s)
newByteArray (I# n#) =
  ST $ \s -> case newByteArray# n# s of
    (# s', mba# #) -> (# s', MBA mba# #)

newPinnedByteArray :: Int -> ST s (MBA s)
newPinnedByteArray (I# n#) =
  ST $ \s -> case newPinnedByteArray# n# s of
    (# s', mba# #) -> (# s', MBA mba# #)

writeWord8Array :: MBA s -> Int -> Word8 -> ST s ()
writeWord8Array (MBA mba#) (I# i#) (W8# w#) =
  ST $ \s -> case writeWord8Array# mba# i# w# s of
    s' -> (# s', () #)

unsafeFreezeByteArray :: MBA s -> ST s BA
unsafeFreezeByteArray (MBA mba#) =
  ST $ \s -> case unsafeFreezeByteArray# mba# s of
    (# s', ba# #) -> (# s', BA ba# #)

-- 'compareByteArrays#', normalised to an 'Ordering'.
compareSlices :: BA -> Int -> BA -> Int -> Int -> Ordering
compareSlices (BA ba1#) (I# o1#) (BA ba2#) (I# o2#) (I# n#) =
  compare (I# (compareByteArrays# ba1# o1# ba2# o2# n#)) 0

createByteArray :: Bool -> Int -> (forall s. MBA s -> ST s ()) -> BA
createByteArray pinned n go = runST $ do
  mba <- (if pinned then newPinnedByteArray else newByteArray) n
  go mba
  unsafeFreezeByteArray mba

pack :: Bool -> [Word8] -> BA
pack pinned xs = createByteArray pinned (length xs) $ \mba -> go mba 0 xs
  where
    go _ _ [] = return ()
    go mba i (y : ys) = writeWord8Array mba i y >> go mba (i + 1) ys

-- Deterministic byte pattern covering the whole 0x00..0xff range
-- (31 is coprime to 256, so every value occurs), including the
-- unsigned-comparison traps 0x7f/0x80.
pat :: Int -> Word8
pat i = fromIntegral ((i * 31 + 7) `mod` 256)

-- Reference model: unsigned lexicographic comparison.
cmpRef :: [Word8] -> Int -> [Word8] -> Int -> Int -> Ordering
cmpRef xs o1 ys o2 n = compare (take n (drop o1 xs)) (take n (drop o2 ys))

checkPrim :: String -> [Word8] -> BA -> Int -> [Word8] -> BA -> Int -> Int -> Maybe String
checkPrim tag xs ba1 o1 ys ba2 o2 n =
  let want = cmpRef xs o1 ys o2 n
      got = compareSlices ba1 o1 ba2 o2 n
   in if got == want
        then Nothing
        else Just (tag ++ " n=" ++ show n ++ " o1=" ++ show o1 ++ " o2=" ++ show o2
                   ++ " want=" ++ show want ++ " got=" ++ show got)

-- Replace the element at index k.
replace :: Int -> Word8 -> [Word8] -> [Word8]
replace k w xs = take k xs ++ [w] ++ drop (k + 1) xs

-- Constant-length comparison with a literal length, so the NCG sees
-- @CmmLit (CmmInt n _)@ and exercises the inline memcmp lowering.
{-# INLINE cmpConst #-}
cmpConst :: ByteArray# -> Int# -> ByteArray# -> Int# -> Int# -> Int
cmpConst ba1# o1# ba2# o2# n# = I# (compareByteArrays# ba1# o1# ba2# o2# n#)

-- Two back-to-back constant-size comparisons in the same unboxed
-- expression, so two 'MO_Memcmp' nodes share a Cmm block.
{-# INLINE pairConst #-}
pairConst
  :: ByteArray# -> Int# -> ByteArray# -> Int#
  -> ByteArray# -> Int# -> ByteArray# -> Int# -> Int# -> (# Int#, Int# #)
pairConst ba1# o1# ba2# o2# ba3# o3# ba4# o4# n# =
  (# compareByteArrays# ba1# o1# ba2# o2# n#
   , compareByteArrays# ba3# o3# ba4# o4# n# #)

{-# INLINE checkConstLen #-}
checkConstLen :: Int -> Int# -> [String]
checkConstLen n n# =
  -- Equal slices at every unaligned offset pair in [0..7].
  [ f
  | o1 <- [0 .. 7]
  , o2 <- [0 .. 7]
  , let b1 = replicate o1 0xaa ++ slice ++ replicate 8 0xbb
        b2 = replicate o2 0xcc ++ slice ++ replicate 8 0xdd
        BA ba1# = pack False b1
        BA ba2# = pack False b2
        I# o1# = o1
        I# o2# = o2
        got = compare (cmpConst ba1# o1# ba2# o2# n#) 0
  , got /= EQ
  , let f = "const-eq n=" ++ show n ++ " o1=" ++ show o1 ++ " o2=" ++ show o2
            ++ " got=" ++ show got
  ]
  ++
  -- Every mismatch position, both signs, with unsigned high-bit pairs.
  [ f
  | (o1, o2) <- [(0, 0), (3, 5)]
  , pos <- [0 .. n - 1]
  , (x, y, want) <- directedPairs
  , let b1 = replicate o1 0xaa ++ replace pos x slice ++ replicate 8 0xbb
        b2 = replicate o2 0xcc ++ replace pos y slice ++ replicate 8 0xdd
        BA ba1# = pack False b1
        BA ba2# = pack False b2
        I# o1# = o1
        I# o2# = o2
        got = compare (cmpConst ba1# o1# ba2# o2# n#) 0
  , got /= want
  , let f = "const-diff n=" ++ show n ++ " o1=" ++ show o1 ++ " o2=" ++ show o2
            ++ " pos=" ++ show pos ++ " x=" ++ show x ++ " y=" ++ show y
            ++ " want=" ++ show want ++ " got=" ++ show got
  ]
  ++
  -- Two back-to-back constant-size comparisons sharing one expression.
  [ f
  | (o1, o2, o3) <- [(0, 2, 1), (7, 5, 0)]
  , let b1 = replicate o1 0xaa ++ slice ++ replicate 8 0xbb
        b2 = replicate o2 0xcc ++ slice ++ replicate 8 0xdd
        b3 = replicate o3 0xee ++ bump ++ replicate 8 0xff
        BA ba1# = pack False b1
        BA ba2# = pack False b2
        BA ba3# = pack False b3
        I# q1# = o1
        I# q2# = o2
        I# q3# = o3
        (# r1#, r2# #) = pairConst ba1# q1# ba2# q2# ba1# q1# ba3# q3# n#
        got1 = compare (I# r1#) 0
        got2 = compare (I# r2#) 0
  , got1 /= EQ || got2 /= wantPair
  , let f = "const-pair n=" ++ show n
            ++ " got1=" ++ show got1 ++ " got2=" ++ show got2
            ++ " wantPair=" ++ show wantPair
  ]
  where
    slice = take n (map pat [0 ..])
    directedPairs =
      [ (0x00, 0x80, LT), (0x7f, 0x80, LT), (0x80, 0xff, LT), (0x01, 0x02, LT)
      , (0x80, 0x00, GT), (0x80, 0x7f, GT), (0xff, 0x80, GT), (0x02, 0x01, GT)
      ]
    -- Last byte bumped up (or down from 0xff), so b1 vs bump differs.
    (bump, wantPair) = case slice of
      [] -> (slice, EQ)
      _ -> let old = last slice
           in if old < 0xff
                then (init slice ++ [old + 1], LT)
                else (init slice ++ [old - 1], GT)

-- Compile-time-constant lengths exercising the inline memcmp lowering.
-- Each call passes a literal length so the NCG sees @CmmLit (CmmInt n _)@.
constChecks :: [String]
constChecks =
  checkConstLen 0 0# ++
  checkConstLen 1 1# ++
  checkConstLen 2 2# ++
  checkConstLen 3 3# ++
  checkConstLen 4 4# ++
  checkConstLen 5 5# ++
  checkConstLen 6 6# ++
  checkConstLen 7 7# ++
  checkConstLen 8 8# ++
  checkConstLen 9 9# ++
  checkConstLen 12 12# ++
  checkConstLen 15 15# ++
  checkConstLen 16 16# ++
  checkConstLen 17 17# ++
  checkConstLen 24 24# ++
  checkConstLen 28 28# ++
  checkConstLen 31 31# ++
  checkConstLen 32 32# ++
  checkConstLen 33 33# ++
  checkConstLen 48 48# ++
  checkConstLen 63 63# ++
  checkConstLen 64 64# ++
  checkConstLen 65 65#

primChecks :: [String]
primChecks =
  [ f
  | pinned <- [False, True]
  , let ba1 = pack pinned xs
        ba2 = pack pinned ys
  , o1 <- offs smallMax
  , o2 <- offs smallMax
  , n <- smallSizes
  , o1 + n <= bufLen
  , o2 + n <= bufLen
  , Just f <- [checkPrim (if pinned then "pinned" else "unpinned") xs ba1 o1 ys ba2 o2 n]
  ]
  ++
  [ f
  | pinned <- [False, True]
  , let ba1 = pack pinned xs
        ba2 = pack pinned ys
  , o1 <- bigOffs
  , o2 <- bigOffs
  , n <- bigSizes
  , o1 + n <= bufLen
  , o2 + n <= bufLen
  , Just f <- [checkPrim (if pinned then "pinned-big" else "unpinned-big") xs ba1 o1 ys ba2 o2 n]
  ]
  ++
  -- Mixed pinned/unpinned operands.
  [ f
  | (o1, o2) <- [(0, 1), (7, 8), (31, 30)]
  , n <- mixedSizes
  , o1 + n <= bufLen
  , o2 + n <= bufLen
  , Just f <- [checkPrim "mixed" xs baP o1 ys baU o2 n]
  ]
  ++
  -- First/middle/last byte differences, both signs, on otherwise-identical
  -- slices at possibly distinct unaligned offsets (with distinct surrounding
  -- guard bytes so out-of-bounds reads fail).
  [ f
  | n <- diffSizes
  , (o1, o2) <- [(0, 0), (3, 5), (16, 7), (31, 0)]
  , pos <- nub [0, n `div` 2, n - 1]
  , let slice = take n xs
        old = slice !! pos
  , (new, want) <-
      [ (old + 1, LT) | old < 0xff ] ++ [ (old - 1, GT) | old > 0x00 ]
  , let b1 = replicate o1 0xaa ++ slice ++ replicate 16 0xbb
        b2 = replicate o2 0xcc ++ replace pos new slice ++ replicate 16 0xdd
        got = compareSlices (pack False b1) o1 (pack False b2) o2 n
  , got /= want
  , let f = "diff n=" ++ show n ++ " o1=" ++ show o1 ++ " o2=" ++ show o2
            ++ " pos=" ++ show pos ++ " want=" ++ show want ++ " got=" ++ show got
  ]
  ++
  -- Multi-iteration SIMD equal & late-mismatch checks (4 KiB and 64 KiB).
  [ f
  | n <- bigSizes
  , (o1, o2) <- [(0, 0), (3, 19), (16, 31)]
  , let slice = take n xs
        b1 = replicate o1 0xaa ++ slice ++ replicate 16 0xbb
        ba1 = pack False b1
        bEq = replicate o2 0xcc ++ slice ++ replicate 16 0xdd
        gotEq = compareSlices ba1 o1 (pack False bEq) o2 n
        lastIdx = n - 1
        old = slice !! lastIdx
        (new, wantLast) = if old < 0xff then (old + 1, LT) else (old - 1, GT)
        bLast = replicate o2 0xcc ++ replace lastIdx new slice ++ replicate 16 0xdd
        gotLast = compareSlices ba1 o1 (pack False bLast) o2 n
  , (tag, got, want) <-
      [ ("big-eq", gotEq, EQ)
      , ("big-last-diff", gotLast, wantLast)
      ]
  , got /= want
  , let f = tag ++ " n=" ++ show n ++ " o1=" ++ show o1 ++ " o2=" ++ show o2
            ++ " want=" ++ show want ++ " got=" ++ show got
  ]
  ++
  -- Unsigned high-bit traps.
  [ f
  | (x, y, want) <- [ ([0x80], [0x7f], GT)
                    , ([0x7f], [0x80], LT)
                    , ([0xff], [0x00], GT)
                    , ([0x00], [0xff], LT)
                    , ([0x00], [0x00], EQ)
                    ]
  , let got = compareSlices (pack False x) 0 (pack False y) 0 1
  , got /= want
  , let f = "high-bit " ++ show x ++ " vs " ++ show y ++ " got=" ++ show got
  ]
  ++
  -- Equal pointers short-circuit, and n == 0 on empty arrays.
  [ f
  | (tag, got, want) <-
      [ ("self-eq", compareSlices baU 0 baU 0 bufLen, EQ)
      , ("empty", compareSlices (pack False []) 0 (pack True []) 0 0, EQ)
      , ("zero-n", compareSlices baU 0 baU2 0 0, EQ)
      ]
  , got /= want
  , let f = tag ++ " got=" ++ show got
  ]
  ++
  -- Compile-time-constant lengths (inline memcmp lowering).
  constChecks
  where
    bufLen = 65536 + 32
    xs = map pat [0 .. bufLen - 1]
    -- A few flipped bytes so most overlapping slices differ somewhere.
    ys = replace 100 0x00 . replace 10000 0xff . replace 60000 0x80 $ xs
    baU = pack False xs
    baU2 = pack False xs
    baP = pack True xs
    smallMax = 31
    offs m = [0 .. m]
    smallSizes = [0 .. 65] ++ [127, 128, 255, 256]
    bigSizes = [4096, 65536]
    bigOffs = [0, 1, 7, 16, 31]
    mixedSizes = [1, 2, 3, 7, 8, 9, 15, 16, 17, 31, 32, 33, 64, 65, 128, 256]
    diffSizes = [1 .. 65] ++ [128, 256]
    nub [] = []
    nub (z : zs) = z : nub [w | w <- zs, w /= z]

#ifndef javascript_HOST_ARCH

foreign import ccall unsafe "hs_memcmp"
  c_hs_memcmp :: Ptr Word8 -> Ptr Word8 -> CSize -> IO CInt

ffiChecks :: IO [String]
ffiChecks = do
  let total = 4096 + 64
      xs = map pat [0 .. total - 1]
      ys = replace 100 0x00 . replace 3000 0xff $ xs
  p <- mallocBytes total
  q <- mallocBytes total
  forM_ (zip [0 ..] xs) $ \(i, w) -> poke (p `plusPtr` i) w
  forM_ (zip [0 ..] ys) $ \(i, w) -> poke (q `plusPtr` i) w
  fails <- fmap concat . sequence $
    [ do
        r <- c_hs_memcmp (p `plusPtr` o1) (q `plusPtr` o2) (fromIntegral n)
        let want = cmpRef xs o1 ys o2 n
            got = compare r 0
        return [ "ffi n=" ++ show n ++ " o1=" ++ show o1 ++ " o2=" ++ show o2
                 ++ " want=" ++ show want ++ " got=" ++ show got
               | got /= want
               ]
    | o1 <- [0 .. 31]
    , o2 <- [0 .. 31]
    , n <- ffiSizes
    , o1 + n <= total
    , o2 + n <= total
    ]
  -- Pointer equality and n == 0 edges, including NULL pointers.
  r1 <- c_hs_memcmp p p (fromIntegral total)
  r2 <- c_hs_memcmp p q 0
  r3 <- c_hs_memcmp nullPtr nullPtr 0
  r4 <- c_hs_memcmp nullPtr q 0
  free p
  free q
  return $ fails
    ++ ["ffi self-eq got=" ++ show r1 | r1 /= 0]
    ++ ["ffi zero-n got=" ++ show r2 | r2 /= 0]
    ++ ["ffi null-null-zero got=" ++ show r3 | r3 /= 0]
    ++ ["ffi null-zero got=" ++ show r4 | r4 /= 0]
  where
    ffiSizes = [0 .. 65] ++ [127, 128, 255, 256, 4096]

#endif

main :: IO ()
main = do
  let fails1 = primChecks
  mapM_ putStrLn (take 50 fails1)
#ifndef javascript_HOST_ARCH
  fails2 <- ffiChecks
  mapM_ putStrLn (take 50 fails2)
  let fails = fails1 ++ fails2
#else
  let fails = fails1
#endif
  unless (null fails) $
    putStrLn ("FAILURES: " ++ show (length fails))
  when (null fails) $
    putStrLn "PASS"
