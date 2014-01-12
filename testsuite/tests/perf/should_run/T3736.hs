{-# OPTIONS_GHC -funbox-strict-fields -O #-}
{-# LANGUAGE ExistentialQuantification #-}

{- OPTIONS_GHC -ddump-simpl -ddump-asm -}

module Main (main) where

import GHC.Float (float2Int, int2Float)

import System.Environment

import Prelude hiding           (null
                                ,lines,unlines
                                ,writeFile
                                )

import Control.Exception        (assert, bracket, )

import Foreign.Marshal.Array    (advancePtr)
import Foreign.Ptr              (minusPtr)
import Foreign.Storable         (Storable(..))

import Control.Monad            (when)

import System.IO                (openBinaryFile, hClose,
                                 hPutBuf,
                                 Handle, IOMode(..))

import System.IO.Unsafe         (unsafePerformIO)

import Foreign.Ptr              (Ptr)
import Foreign.ForeignPtr       (ForeignPtr, withForeignPtr, )
import Foreign.Marshal.Array    (copyArray)

import qualified Foreign.ForeignPtr as F

main :: IO ()
main = do args <- getArgs
          case args of
              ["1"] -> mainMonolithic1Generator
              ["2"] -> mainMonolithic1Composed
              _ -> error "Huh?"

type Phase = (Float, Float, Float)

{-# INLINE saw #-}
saw :: Num a => a -> a
saw t = 1-2*t

{-# INLINE fraction #-}
fraction :: Float -> Float
fraction x = x - int2Float (float2Int x)

{-# INLINE generator0Freq #-}
generator0Freq :: Float -> Float -> Maybe (Float, Float)
generator0Freq freq =
   \p -> Just (saw p, fraction (p+freq))

infixl 6 `mix`, `mixGen`

{-# INLINE mix #-}
mix ::
   (Num y) =>
   (s -> Maybe (y, s)) ->
   (t -> Maybe (y, t)) ->
   ((s,t) -> Maybe (y, (s,t)))
mix f g (s0,t0) =
   do (a,s1) <- f s0
      (b,t1) <- g t0
      return ((a+b), (s1,t1))

data Generator a =
   forall s.
      Generator (s -> Maybe (a, s)) s

{-# INLINE runGeneratorMonolithic #-}
runGeneratorMonolithic :: Int -> Generator Float -> Vector Float
runGeneratorMonolithic size' (Generator f s) =
   fst $ unfoldrN size' f s

{- SPECIALISE INLINE generator0Gen :: Float -> Float -> Generator Float -}
{-# INLINE generator0Gen #-}
generator0Gen :: Float -> Float -> Generator Float
generator0Gen freq phase =
   Generator (\p -> Just (saw p, fraction (p+freq))) phase

{- SPECIALISE INLINE mixGen :: Generator Float -> Generator Float -> Generator Float -}
{-# INLINE mixGen #-}
mixGen ::
   (Num y) =>
   Generator y ->
   Generator y ->
   Generator y
mixGen (Generator f s) (Generator g t) =
   Generator (\(s0,t0) ->
      do (a,s1) <- f s0
         (b,t1) <- g t0
         return ((a+b), (s1,t1))) (s,t)

{-# INLINE dl #-}
dl :: Phase
dl = (0.01008, 0.01003, 0.00990)

{-# INLINE initPhase2 #-}
initPhase2 :: (Phase, Phase)
initPhase2 =
   ((0,0.7,0.1), (0.3,0.4,0.6))


size :: Int
size = 10000000


mainMonolithic1Composed :: IO ()
mainMonolithic1Composed =
   writeFile "T3736.speed.f32"
      (fst $ unfoldrN size
          (let (f0,f1,f2) = dl
           in  generator0Freq f0 `mix`
               generator0Freq f1 `mix`
               generator0Freq f2)
          (let (p0,p1,p2) = fst initPhase2
           in  ((p0,p1),p2)))

mainMonolithic1Generator :: IO ()
mainMonolithic1Generator =
   writeFile "T3736.speed.f32"
      (runGeneratorMonolithic size
          (let (f0,f1,f2) = dl
               (p0,p1,p2) = fst initPhase2
           in  generator0Gen f0 p0 `mixGen`
               generator0Gen f1 p1 `mixGen`
               generator0Gen f2 p2))

empty :: (Storable a) => Vector a
empty = unsafeCreate 0 $ const $ return ()
{-# NOINLINE empty #-}

null :: Vector a -> Bool
null (SV _ _ l) = assert (l >= 0) $ l <= 0
{-# INLINE null #-}

unfoldrN :: (Storable b) => Int -> (a -> Maybe (b, a)) -> a -> (Vector b, Maybe a)
unfoldrN n f x0 =
   if n <= 0
     then (empty, Just x0)
     else unsafePerformIO $ createAndTrim' n $ \p -> go p n x0
       where
          go = arguments2 $ \p i -> \x ->
             if i == 0
               then return (0, n-i, Just x)
               else
                 case f x of
                   Nothing     -> return (0, n-i, Nothing)
                   Just (w,x') -> do poke p w
                                     go (incPtr p) (i-1) x'
{-# INLINE unfoldrN #-}

hPut :: (Storable a) => Handle -> Vector a -> IO ()
hPut h v =
   when (not (null v)) $
      withStartPtr v $ \ ptrS l ->
         let ptrE = advancePtr ptrS l
         in  hPutBuf h ptrS (minusPtr ptrE ptrS)

writeFile :: (Storable a) => FilePath -> Vector a -> IO ()
writeFile f txt =
   bracket (openBinaryFile f WriteMode) hClose
      (\h -> hPut h txt)

data Vector a = SV {-# UNPACK #-} !(ForeignPtr a)
                   {-# UNPACK #-} !Int                -- offset
                   {-# UNPACK #-} !Int                -- length

withStartPtr :: Storable a => Vector a -> (Ptr a -> Int -> IO b) -> IO b
withStartPtr (SV x s l) f =
   withForeignPtr x $ \p -> f (p `advancePtr` s) l
{-# INLINE withStartPtr #-}

incPtr :: (Storable a) => Ptr a -> Ptr a
incPtr v = advancePtr v 1
{-# INLINE incPtr #-}

unsafeCreate :: (Storable a) => Int -> (Ptr a -> IO ()) -> Vector a
unsafeCreate l f = unsafePerformIO (create l f)
{-# INLINE unsafeCreate #-}

create :: (Storable a) => Int -> (Ptr a -> IO ()) -> IO (Vector a)
create l f = do
    fp <- mallocForeignPtrArray l
    withForeignPtr fp $ \p -> f p
    return $! SV fp 0 l

createAndTrim' :: (Storable a) => Int
                               -> (Ptr a -> IO (Int, Int, b))
                               -> IO (Vector a, b)
createAndTrim' l f = do
    fp <- mallocForeignPtrArray l
    withForeignPtr fp $ \p -> do
        (off, l', res) <- f p
        if assert (l' <= l) $ l' >= l
            then return $! (SV fp 0 l, res)
            else do ps <- create l' $ \p' -> copyArray p' (p `advancePtr` off) l'
                    return $! (ps, res)

{-# INLINE arguments2 #-}
arguments2 :: (a -> b -> x) -> a -> b -> x
arguments2 f = \a b -> (f $! a) $! b

{-# INLINE mallocForeignPtrArray #-}
mallocForeignPtrArray :: Storable a => Int -> IO (F.ForeignPtr a)
mallocForeignPtrArray = F.mallocForeignPtrArray
