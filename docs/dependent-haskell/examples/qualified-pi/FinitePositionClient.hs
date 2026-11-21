{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, ScopedTypeVariables, TypeApplications #-}
module FinitePositionClient (lookupVecViaView, observeFinitePosition, observeConstructedViews, observeZeroViews, observeListIndices, observeEmptyIndex) where
import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.IORef (newIORef, readIORef)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Sigma (Sigma(Pair))
import FinitePosition
import PackedVecLibrary (Nat(..), Vec(..))
import ParameterizedIndexLibrary (Fin(..), indexGeneric, indexVec, mkFinGeneric, mkFinVec)

-- | Check a signed position and select the element through its finite view.
lookupVecViaView :: Vec a n -> Int -> Maybe a
lookupVecViaView xs offset = do
    i <- mkFinVec xs offset
    case viewFin i of
        AtZero -> case xs of
            VCons x _ -> Just x
        AtSucc (Pair p Refl) -> case xs of
            VCons _ rest -> Just (indexVec p rest)

-- | Count the successor constructors of a finite position.
rank :: Fin n -> Int
rank FZ = 0
rank (FS i) = 1 + rank i

-- | Open the proof before reading the numeric predecessor.
position :: Fin n -> Maybe Int
position i = case viewFin i of
    AtZero -> Nothing
    AtSucc (Pair p Refl) -> Just (rank p)

-- | Demand the successor proof and leave its predecessor unevaluated.
lazy :: Fin n -> Bool
lazy i = case viewFin i of
    AtSucc (Pair _ Refl) -> True
    AtZero -> False

-- | Inspect one constructor of the bounded position.
isZero :: Fin n -> Bool
isZero i = case viewFin i of
    AtZero -> True
    AtSucc _ -> False

-- | Read a bounded position from IO and check the demand of its view.
observeFinitePosition :: IO ((Maybe Int, Maybe Int), (Bool, Bool, Bool, Bool))
observeFinitePosition = do
    i <- newIORef (FS (FS FZ) :: Fin ('S ('S ('S 'Z)))) >>= readIORef
    let bad = error "forced predecessor" :: Fin ('S 'Z)
    raised <- try (evaluate (isZero (error "forced position" :: Fin ('S 'Z)))) :: IO (Either ErrorCall Bool)
    let demanded = either ((== "forced position") . takeWhile (/= '\n') . displayException) (const False) raised
    pure ((position (FZ :: Fin ('S 'Z)), position i),
          (isZero (FZ :: Fin ('S 'Z)), isZero (FS bad), lazy (FS bad), demanded))

-- | Read a position and recover it from a written successor constructor.
observeConstructedViews :: IO (Maybe Int, Bool)
observeConstructedViews = do
    positionFromIO <- newIORef (FS FZ :: Fin ('S ('S 'Z))) >>= readIORef
    let firstAndSucc :: Fin n -> (Bool, Maybe Int)
        firstAndSucc i = case constructedViews i of
            (AtZero, AtSucc (Pair p Refl)) -> (True, Just (rank p))
            _ -> (False, Nothing)
    pure (snd (firstAndSucc positionFromIO),
          fst (firstAndSucc (error "forced predecessor" :: Fin ('S 'Z))))

-- | Inspect the zero views exported with both unfolding pragmas.
observeZeroViews :: (Bool, Bool)
observeZeroViews = (isZeroView (zeroView @'Z), isZeroView (zeroViewInlinable @'Z))
  where
    isZeroView :: FinView i -> Bool
    isZeroView AtZero = True
    isZeroView AtSucc{} = False

-- | Check an inferred ordinary caller on finite, partial, and cyclic lists.
observeListIndices :: IO [Maybe Char]
observeListIndices = do
    xs <- newIORef "abc" >>= readIORef
    i <- newIORef 1 >>= readIORef
    let checked ys n = fmap (indexGeneric ys) (mkFinGeneric ys n)
    pure [checked xs i, checked xs 3, checked (error "forced negative input") (-1),
          checked ('x' : error "forced suffix") 0, checked (cycle "xy") 4]

-- | An empty-list index takes its position before it inspects it.
observeEmptyIndex :: IO Bool
observeEmptyIndex = do
    select <- evaluate (indexGeneric @Char [])
    result <- try @ErrorCall (evaluate (select (error "empty position")))
    pure $ case result of
        Left err -> takeWhile (/= '\n') (displayException err) == "empty position"
        Right _ -> False
