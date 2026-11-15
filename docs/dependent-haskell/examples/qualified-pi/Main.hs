{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImpredicativeTypes #-}

module Main (main) where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Control.Monad (forM_, unless)
import Data.IORef (newIORef, readIORef)
import Data.List (isInfixOf)
import Data.Type.Equality ((:~:) (Refl))
import FunctionStableNameClient (sameFunction, samePlainFunction)
import GHC.Exts (withDict)
import PackedVecLibrary
import QualifiedPiLibrary
import System.Mem.StableName (makeStableName)

-- | Specialize from the complete expected arrow without a source type argument.
specialize :: QPi -> QPi
specialize = exposeQ

-- | Specialize the payload beneath the retained binder from its expected type.
specializeBeneath :: LiftQ -> LiftQ
specializeBeneath = beneathQ

-- | Construct vectors without using the available Eq Int dictionary.
original :: QPi
original n = replicateNative @Int n sharedPayload

-- | Ignore both arguments before returning the qualified function.
underneath :: LiftQ
underneath m xs = original

-- | Leave the index unforced when the vector is already bottom.
ignoreIndex :: QPi
ignoreIndex n = bottomVector

-- | Share one ordinary lifted payload across both paths.
sharedPayload :: Int
sharedPayload = read "7"

-- | Supply an ignored or demanded runtime-index failure.
bottomLength :: Nat
bottomLength = error "native-q-bottom-index"

-- | Supply a cyclic runtime prefix without evaluating it during construction.
cyclicLength :: Nat
cyclicLength = S cyclicLength

-- | Supply an unavailable vector at any erased length.
bottomVector :: forall a index. Vec a index
bottomVector = error "native-q-bottom-vector"

-- | Supply a bottom function with the complete qualified type.
bottomFunction :: QPi
bottomFunction = error "native-q-bottom-function"

-- | Supply a bottom outer retained function.
bottomLift :: LiftQ
bottomLift = error "native-q-bottom-lift"

-- | Check a result without requiring a native type in IO's result parameter.
checkWhnf :: a -> IO ()
checkWhnf value = do
    _ <- evaluate value
    pure ()

-- | Check one ordinary result.
assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual =
    unless (expected == actual) $
        fail (label ++ ": expected " ++ show expected ++ ", got " ++ show actual)

-- | Print only the requested exception marker.
expectMarker :: String -> String -> IO () -> IO ()
expectMarker label marker action = do
    result <- try @ErrorCall action
    case result of
        Left exception
            | marker `isInfixOf` displayException exception ->
                putStrLn (label ++ ": " ++ marker)
        Left exception -> fail (displayException exception)
        Right () -> fail (label ++ ": expected exception")

-- | Check a finite input read independently from the outer retained index.
checkFinite :: Int -> Nat -> Nat -> IO ()
checkFinite count index outer = do
    indexCell <- newIORef index
    outerCell <- newIORef outer
    n <- readIORef indexCell
    m <- readIORef outerCell
    let direct = toList (specialize original n)
        lifted = toList (specializeBeneath underneath m (replicateNative @Bool m True) n)
        expected = replicate count 7
        label = "qualified finite " ++ show count
    assertEqual label expected direct
    assertEqual label expected lifted
    putStrLn (label ++ ": " ++ show direct ++ " | " ++ show lifted)

-- | Check ordinary payload identity after both function transports.
checkSharing :: IO ()
checkSharing = do
    cell <- newIORef (S Z)
    n <- readIORef cell
    let direct = toList (specialize original n)
        lifted = toList (specializeBeneath underneath Z VNil n)
    case (direct, lifted) of
        (first : _, second : _) -> do
            a <- evaluate first
            b <- evaluate second
            originalName <- makeStableName sharedPayload
            aName <- makeStableName a
            bName <- makeStableName b
            unless (originalName == aName && originalName == bName) $
                fail "qualified payload sharing"
            putStrLn "qualified payload sharing: ok"
        _ -> fail "qualified payload sharing: expected two nonempty vectors"

-- | Exercise checked specialization and its demand boundaries.
main :: IO ()
main = do
    forM_
        [ (0, Z, S Z)
        , (1, S Z, S (S Z))
        , (2, S (S Z), S (S (S Z)))
        , (3, S (S (S Z)), Z)
        ]
        $ \(count, n, m) -> checkFinite count n m
    assertEqual "qualified ignored function" 42 (ignoreQ bottomFunction)
    putStrLn "qualified ignored function: 42"
    checkWhnf specialize
    checkWhnf specializeBeneath
    putStrLn "qualified helper aliases: ok"
    expectMarker "qualified identity bottom" "native-q-bottom-function" $
        checkWhnf (specialize bottomFunction)
    expectMarker "qualified beneath identity bottom" "native-q-bottom-lift" $
        checkWhnf (specializeBeneath bottomLift)
    checkWhnf (PackVec bottomLength (ignoreIndex bottomLength))
    putStrLn "qualified package ignores bottom index and vector: ok"
    checkWhnf (PackVec cyclicLength (ignoreIndex cyclicLength))
    putStrLn "qualified package ignores cyclic index and vector: ok"
    expectMarker "qualified selected bottom vector" "native-q-bottom-vector" $
        checkWhnf (ignoreIndex Z)
    expectMarker "qualified demanded bottom index" "native-q-bottom-index" $
        checkWhnf (toList (specialize original bottomLength))
    checkWhnf (specializeBeneath underneath bottomLength bottomVector)
    checkWhnf (specializeBeneath underneath cyclicLength bottomVector)
    putStrLn "qualified beneath partial prefixes: ok"
    checkSharing
    runDictionaryChecks
    runFunctionIdentityChecks
    runContainerChecks
    runOrdinaryPatternChecks
    runGadtPatternChecks

-- | Specialize the injected-context function from its complete expected arrow.
specializeDemand :: DPi -> DPi
specializeDemand = exposeDemand

-- | Leave the injected dictionary unused even when its vector is consumed.
ignoreDemand :: DPi
ignoreDemand n = replicateNative @Int n 7

-- | Store the injected method lazily in each vector element.
useDemand :: DPi
useDemand n = replicateNative @Int n demand

-- | Supply bottom as the single-method dictionary representation.
bottomDictionary :: Int
bottomDictionary = error "native-q-bottom-dictionary"

-- | Supply an unused function with the injected-context type.
bottomDemandFunction :: DPi
bottomDemandFunction = error "native-q-bottom-demand-function"

-- | Ignore a function and its injected dictionary context.
ignoreDemandFunction :: DPi -> Int
ignoreDemandFunction _ = 42

-- | Separate dictionary construction, vector shape and method demand.
runDictionaryChecks :: IO ()
runDictionaryChecks = do
    cell <- newIORef (S (S Z))
    zeroCell <- newIORef Z
    n <- readIORef cell
    zero <- readIORef zeroCell
    let ignored =
            withDict @(Demand Int) @Int @Int
                bottomDictionary
                (sum (toList (specializeDemand ignoreDemand n)))
    assertEqual "qualified ignored bottom dictionary" 14 ignored
    putStrLn "qualified ignored bottom dictionary: 14"
    let used =
            withDict @(Demand Int) @Int @Int
                19
                (sum (toList (specializeDemand useDemand n)))
    assertEqual "qualified injected dictionary" 38 used
    putStrLn "qualified injected dictionary: 38"
    let shape =
            withDict @(Demand Int) @Int @Int
                bottomDictionary
                ( case specializeDemand useDemand n of
                    VNil -> 0
                    VCons _ _ -> 42
                )
    assertEqual "qualified vector shape ignores dictionary" 42 shape
    putStrLn "qualified vector shape ignores dictionary: 42"
    expectMarker "qualified demanded dictionary" "native-q-bottom-dictionary" $
        checkWhnf
            ( withDict @(Demand Int) @Int @Int
                bottomDictionary
                (sum (toList (specializeDemand useDemand n)))
            )
    let empty =
            withDict @(Demand Int) @Int @Int
                bottomDictionary
                (sum (toList (specializeDemand useDemand zero)))
    assertEqual "qualified empty vector ignores dictionary" 0 empty
    putStrLn "qualified empty vector ignores dictionary: 0"
    let ignoredFunction =
            withDict @(Demand Int) @Int @Int
                bottomDictionary
                (ignoreDemandFunction bottomDemandFunction)
    assertEqual "qualified ignored dictionary and function" 42 ignoredFunction
    putStrLn "qualified ignored dictionary and function: 42"

-- | Compare shared function values after evaluating both to weak-head form.
runFunctionIdentityChecks :: IO ()
runFunctionIdentityChecks = do
    let transported :: QPi
        transported = specialize original
        returned :: QPi
        returned = specializeBeneath underneath Z VNil
    checkWhnf original
    checkWhnf transported
    direct <- sameFunction original transported
    assertEqual "qualified function identity" True direct
    putStrLn "qualified function identity: True"
    checkWhnf returned
    nested <- sameFunction original returned
    assertEqual "qualified returned function identity" True nested
    putStrLn "qualified returned function identity: True"

-- | Select the erased parameter from the complete function expectation.
viaWitness :: QPi -> QPi
viaWitness = withQ Refl

-- | Keep the bottom witness unused when no equality evidence is needed.
ignoringProof :: QPi -> QPi
ignoringProof = ignoreProofQ (error "native-container-bottom-proof")

-- | Force the bottom witness before exposing the returned function.
demandingProof :: QPi -> QPi
demandingProof = withQ (error "native-container-bottom-proof")

-- | Exercise each container route at independent runtime lengths.
checkContainerFinite :: Int -> Nat -> Int -> Nat -> IO ()
checkContainerFinite count index outerCount outer = do
    cell <- newIORef index
    outerCell <- newIORef outer
    n <- readIORef cell
    m <- readIORef outerCell
    let expected = replicate count 7
        bools = replicateNative @Bool m True
    assertEqual "container witness" expected (toList (viaWitness original n))
    assertEqual
        "container headTuple id"
        expected
        (toList (headTuple (id (original, original)) n))
    assertEqual
        "container worker duplicate"
        expected
        (toList (headTuple ((keepDuplicate duplicateQ) original) n))
    assertEqual
        "container newtype id"
        expected
        (toList (unwrapBox (id (Box original)) n))
    case (keepTupleTransformer tupleTransport) (makeTuple original) m bools of
        (transported, returned) -> do
            let actual = toList (transported n)
                returnedBools = toList returned
            assertEqual "container tuple Pi function" expected actual
            assertEqual "container tuple Pi vector" (replicate outerCount True) returnedBools
            putStrLn
                ( "container finite "
                    ++ show count
                    ++ ": "
                    ++ show actual
                    ++ " | "
                    ++ show returnedBools
                )

-- | Check proof order, function identity and newtype bottom behavior.
runContainerChecks :: IO ()
runContainerChecks = do
    forM_
        [ (0, Z, 1, S Z)
        , (1, S Z, 2, S (S Z))
        , (2, S (S Z), 3, S (S (S Z)))
        , (3, S (S (S Z)), 0, Z)
        ]
        $ \(count, n, outerCount, m) -> checkContainerFinite count n outerCount m
    let proved :: QPi
        proved = viaWitness original
        duplicated :: QPi
        duplicated = headTuple ((keepDuplicate duplicateQ) original)
        unwrapped :: QPi
        unwrapped = unwrapBox (id (Box original))
    checkWhnf original
    checkWhnf proved
    proofSame <- sameFunction original proved
    assertEqual "container witness function identity" True proofSame
    putStrLn "container witness function identity: True"
    checkWhnf (ignoringProof original)
    putStrLn "container ignored bottom proof: ok"
    let ignoredVector = toList (ignoringProof original (S Z))
    assertEqual "container ignored proof vector" [7] ignoredVector
    putStrLn "container ignored proof vector: [7]"
    checkWhnf demandingProof
    putStrLn "container partial bottom proof: ok"
    expectMarker "container demanded proof at function WHNF" "native-container-bottom-proof" $
        checkWhnf (demandingProof original)
    expectMarker "container proof before bottom function" "native-container-bottom-proof" $
        checkWhnf (toList (demandingProof bottomFunction Z))
    expectMarker "container proof before bottom index" "native-container-bottom-proof" $
        checkWhnf (toList (demandingProof original bottomLength))
    let firstOnly = toList (headTuple (id (original, bottomFunction)) (S Z))
    assertEqual "container tuple ignores second function" [7] firstOnly
    putStrLn "container tuple ignores second function: [7]"
    checkWhnf duplicated
    duplicateSame <- sameFunction original duplicated
    assertEqual "container duplicate worker identity" True duplicateSame
    putStrLn "container duplicate worker identity: True"
    checkWhnf unwrapped
    newtypeSame <- sameFunction original unwrapped
    assertEqual "container newtype function identity" True newtypeSame
    putStrLn "container newtype function identity: True"
    expectMarker "container newtype unwrapped bottom" "native-q-bottom-function" $
        checkWhnf (unwrapBox (id (Box bottomFunction)))
    expectMarker "container newtype WHNF bottom" "native-q-bottom-function" $
        checkWhnf (id (Box bottomFunction))

-- | Leave ordinary newtype and data pattern demand to match desugaring.
runOrdinaryPatternChecks :: IO ()
runOrdinaryPatternChecks = do
    checkWhnf (ignoreBoxFixed (error "native-container-bottom-box"))
    putStrLn "pattern named newtype ignores bottom box: ok"
    expectMarker "pattern named data forces bottom box" "native-container-bottom-data-box" $
        checkWhnf (ignoreDataFixed (error "native-container-bottom-data-box"))
    checkWhnf
        (keepBoxFunction (\(Box _) -> fixedQ) (error "native-container-bottom-box"))
    putStrLn "pattern lambda newtype ignores bottom box: ok"
    expectMarker "pattern lambda data forces bottom box" "native-container-bottom-data-box" $
        checkWhnf
            (keepDataFunction (\(DataBox _) -> fixedQ) (error "native-container-bottom-data-box"))
    checkWhnf (ignoreDataFixed (DataBox bottomFunction))
    putStrLn "pattern data ignores bottom function field: ok"
    let result = toList (ignoreBoxFixed (error "native-container-bottom-box") (S Z))
    assertEqual "pattern newtype ignored box vector" [7] result
    putStrLn "pattern newtype ignored box vector: [7]"

-- | Choose both element parameters before applying the transported function.
transportPlain :: PlainPi -> PlainPi
transportPlain = transportRefl Refl

-- | Leave the GADT witness untouched until the second value argument arrives.
transportPlainBottom :: PlainPi -> PlainPi
transportPlainBottom = transportRefl (error "native-container-gadt-bottom-proof")

-- | Check the constructor equation's evidence, identity and demand order.
runGadtPatternChecks :: IO ()
runGadtPatternChecks = do
    cell <- newIORef (S Z)
    n <- readIORef cell
    let transported :: PlainPi
        transported = transportPlain plainFunction
        actual = toList (transported n)
    assertEqual "GADT Refl vector" [7] actual
    putStrLn "GADT Refl vector: [7]"
    checkWhnf plainFunction
    checkWhnf transported
    same <- samePlainFunction plainFunction transported
    assertEqual "GADT Refl function identity" True same
    putStrLn "GADT Refl function identity: True"
    checkWhnf transportPlainBottom
    putStrLn "GADT partial bottom proof: ok"
    expectMarker "GADT demanded returned function" "native-container-gadt-bottom-proof" $
        checkWhnf (transportPlainBottom plainFunction)
