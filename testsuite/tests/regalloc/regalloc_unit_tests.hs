{-# LANGUAGE ScopedTypeVariables #-}
module Main where

-- Register Allocator Unit Tests
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- This file contains fine-grained tests of the register allocator
-- ("regalloc"), which maps variables onto real machine registers.
-- These tests require inspection and manipulation
-- of the register allocator's intermediate data structures.
--
-- The tests are enumerated in the "runTests" function--each returns a Bool
-- and runTests simply checks that none returned False.
-- (currently the only test is testGraphNoSpills--see its comments for
-- details)
--
-- If the tests pass it will print "All tests passed", otherwise it will
-- print which ones failed.
--
-- Also note: "on x86" means "as if we were compiling for x86"--this test
-- doesn't actually have to run on any particular architecture.

import qualified RegAlloc.Graph.Stats as Color
import qualified RegAlloc.Linear.Base as Linear
import qualified X86.Instr
import HscMain
import CgUtils
import AsmCodeGen
import CmmBuildInfoTables
import CmmPipeline
import CmmParse
import CmmInfo
import Cmm
import Module
import Debug
import GHC
import GhcMonad
import UniqFM
import UniqSupply
import DynFlags
import ErrUtils
import Outputable
import BasicTypes

import Stream (collect, yield)

import Data.Typeable
import Data.Maybe
import Control.Monad
import Control.Applicative
import Control.Exception (Exception, throwIO)
import System.Environment
import System.IO

main :: IO ()
main = do
    [libdir] <- getArgs

    --get a GHC context and run the tests
    runGhc (Just libdir) $ do
        dflags <- fmap setOptions getDynFlags
        reifyGhc $ \_ -> do
            us <- unitTestUniqSupply
            runTests dflags us

    return ()

    where setOptions = (flip gopt_set) Opt_RegsGraph


-- | TODO: Make this an IORef along the lines of Data.Unique.newUnique to add
-- stronger guarantees a UniqSupply won't be accidentally reused
unitTestUniqSupply :: IO UniqSupply
unitTestUniqSupply = mkSplitUniqSupply 't'


newtype RegAllocTestException = RegAllocTestException String
            deriving (Show, Typeable)

instance Exception RegAllocTestException


-- | a safer assert in the IO monad
-- perform some action if the passed Bool is false
assertOr :: (String -> IO ()) -> String -> Bool -> IO Bool
assertOr alt msg False = alt msg >> return False
assertOr _   msg True  = return True

-- | Raise an exception if the passed Bool is false
assertIO :: String -> Bool -> IO Bool
assertIO = assertOr $ \msg -> void (throwIO . RegAllocTestException $ msg)


-- | compile the passed cmm file and return the register allocator stats
-- ***NOTE*** This function sets Opt_D_dump_asm_stats in the passed
-- DynFlags because it won't work without it.  Handle stderr appropriately.
compileCmmForRegAllocStats ::
    DynFlags ->
    FilePath ->
    (DynFlags ->
        NcgImpl (Alignment, CmmStatics) X86.Instr.Instr X86.Instr.JumpDest) ->
    UniqSupply ->
    IO [( Maybe [Color.RegAllocStats (Alignment, CmmStatics) X86.Instr.Instr]
        , Maybe [Linear.RegAllocStats])]
compileCmmForRegAllocStats dflags' cmmFile ncgImplF us = do
    let ncgImpl = ncgImplF dflags
    hscEnv <- newHscEnv dflags

    -- parse the cmm file and output any warnings or errors
    ((warningMsgs, errorMsgs), parsedCmm) <- parseCmmFile dflags cmmFile

    -- print parser errors or warnings
    mapM_ (printBagOfErrors dflags) [warningMsgs, errorMsgs]

    let initTopSRT = emptySRT thisMod
    cmmGroup <- fmap snd $ cmmPipeline hscEnv initTopSRT $ fromJust parsedCmm

    rawCmms <- cmmToRawCmm dflags (Stream.yield cmmGroup)

    collectedCmms <- mconcat <$> Stream.collect rawCmms

    -- compile and discard the generated code, returning regalloc stats
    mapM (\ (count, thisCmm) ->
        cmmNativeGen dflags thisMod thisModLoc ncgImpl
            usb dwarfFileIds dbgMap thisCmm count >>=
                (\(_, _, _, _, colorStats, linearStats, _) ->
                -- scrub unneeded output from cmmNativeGen
                return (colorStats, linearStats)))
                $ zip [0.. (length collectedCmms)] collectedCmms

    where
          --the register allocator's intermediate data
          --structures are usually discarded
          --(in AsmCodeGen.cmmNativeGen) for performance
          --reasons.  To prevent this we need to tell
          --cmmNativeGen we want them printed out even
          --though we ignore stderr in the test configuration.
          dflags = dopt_set dflags' Opt_D_dump_asm_stats
          [usa, usb, usc, usd] = take 4 . listSplitUniqSupply $ us
          -- don't need debugging information
          dwarfFileIds = emptyUFM
          dbgMap = debugToMap []
          thisMod = mkModule
                        (stringToUnitId . show . uniqFromSupply $ usc)
                        (mkModuleName . show . uniqFromSupply $ usd)
          thisModLoc = ModLocation Nothing (cmmFile ++ ".hi") (cmmFile ++ ".o")


-- | The register allocator should be able to see that each variable only
-- has a dependency on the one before it and that therefore only 1 variable
-- is live after each computation, no spilling needed.
noSpillsCmmFile = "no_spills.cmm"

-- | Run each unit test in this file and notify the user of success or
-- failure.
runTests :: DynFlags -> UniqSupply -> IO ()
runTests dflags us = testGraphNoSpills dflags noSpillsCmmFile us >>= \res ->
                        if res then putStrLn "All tests passed."
                               else hPutStr stderr "testGraphNoSpills failed!"


-- | To map an unlimited number of abstract variables to a limited number of
-- real registers the allocator is sometimes forced to "spill" data that
-- isn't needed for the next instruction from a register into memory.
-- This is expensive so minimizing spills and reloads is a high priority.
--
-- testGraphNoSpills compiles the passed cmm file using the graph coloring
-- register allocator and asserts that it doesn't contain
-- any spill instructions.  This (very basic) test is for cases where
-- the register allocator should be able to do everything
-- (on x86) in the passed file without any spills or reloads.
--
testGraphNoSpills :: DynFlags -> FilePath -> UniqSupply -> IO Bool
testGraphNoSpills dflags' path us = do
        colorStats <- fst . concatTupledMaybes <$>
                        compileCmmForRegAllocStats dflags path x86NcgImpl us

        assertIO "testGraphNoSpills: color stats should not be empty"
                        $ not (null colorStats)

        -- spill, reload, and reg-reg moves for the cmm file we just
        -- compiled
        let srms = foldr (\(a, b, c) (x, y, z) ->
                    (a + x, b + y, c + z)) (0, 0, 0)
                    . mapMaybe extractSRMs $ colorStats

        assertIO
                ("testGraphNoSpills called with " ++ path
                    ++ ": (spill, reload, reg-reg) = " ++ show srms)
                (matchesExpected srms)

    where concatTupledMaybes :: [( Maybe [a], Maybe [b])] -> ([a], [b])
          concatTupledMaybes =
            -- either concat the underlying list or return the accumulator list
            let acc n = maybe n (++ n) in
              foldr (\(as, bs) (xs, ys) -> (acc xs as, acc ys bs)) ([], [])

          dflags = dflags' { optLevel = 2 }

          -- discard irrelevant stats
          extractSRMs x = case x of
                                Color.RegAllocStatsColored _ _ _ _ _ _ _ _
                                    rSrms -> Just rSrms
                                _ -> Nothing

          matchesExpected (a, b, c) = a == 0 && b == 0
