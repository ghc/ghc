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

import qualified GHC.CmmToAsm.Reg.Graph.Stats as Color
import qualified GHC.CmmToAsm.Reg.Linear.Base as Linear
import qualified GHC.CmmToAsm.X86.Instr as X86.Instr
import qualified GHC.CmmToAsm.X86 as X86
import GHC.Driver.Config.Cmm.Parser
import GHC.Driver.Config.CmmToAsm
import GHC.Driver.Config.Cmm
import GHC.Driver.Main
import GHC.Driver.Env
import GHC.StgToCmm.CgUtils
import GHC.CmmToAsm
import GHC.CmmToAsm.Config
import GHC.Cmm.Info.Build
import GHC.Cmm.Pipeline
import GHC.Cmm.Parser
import GHC.Cmm.Info
import GHC.Cmm
import GHC.Parser.Errors.Ppr
import GHC.Unit.Module
import GHC.Cmm.DebugBlock
import GHC
import GHC.Driver.Monad
import GHC.Driver.Config.Diagnostic
import GHC.Types.Unique.FM
import GHC.Types.Unique.Supply
import GHC.Driver.Session
import GHC.Driver.Errors
import GHC.Utils.Error
import GHC.Utils.Logger
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Types.Basic
import GHC.Unit.Home
import GHC.Unit.Finder
import GHC.Unit.Env
import GHC.Unit.Home.ModInfo
import GHC.Driver.Config.Finder

import GHC.Data.Stream as Stream (collect, yield)

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
        dflags0 <- flip gopt_set Opt_RegsGraph <$> getDynFlags
        --the register allocator's intermediate data
        --structures are usually discarded
        --(in GHC.CmmToAsm.cmmNativeGen) for performance
        --reasons.  To prevent this we need to tell
        --cmmNativeGen we want them printed out even
        --though we ignore stderr in the test configuration.
        let dflags1 = dopt_set dflags0 Opt_D_dump_asm_stats
        setSessionDynFlags dflags1

        dflags <- getDynFlags
        logger <- getLogger
        home_unit <- hsc_home_unit <$> getSession
        reifyGhc $ \_ -> do
            us <- unitTestUniqSupply
            runTests logger home_unit dflags us

    return ()

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
compileCmmForRegAllocStats
  :: Logger
  -> HomeUnit
  -> DynFlags
  -> FilePath
  -> (NCGConfig ->
        NcgImpl (Alignment, RawCmmStatics) X86.Instr.Instr X86.Instr.JumpDest)
  -> UniqSupply
  -> IO [( Maybe [Color.RegAllocStats (Alignment, RawCmmStatics) X86.Instr.Instr]
        , Maybe [Linear.RegAllocStats])]
compileCmmForRegAllocStats logger home_unit dflags cmmFile ncgImplF us = do
    let ncgImpl = ncgImplF (initNCGConfig dflags thisMod)
    let cmm_config = initCmmConfig dflags

    -- parse the cmm file and output any warnings or errors
    let fake_mod = mkHomeModule home_unit (mkModuleName "fake")
        cmmpConfig = initCmmParserConfig dflags
    (warnings, errors, parsedCmm) <- parseCmmFile cmmpConfig fake_mod home_unit cmmFile

    -- print parser errors or warnings
    let !diag_opts = initDiagOpts dflags
        !print_config = initPsMessageOpts dflags
    mapM_ (printMessages logger print_config diag_opts) [warnings, errors]

    let initTopSRT = emptySRT thisMod
    cmmGroup <- fmap snd $ cmmPipeline logger cmm_config initTopSRT $ fst $ fromJust parsedCmm

    let profile = targetProfile dflags
    rawCmms <- cmmToRawCmm logger profile (Stream.yield cmmGroup)

    collectedCmms <- mconcat <$> Stream.collect rawCmms

    -- compile and discard the generated code, returning regalloc stats
    mapM (\ (count, thisCmm) ->
        cmmNativeGen logger ncgImpl
            usb dwarfFileIds dbgMap thisCmm count >>=
                (\(_, _, _, _, colorStats, linearStats, _, _) ->
                -- scrub unneeded output from cmmNativeGen
                return (colorStats, linearStats)))
                $ zip [0.. (length collectedCmms)] collectedCmms

    where
          [usa, usb, usc, usd] = take 4 . listSplitUniqSupply $ us
          -- don't need debugging information
          dwarfFileIds = emptyUFM
          dbgMap = debugToMap []
          thisMod = mkModule
                        (stringToUnit . show . uniqFromSupply $ usc)
                        (mkModuleName . show . uniqFromSupply $ usd)


-- | The register allocator should be able to see that each variable only
-- has a dependency on the one before it and that therefore only 1 variable
-- is live after each computation, no spilling needed.
noSpillsCmmFile = "no_spills.cmm"

-- | Run each unit test in this file and notify the user of success or
-- failure.
runTests :: Logger -> HomeUnit -> DynFlags -> UniqSupply -> IO ()
runTests logger home_unit dflags us = do
  res <- testGraphNoSpills logger home_unit dflags noSpillsCmmFile us
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
testGraphNoSpills :: Logger -> HomeUnit -> DynFlags -> FilePath -> UniqSupply -> IO Bool
testGraphNoSpills logger home_unit dflags' path us = do
        colorStats <- fst . concatTupledMaybes <$>
                        compileCmmForRegAllocStats logger home_unit dflags path X86.ncgX86 us

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

          dflags = updOptLevel 2 dflags'

          -- discard irrelevant stats
          extractSRMs x = case x of
                                Color.RegAllocStatsColored _ _ _ _ _ _ _ _
                                    rSrms _ -> Just rSrms
                                _ -> Nothing

          matchesExpected (a, b, c) = a == 0 && b == 0
