module Main where

import GHC.Driver.Session
import GHC.Types.RepType
import GHC.Runtime.Heap.Layout
import GHC.StgToCmm.Layout
import GHC.StgToCmm.Closure
import GHC
import GHC.Driver.Monad
import System.Environment
import GHC.Platform

main :: IO ()
main = do
    [libdir] <- getArgs
    runGhc (Just libdir) $ do
      setSessionDynFlags =<< getDynFlags
      tests


-- How to read tests:
--   F(a,8) = field a at offset 8
--   P(4,8) = 4 bytes of padding at offset 8
tests :: Ghc ()
tests = do
      (_, _, off) <- runTest [("a", FloatRep), ("b", DoubleRep)]
      assert_32_64 (map fmt off)
          ["F(b,4)", "F(a,12)"]
          ["F(b,8)", "F(a,16)", "P(4,20)"]

      (_, _, off) <- runTest [("a", FloatRep), ("b", FloatRep)]
      assert_32_64 (map fmt off)
          ["F(a,4)", "F(b,8)"]
          ["F(a,8)", "F(b,12)"]

      (_, _, off) <- runTest [("a", FloatRep), ("b", FloatRep), ("c", FloatRep)]
      assert_32_64 (map fmt off)
          ["F(a,4)", "F(b,8)", "F(c,12)"]
          ["F(a,8)", "F(b,12)", "F(c,16)", "P(4,20)"]

      (_, _, off) <- runTest [("a", FloatRep), ("b", FloatRep), ("c", Int64Rep)]
      assert_32_64 (map fmt off)
          ["F(c,4)", "F(a,12)", "F(b,16)"]
          ["F(c,8)", "F(a,16)", "F(b,20)"]

      (_, _, off) <- runTest [("a", Int64Rep), ("b", FloatRep), ("c", FloatRep)]
      assert_32_64 (map fmt off)
          ["F(a,4)", "F(b,12)", "F(c,16)"]
          ["F(a,8)", "F(b,16)", "F(c,20)"]

      (_, _, off) <- runTest [("a", Int64Rep), ("b", FloatRep), ("c", Int64Rep)]
      assert_32_64 (map fmt off)
          ["F(a,4)", "F(c,12)", "F(b,20)"]
          ["F(a,8)", "F(c,16)", "F(b,24)", "P(4,28)"]


assert_32_64 :: (Eq a, Show a) => a -> a -> a -> Ghc ()
assert_32_64 actual expected32 expected64 = do
    dflags <- getDynFlags
    let
      expected
          | word_size == 4 = expected32
          | word_size == 8 = expected64
      word_size = pc_WORD_SIZE (platformConstants (targetPlatform dflags))
    case actual == expected of
        True -> return ()
        False ->
            error $ "Expected:\n" ++ show expected
                 ++ "\nBut got:\n" ++ show actual

runTest :: [(a, PrimRep)] -> Ghc (WordOff , WordOff, [FieldOffOrPadding a])
runTest prim_reps = do
    dflags <- getDynFlags
    return $ mkVirtHeapOffsetsWithPadding (targetProfile dflags) StdHeader (mkNonVoids prim_reps)
  where
    mkNonVoids = map (\(a, prim_rep) -> NonVoid (prim_rep, a))

fmt :: FieldOffOrPadding String -> String
fmt (FieldOff (NonVoid id) off) = "F(" ++ id ++ "," ++ show off ++ ")"
fmt (Padding len off) = "P(" ++ show len ++ "," ++ show off ++ ")"
