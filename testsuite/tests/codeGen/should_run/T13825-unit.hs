module Main where

import DynFlags
import RepType
import SMRep
import StgCmmLayout
import StgCmmClosure
import GHC
import GhcMonad
import System.Environment
import Platform

main :: IO ()
main = do
    [libdir] <- getArgs
    runGhc (Just libdir) tests


-- How to read tests:
--   F(a,8) = field a at offset 8
--   P(4,8) = 4 bytes of padding at offset 8
tests :: Ghc ()
tests = do
      (_, _, off) <- runTest [("a", FloatRep), ("b", DoubleRep)]
      assert_32_64 (map fmt off)
          ["F(a,4)", "F(b,8)"]
          ["F(a,8)", "P(4,12)", "F(b,16)"]

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
          ["F(a,4)", "F(b,8)", "F(c,12)"]
          ["F(a,8)", "F(b,12)", "F(c,16)"]

      (_, _, off) <- runTest [("a", Int64Rep), ("b", FloatRep), ("c", FloatRep)]
      assert_32_64 (map fmt off)
          ["F(a,4)", "F(b,12)", "F(c,16)"]
          ["F(a,8)", "F(b,16)", "F(c,20)"]

      (_, _, off) <- runTest [("a", Int64Rep), ("b", FloatRep), ("c", Int64Rep)]
      assert_32_64 (map fmt off)
          ["F(a,4)", "F(b,12)", "F(c,16)"]
          ["F(a,8)", "F(b,16)", "P(4,20)", "F(c,24)"]


assert_32_64 :: (Eq a, Show a) => a -> a -> a -> Ghc ()
assert_32_64 actual expected32 expected64 = do
    dflags <- getDynFlags
    let
      expected
          | word_size == 4 = expected32
          | word_size == 8 = expected64
      word_size = wORD_SIZE dflags
    case actual == expected of
        True -> return ()
        False ->
            error $ "Expected:\n" ++ show expected
                 ++ "\nBut got:\n" ++ show actual

runTest :: [(a, PrimRep)] -> Ghc (WordOff , WordOff, [FieldOffOrPadding a])
runTest prim_reps = do
    dflags <- getDynFlags
    return $ mkVirtHeapOffsetsWithPadding dflags StdHeader (mkNonVoids prim_reps)
  where
    mkNonVoids = map (\(a, prim_rep) -> NonVoid (prim_rep, a))

fmt :: FieldOffOrPadding String -> String
fmt (FieldOff (NonVoid id) off) = "F(" ++ id ++ "," ++ show off ++ ")"
fmt (Padding len off) = "P(" ++ show len ++ "," ++ show off ++ ")"
