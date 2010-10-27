module Main where

-- This test works efficiently because the full laziness
-- pass now floats out applications
-- 	\x -> f y (x+1)
-- It'll float out the (f y) if that's a redex

loop :: Double -> [Int] -> Double
{-# NOINLINE loop #-}
loop x [] = x
loop x (n:ns) = x `seq` loop (x ^ n) ns

main = print $ loop 1 (replicate 10000000 5)

----------------------------------------------------
{-  Roman's message of May 2010

I tried running nofib with -fno-method-sharing (we discussed this at some point). These are the results:

--------------------------------------------------------------------------------
        Program           Size    Allocs   Runtime   Elapsed
--------------------------------------------------------------------------------

            Min          -0.3%    -25.0%    -12.5%     -9.9%
            Max          +0.2%   +159.1%    +90.0%    +84.7%
 Geometric Mean          -0.0%     +2.2%     +6.8%     +5.1%

This is the worst program:

         simple          +0.2%   +159.1%    +65.3%    +63.9%

I looked at it a bit and came up with this small example:

----
loop :: Double -> [Int] -> Double
{-# NOINLINE loop #-}
loop x [] = x
loop x (n:ns) = x `seq` loop (x ^ n) ns

main = print $ loop 1 (replicate 10000000 5)
----

This is over 2x slower with -fno-method-sharing. The culprit is, of
course, (^). Here is the difference:

Without -fno-method-sharing:

----
^_rVB :: GHC.Types.Double -> GHC.Types.Int -> GHC.Types.Double ^_rVB =
  GHC.Real.^
    @ GHC.Types.Double
    @ GHC.Types.Int
    GHC.Float.$fNumDouble
    GHC.Real.$fIntegralInt

Main.loop [InlPrag=NOINLINE (sat-args=2), Occ=LoopBreaker]
  :: GHC.Types.Double -> [GHC.Types.Int] -> GHC.Types.Double Main.loop =
  \ (x1_aat :: GHC.Types.Double) (ds_drG :: [GHC.Types.Int]) ->
    case ds_drG of _ {
      [] -> x1_aat;
      : n_aav ns_aaw ->
        case x1_aat of x2_aau { GHC.Types.D# ipv_srQ ->
        Main.loop (^_rVB x2_aau n_aav) ns_aaw
        }
    }
----

With:

----
Main.loop [InlPrag=NOINLINE (sat-args=2), Occ=LoopBreaker]
  :: GHC.Types.Double -> [GHC.Types.Int] -> GHC.Types.Double Main.loop =
  \ (x1_aat :: GHC.Types.Double) (ds_drD :: [GHC.Types.Int]) ->
    case ds_drD of _ {
      [] -> x1_aat;
      : n_aav ns_aaw ->
        case x1_aat of x2_aau { GHC.Types.D# ipv_srN ->
        Main.loop
          (GHC.Real.^
             @ GHC.Types.Double
             @ GHC.Types.Int
             GHC.Float.$fNumDouble
             GHC.Real.$fIntegralInt
             x2_aau
             n_aav)
          ns_aaw
        }
    }
----

This is a bit disappointing. I would have expected GHC to float out
the application of (^) to the two dictionaries during full laziness
(note that (^) has arity 2 so the application is oversaturated). Why
doesn't that happen? SetLevels (if this is the right place to look)
has this:
 
-}