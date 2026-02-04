{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module QuasiJoinPoints where

import Data.Coerce
  ( coerce )
import Data.Kind
  ( Type )


import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

-----------------------------------
-- Join points and profiling ticks

data ModGuts2 = MkModGuts2

runCorePasses3 :: Bool -> ModGuts2 -> IO ModGuts2
runCorePasses3 pass guts = doCorePass3 pass guts

doCorePass3 :: Bool -> ModGuts2 -> IO ModGuts2
doCorePass3 pass guts = do
  _ <- putStrLn "hi"

  let
    updateBinds _ = return guts

  case pass of
    True -> {-# SCC "XXX3" #-} updateBinds False
    _ -> {-# SCC "YYY3" #-} updateBinds True

--------------------------
-- Join points & casts

newtype AdjacencyMap a = AM {
    adjacencyMap :: Map a (Set.Set a) }

overlays :: Ord a => [AdjacencyMap a] -> AdjacencyMap a
overlays = AM . Map.unionsWith Set.union . map adjacencyMap


type SBool :: Bool -> Type
data SBool b where
  SFalse :: SBool False
  STrue  :: SBool True

type N :: Bool -> Type
data family N b
newtype instance N False = NF ( Int -> Int )
newtype instance N True  = NT ( Int -> Int )

testCast :: forall b. SBool b -> Int -> Int
testCast b n =
  case
    ( let
        {-# NOINLINE juliet #-}
        juliet :: Int -> Int -> Int
        juliet x = \ y -> x + y + n
      in
      case b of
        SFalse -> NF (juliet 1)
        STrue  -> NT (juliet 2)
    ) :: N b of
      n | SFalse <- b
        , NF f <- n
        -> f 100
        | STrue <- b
        , NT g <- n
        -> g 200


------------------------------------------
-- Join points, profiling ticks and casts

newtype M = M ( Int -> Int -> Int )

testCastTick :: forall b. SBool b -> Int -> Int
testCastTick b n =
  case
    ( let
        {-# NOINLINE j #-}
        j :: Int -> Int -> Int
        j x = \ y -> x + y + n
        {-# NOINLINE k #-}
        k :: M
        k = coerce j
      in
      case b of
        SFalse -> {-# SCC "ticked" #-} NF ( coerce @M @( Int -> Int -> Int ) k 1 )
        STrue  -> NT ( coerce @M @( Int -> Int -> Int ) k 2 )
    ) :: N b of
      n | SFalse <- b
        , NF f <- n
        -> f 100
        | STrue <- b
        , NT g <- n
        -> g 200

------------------------------------------
-- Test for Wrinkle [Transitivity of quasi join points]

{-# NOINLINE testQuasiTransitivity #-}
testQuasiTransitivity :: Bool -> Int -> Int
testQuasiTransitivity b n =
  let
    f x = x ^ ( 99 :: Int ) + 7 * ( x - 19 )
    {-# NOINLINE f #-}
  in
    f (
      let
        j1 :: Int -> Int
        j1 x = x + n
        {-# NOINLINE j1 #-}

        j2 :: Int -> Int
        j2 y = j1 (y * 2)
        {-# NOINLINE j2 #-}

        j3 :: Int -> Int
        j3 z = j2 (z * 3)
        {-# NOINLINE j3 #-}

      in case b of
        True  -> {-# SCC "ticked" #-} j3 10
        False -> j3 20
    )

--------------------------------------------------------------------------------
-- Test relating to Note [TailCallInfo is conservative]

expt :: Int -> Int
expt _ = 3
{-# NOINLINE expt #-}

repro :: (Int, Int) -> (Int, Int)
repro (f0,e0) =
 let
  (f,e) =
    let n = e0
    in
      case n > 0 of
        True  -> (f0, e0 + n)
        False -> (f0, e0)
  r = let be = expt e in f * be
  in
    (r, 7)
