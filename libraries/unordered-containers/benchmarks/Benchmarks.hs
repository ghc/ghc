{-# LANGUAGE CPP, DeriveAnyClass, DeriveGeneric, GADTs, PackageImports, RecordWildCards #-}

module Main where

import Control.DeepSeq
import Gauge (bench, bgroup, defaultMain, env, nf, whnf)
import Data.Bits ((.&.))
import Data.Functor.Identity
import Data.Hashable (Hashable, hash)
import qualified Data.ByteString as BS
import qualified "hashmap" Data.HashMap as IHM
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Prelude hiding (lookup)

import qualified Util.ByteString as UBS
import qualified Util.Int as UI
import qualified Util.String as US

#if !MIN_VERSION_bytestring(0,10,0)
instance NFData BS.ByteString
#endif

data B where
    B :: NFData a => a -> B

instance NFData B where
    rnf (B b) = rnf b

-- TODO: This a stopgap measure to keep the benchmark work with
-- Criterion 1.0.
data Env = Env {
    n :: !Int,

    elems   :: ![(String, Int)],
    keys    :: ![String],
    elemsBS :: ![(BS.ByteString, Int)],
    keysBS  :: ![BS.ByteString],
    elemsI  :: ![(Int, Int)],
    keysI   :: ![Int],
    elemsI2 :: ![(Int, Int)],  -- for union

    keys'    :: ![String],
    keysBS'  :: ![BS.ByteString],
    keysI'   :: ![Int],

    keysDup    :: ![String],
    keysDupBS  :: ![BS.ByteString],
    keysDupI   :: ![Int],
    elemsDup   :: ![(String, Int)],
    elemsDupBS :: ![(BS.ByteString, Int)],
    elemsDupI  :: ![(Int, Int)],

    hm          :: !(HM.HashMap String Int),
    hmSubset    :: !(HM.HashMap String Int),
    hmbs        :: !(HM.HashMap BS.ByteString Int),
    hmbsSubset  :: !(HM.HashMap BS.ByteString Int),
    hmi         :: !(HM.HashMap Int Int),
    hmiSubset   :: !(HM.HashMap Int Int),
    hmi2        :: !(HM.HashMap Int Int),
    m           :: !(M.Map String Int),
    mSubset     :: !(M.Map String Int),
    mbs         :: !(M.Map BS.ByteString Int),
    mbsSubset   :: !(M.Map BS.ByteString Int),
    im          :: !(IM.IntMap Int),
    imSubset    :: !(IM.IntMap Int),
    ihm         :: !(IHM.Map String Int),
    ihmSubset   :: !(IHM.Map String Int),
    ihmbs       :: !(IHM.Map BS.ByteString Int),
    ihmbsSubset :: !(IHM.Map BS.ByteString Int)
    } deriving (Generic, NFData)

setupEnv :: IO Env
setupEnv = do
    let n = 2^(12 :: Int)

        elems   = zip keys [1..n]
        keys    = US.rnd 8 n
        elemsBS = zip keysBS [1..n]
        keysBS  = UBS.rnd 8 n
        elemsI  = zip keysI [1..n]
        keysI   = UI.rnd (n+n) n
        elemsI2 = zip [n `div` 2..n + (n `div` 2)] [1..n]  -- for union

        keys'    = US.rnd' 8 n
        keysBS'  = UBS.rnd' 8 n
        keysI'   = UI.rnd' (n+n) n

        keysDup    = US.rnd 2 n
        keysDupBS  = UBS.rnd 2 n
        keysDupI   = UI.rnd (n`div`4) n
        elemsDup   = zip keysDup [1..n]
        elemsDupBS = zip keysDupBS [1..n]
        elemsDupI  = zip keysDupI [1..n]

        hm          = HM.fromList elems
        hmSubset    = HM.fromList (takeSubset n elems)
        hmbs        = HM.fromList elemsBS
        hmbsSubset  = HM.fromList (takeSubset n elemsBS)
        hmi         = HM.fromList elemsI
        hmiSubset   = HM.fromList (takeSubset n elemsI)
        hmi2        = HM.fromList elemsI2
        m           = M.fromList elems
        mSubset     = M.fromList (takeSubset n elems)
        mbs         = M.fromList elemsBS
        mbsSubset   = M.fromList (takeSubset n elemsBS)
        im          = IM.fromList elemsI
        imSubset    = IM.fromList (takeSubset n elemsI)
        ihm         = IHM.fromList elems
        ihmSubset   = IHM.fromList (takeSubset n elems)
        ihmbs       = IHM.fromList elemsBS
        ihmbsSubset = IHM.fromList (takeSubset n elemsBS)
    return Env{..}
  where
    takeSubset n elements =
      -- use 50% of the elements for a subset check.
      let subsetSize = round (fromIntegral n * 0.5 :: Double) :: Int
      in take subsetSize elements

main :: IO ()
main = do
    defaultMain
        [
          env setupEnv $ \ ~(Env{..}) ->
          -- * Comparison to other data structures
          -- ** Map
          bgroup "Map"
          [ bgroup "lookup"
            [ bench "String" $ whnf (lookupM keys) m
            , bench "ByteString" $ whnf (lookupM keysBS) mbs
            ]
          , bgroup "lookup-miss"
            [ bench "String" $ whnf (lookupM keys') m
            , bench "ByteString" $ whnf (lookupM keysBS') mbs
            ]
          , bgroup "insert"
            [ bench "String" $ whnf (insertM elems) M.empty
            , bench "ByteStringString" $ whnf (insertM elemsBS) M.empty
            ]
          , bgroup "insert-dup"
            [ bench "String" $ whnf (insertM elems) m
            , bench "ByteStringString" $ whnf (insertM elemsBS) mbs
            ]
          , bgroup "delete"
            [ bench "String" $ whnf (deleteM keys) m
            , bench "ByteString" $ whnf (deleteM keysBS) mbs
            ]
          , bgroup "delete-miss"
            [ bench "String" $ whnf (deleteM keys') m
            , bench "ByteString" $ whnf (deleteM keysBS') mbs
            ]
          , bgroup "size"
            [ bench "String" $ whnf M.size m
            , bench "ByteString" $ whnf M.size mbs
            ]
          , bgroup "fromList"
            [ bench "String" $ whnf M.fromList elems
            , bench "ByteString" $ whnf M.fromList elemsBS
            ]
          , bgroup "isSubmapOf"
            [ bench "String" $ whnf (M.isSubmapOf mSubset) m
            , bench "ByteString" $ whnf (M.isSubmapOf mbsSubset) mbs
            ]
          ]

          -- ** Map from the hashmap package
        , env setupEnv $ \ ~(Env{..}) ->
          bgroup "hashmap/Map"
          [ bgroup "lookup"
            [ bench "String" $ whnf (lookupIHM keys) ihm
            , bench "ByteString" $ whnf (lookupIHM keysBS) ihmbs
            ]
          , bgroup "lookup-miss"
            [ bench "String" $ whnf (lookupIHM keys') ihm
            , bench "ByteString" $ whnf (lookupIHM keysBS') ihmbs
            ]
          , bgroup "insert"
            [ bench "String" $ whnf (insertIHM elems) IHM.empty
            , bench "ByteStringString" $ whnf (insertIHM elemsBS) IHM.empty
            ]
          , bgroup "insert-dup"
            [ bench "String" $ whnf (insertIHM elems) ihm
            , bench "ByteStringString" $ whnf (insertIHM elemsBS) ihmbs
            ]
          , bgroup "delete"
            [ bench "String" $ whnf (deleteIHM keys) ihm
            , bench "ByteString" $ whnf (deleteIHM keysBS) ihmbs
            ]
          , bgroup "delete-miss"
            [ bench "String" $ whnf (deleteIHM keys') ihm
            , bench "ByteString" $ whnf (deleteIHM keysBS') ihmbs
            ]
          , bgroup "size"
            [ bench "String" $ whnf IHM.size ihm
            , bench "ByteString" $ whnf IHM.size ihmbs
            ]
          , bgroup "fromList"
            [ bench "String" $ whnf IHM.fromList elems
            , bench "ByteString" $ whnf IHM.fromList elemsBS
            ]
          , bgroup "isSubmapOf"
            [ bench "String" $ whnf (IHM.isSubmapOf ihmSubset) ihm
            , bench "ByteString" $ whnf (IHM.isSubmapOf ihmbsSubset) ihmbs
            ]
          , bgroup "hash"
            [ bench "String" $ whnf hash hm
            , bench "ByteString" $ whnf hash hmbs
            ]
          ]

          -- ** IntMap
        , env setupEnv $ \ ~(Env{..}) ->
          bgroup "IntMap"
          [ bench "lookup" $ whnf (lookupIM keysI) im
          , bench "lookup-miss" $ whnf (lookupIM keysI') im
          , bench "insert" $ whnf (insertIM elemsI) IM.empty
          , bench "insert-dup" $ whnf (insertIM elemsI) im
          , bench "delete" $ whnf (deleteIM keysI) im
          , bench "delete-miss" $ whnf (deleteIM keysI') im
          , bench "size" $ whnf IM.size im
          , bench "fromList" $ whnf IM.fromList elemsI
          , bench "isSubmapOf" $ whnf (IM.isSubmapOf imSubset) im
          ]

        , env setupEnv $ \ ~(Env{..}) ->
          bgroup "HashMap"
          [ -- * Basic interface
            bgroup "lookup"
            [ bench "String" $ whnf (lookup keys) hm
            , bench "ByteString" $ whnf (lookup keysBS) hmbs
            , bench "Int" $ whnf (lookup keysI) hmi
            ]
          , bgroup "lookup-miss"
            [ bench "String" $ whnf (lookup keys') hm
            , bench "ByteString" $ whnf (lookup keysBS') hmbs
            , bench "Int" $ whnf (lookup keysI') hmi
            ]
          , bgroup "insert"
            [ bench "String" $ whnf (insert elems) HM.empty
            , bench "ByteString" $ whnf (insert elemsBS) HM.empty
            , bench "Int" $ whnf (insert elemsI) HM.empty
            ]
          , bgroup "insert-dup"
            [ bench "String" $ whnf (insert elems) hm
            , bench "ByteString" $ whnf (insert elemsBS) hmbs
            , bench "Int" $ whnf (insert elemsI) hmi
            ]
          , bgroup "delete"
            [ bench "String" $ whnf (delete keys) hm
            , bench "ByteString" $ whnf (delete keysBS) hmbs
            , bench "Int" $ whnf (delete keysI) hmi
            ]
          , bgroup "delete-miss"
            [ bench "String" $ whnf (delete keys') hm
            , bench "ByteString" $ whnf (delete keysBS') hmbs
            , bench "Int" $ whnf (delete keysI') hmi
            ]
          , bgroup "alterInsert"
            [ bench "String" $ whnf (alterInsert elems) HM.empty
            , bench "ByteString" $ whnf (alterInsert elemsBS) HM.empty
            , bench "Int" $ whnf (alterInsert elemsI) HM.empty
            ]
          , bgroup "alterFInsert"
            [ bench "String" $ whnf (alterFInsert elems) HM.empty
            , bench "ByteString" $ whnf (alterFInsert elemsBS) HM.empty
            , bench "Int" $ whnf (alterFInsert elemsI) HM.empty
            ]
          , bgroup "alterInsert-dup"
            [ bench "String" $ whnf (alterInsert elems) hm
            , bench "ByteString" $ whnf (alterInsert elemsBS) hmbs
            , bench "Int" $ whnf (alterInsert elemsI) hmi
            ]
          , bgroup "alterFInsert-dup"
            [ bench "String" $ whnf (alterFInsert elems) hm
            , bench "ByteString" $ whnf (alterFInsert elemsBS) hmbs
            , bench "Int" $ whnf (alterFInsert elemsI) hmi
            ]
          , bgroup "alterDelete"
            [ bench "String" $ whnf (alterDelete keys) hm
            , bench "ByteString" $ whnf (alterDelete keysBS) hmbs
            , bench "Int" $ whnf (alterDelete keysI) hmi
            ]
          , bgroup "alterFDelete"
            [ bench "String" $ whnf (alterFDelete keys) hm
            , bench "ByteString" $ whnf (alterFDelete keysBS) hmbs
            , bench "Int" $ whnf (alterFDelete keysI) hmi
            ]
          , bgroup "alterDelete-miss"
            [ bench "String" $ whnf (alterDelete keys') hm
            , bench "ByteString" $ whnf (alterDelete keysBS') hmbs
            , bench "Int" $ whnf (alterDelete keysI') hmi
            ]
          , bgroup "alterFDelete-miss"
            [ bench "String" $ whnf (alterFDelete keys') hm
            , bench "ByteString" $ whnf (alterFDelete keysBS') hmbs
            , bench "Int" $ whnf (alterFDelete keysI') hmi
            ]
          , bgroup "isSubmapOf"
            [ bench "String" $ whnf (HM.isSubmapOf hmSubset) hm
            , bench "ByteString" $ whnf (HM.isSubmapOf hmbsSubset) hmbs
            , bench "Int" $ whnf (HM.isSubmapOf hmiSubset) hmi
            ]
          , bgroup "isSubmapOfNaive"
            [ bench "String" $ whnf (isSubmapOfNaive hmSubset) hm
            , bench "ByteString" $ whnf (isSubmapOfNaive hmbsSubset) hmbs
            , bench "Int" $ whnf (isSubmapOfNaive hmiSubset) hmi
            ]

            -- Combine
          , bench "union" $ whnf (HM.union hmi) hmi2

            -- Transformations
          , bench "map" $ whnf (HM.map (\ v -> v + 1)) hmi

            -- * Difference and intersection
          , bench "difference" $ whnf (HM.difference hmi) hmi2
          , bench "intersection" $ whnf (HM.intersection hmi) hmi2

            -- Folds
          , bench "foldl'" $ whnf (HM.foldl' (+) 0) hmi
          , bench "foldr" $ nf (HM.foldr (:) []) hmi

            -- Filter
          , bench "filter" $ whnf (HM.filter (\ v -> v .&. 1 == 0)) hmi
          , bench "filterWithKey" $ whnf (HM.filterWithKey (\ k _ -> k .&. 1 == 0)) hmi

            -- Size
          , bgroup "size"
            [ bench "String" $ whnf HM.size hm
            , bench "ByteString" $ whnf HM.size hmbs
            , bench "Int" $ whnf HM.size hmi
            ]

            -- fromList
          , bgroup "fromList"
            [ bgroup "long"
              [ bench "String" $ whnf HM.fromList elems
              , bench "ByteString" $ whnf HM.fromList elemsBS
              , bench "Int" $ whnf HM.fromList elemsI
              ]
            , bgroup "short"
              [ bench "String" $ whnf HM.fromList elemsDup
              , bench "ByteString" $ whnf HM.fromList elemsDupBS
              , bench "Int" $ whnf HM.fromList elemsDupI
              ]
            ]
            -- fromListWith
          , bgroup "fromListWith"
            [ bgroup "long"
              [ bench "String" $ whnf (HM.fromListWith (+)) elems
              , bench "ByteString" $ whnf (HM.fromListWith (+)) elemsBS
              , bench "Int" $ whnf (HM.fromListWith (+)) elemsI
              ]
            , bgroup "short"
              [ bench "String" $ whnf (HM.fromListWith (+)) elemsDup
              , bench "ByteString" $ whnf (HM.fromListWith (+)) elemsDupBS
              , bench "Int" $ whnf (HM.fromListWith (+)) elemsDupI
              ]
            ]
          ]
        ]

------------------------------------------------------------------------
-- * HashMap

lookup :: (Eq k, Hashable k) => [k] -> HM.HashMap k Int -> Int
lookup xs m = foldl' (\z k -> fromMaybe z (HM.lookup k m)) 0 xs
{-# SPECIALIZE lookup :: [Int] -> HM.HashMap Int Int -> Int #-}
{-# SPECIALIZE lookup :: [String] -> HM.HashMap String Int -> Int #-}
{-# SPECIALIZE lookup :: [BS.ByteString] -> HM.HashMap BS.ByteString Int
                      -> Int #-}

insert :: (Eq k, Hashable k) => [(k, Int)] -> HM.HashMap k Int
       -> HM.HashMap k Int
insert xs m0 = foldl' (\m (k, v) -> HM.insert k v m) m0 xs
{-# SPECIALIZE insert :: [(Int, Int)] -> HM.HashMap Int Int
                      -> HM.HashMap Int Int #-}
{-# SPECIALIZE insert :: [(String, Int)] -> HM.HashMap String Int
                      -> HM.HashMap String Int #-}
{-# SPECIALIZE insert :: [(BS.ByteString, Int)] -> HM.HashMap BS.ByteString Int
                      -> HM.HashMap BS.ByteString Int #-}

delete :: (Eq k, Hashable k) => [k] -> HM.HashMap k Int -> HM.HashMap k Int
delete xs m0 = foldl' (\m k -> HM.delete k m) m0 xs
{-# SPECIALIZE delete :: [Int] -> HM.HashMap Int Int -> HM.HashMap Int Int #-}
{-# SPECIALIZE delete :: [String] -> HM.HashMap String Int
                      -> HM.HashMap String Int #-}
{-# SPECIALIZE delete :: [BS.ByteString] -> HM.HashMap BS.ByteString Int
                      -> HM.HashMap BS.ByteString Int #-}

alterInsert :: (Eq k, Hashable k) => [(k, Int)] -> HM.HashMap k Int
             -> HM.HashMap k Int
alterInsert xs m0 =
  foldl' (\m (k, v) -> HM.alter (const . Just $ v) k m) m0 xs
{-# SPECIALIZE alterInsert :: [(Int, Int)] -> HM.HashMap Int Int
                           -> HM.HashMap Int Int #-}
{-# SPECIALIZE alterInsert :: [(String, Int)] -> HM.HashMap String Int
                           -> HM.HashMap String Int #-}
{-# SPECIALIZE alterInsert :: [(BS.ByteString, Int)] -> HM.HashMap BS.ByteString Int
                           -> HM.HashMap BS.ByteString Int #-}

alterDelete :: (Eq k, Hashable k) => [k] -> HM.HashMap k Int
             -> HM.HashMap k Int
alterDelete xs m0 =
  foldl' (\m k -> HM.alter (const Nothing) k m) m0 xs
{-# SPECIALIZE alterDelete :: [Int] -> HM.HashMap Int Int
                           -> HM.HashMap Int Int #-}
{-# SPECIALIZE alterDelete :: [String] -> HM.HashMap String Int
                           -> HM.HashMap String Int #-}
{-# SPECIALIZE alterDelete :: [BS.ByteString] -> HM.HashMap BS.ByteString Int
                           -> HM.HashMap BS.ByteString Int #-}

alterFInsert :: (Eq k, Hashable k) => [(k, Int)] -> HM.HashMap k Int
             -> HM.HashMap k Int
alterFInsert xs m0 =
  foldl' (\m (k, v) -> runIdentity $ HM.alterF (const . Identity . Just $ v) k m) m0 xs
{-# SPECIALIZE alterFInsert :: [(Int, Int)] -> HM.HashMap Int Int
                            -> HM.HashMap Int Int #-}
{-# SPECIALIZE alterFInsert :: [(String, Int)] -> HM.HashMap String Int
                            -> HM.HashMap String Int #-}
{-# SPECIALIZE alterFInsert :: [(BS.ByteString, Int)] -> HM.HashMap BS.ByteString Int
                            -> HM.HashMap BS.ByteString Int #-}

alterFDelete :: (Eq k, Hashable k) => [k] -> HM.HashMap k Int
             -> HM.HashMap k Int
alterFDelete xs m0 =
  foldl' (\m k -> runIdentity $ HM.alterF (const . Identity $ Nothing) k m) m0 xs
{-# SPECIALIZE alterFDelete :: [Int] -> HM.HashMap Int Int
                            -> HM.HashMap Int Int #-}
{-# SPECIALIZE alterFDelete :: [String] -> HM.HashMap String Int
                            -> HM.HashMap String Int #-}
{-# SPECIALIZE alterFDelete :: [BS.ByteString] -> HM.HashMap BS.ByteString Int
                            -> HM.HashMap BS.ByteString Int #-}

isSubmapOfNaive :: (Eq k, Hashable k) => HM.HashMap k Int -> HM.HashMap k Int -> Bool
isSubmapOfNaive m1 m2 = and [ Just v1 == HM.lookup k1 m2 | (k1,v1) <- HM.toList m1 ]
{-# SPECIALIZE isSubmapOfNaive :: HM.HashMap Int Int -> HM.HashMap Int Int -> Bool #-}
{-# SPECIALIZE isSubmapOfNaive :: HM.HashMap String Int -> HM.HashMap String Int -> Bool #-}
{-# SPECIALIZE isSubmapOfNaive :: HM.HashMap BS.ByteString Int -> HM.HashMap BS.ByteString Int -> Bool #-}

------------------------------------------------------------------------
-- * Map

lookupM :: Ord k => [k] -> M.Map k Int -> Int
lookupM xs m = foldl' (\z k -> fromMaybe z (M.lookup k m)) 0 xs
{-# SPECIALIZE lookupM :: [String] -> M.Map String Int -> Int #-}
{-# SPECIALIZE lookupM :: [BS.ByteString] -> M.Map BS.ByteString Int -> Int #-}

insertM :: Ord k => [(k, Int)] -> M.Map k Int -> M.Map k Int
insertM xs m0 = foldl' (\m (k, v) -> M.insert k v m) m0 xs
{-# SPECIALIZE insertM :: [(String, Int)] -> M.Map String Int
                       -> M.Map String Int #-}
{-# SPECIALIZE insertM :: [(BS.ByteString, Int)] -> M.Map BS.ByteString Int
                       -> M.Map BS.ByteString Int #-}

deleteM :: Ord k => [k] -> M.Map k Int -> M.Map k Int
deleteM xs m0 = foldl' (\m k -> M.delete k m) m0 xs
{-# SPECIALIZE deleteM :: [String] -> M.Map String Int -> M.Map String Int #-}
{-# SPECIALIZE deleteM :: [BS.ByteString] -> M.Map BS.ByteString Int
                       -> M.Map BS.ByteString Int #-}

------------------------------------------------------------------------
-- * Map from the hashmap package

lookupIHM :: (Eq k, Hashable k, Ord k) => [k] -> IHM.Map k Int -> Int
lookupIHM xs m = foldl' (\z k -> fromMaybe z (IHM.lookup k m)) 0 xs
{-# SPECIALIZE lookupIHM :: [String] -> IHM.Map String Int -> Int #-}
{-# SPECIALIZE lookupIHM :: [BS.ByteString] -> IHM.Map BS.ByteString Int
                         -> Int #-}

insertIHM :: (Eq k, Hashable k, Ord k) => [(k, Int)] -> IHM.Map k Int
          -> IHM.Map k Int
insertIHM xs m0 = foldl' (\m (k, v) -> IHM.insert k v m) m0 xs
{-# SPECIALIZE insertIHM :: [(String, Int)] -> IHM.Map String Int
                         -> IHM.Map String Int #-}
{-# SPECIALIZE insertIHM :: [(BS.ByteString, Int)] -> IHM.Map BS.ByteString Int
                         -> IHM.Map BS.ByteString Int #-}

deleteIHM :: (Eq k, Hashable k, Ord k) => [k] -> IHM.Map k Int -> IHM.Map k Int
deleteIHM xs m0 = foldl' (\m k -> IHM.delete k m) m0 xs
{-# SPECIALIZE deleteIHM :: [String] -> IHM.Map String Int
                         -> IHM.Map String Int #-}
{-# SPECIALIZE deleteIHM :: [BS.ByteString] -> IHM.Map BS.ByteString Int
                         -> IHM.Map BS.ByteString Int #-}

------------------------------------------------------------------------
-- * IntMap

lookupIM :: [Int] -> IM.IntMap Int -> Int
lookupIM xs m = foldl' (\z k -> fromMaybe z (IM.lookup k m)) 0 xs

insertIM :: [(Int, Int)] -> IM.IntMap Int -> IM.IntMap Int
insertIM xs m0 = foldl' (\m (k, v) -> IM.insert k v m) m0 xs

deleteIM :: [Int] -> IM.IntMap Int -> IM.IntMap Int
deleteIM xs m0 = foldl' (\m k -> IM.delete k m) m0 xs
