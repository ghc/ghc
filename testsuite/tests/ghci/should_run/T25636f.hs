{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedDatatypes #-}

module Main where

import Prelude
import Control.Monad (unless)
import Data.Array.Unboxed (UArray, listArray)
import qualified Data.ByteString.Char8 as BS
import Data.Word (Word)
import GHC.Data.SmallArray (smallArrayFromList)
import GHC.Exts
import GHCi.CreateBCO (createBCOs)
import GHCi.InfoTable (mkConInfoTable)
import GHCi.RemoteTypes (HValue(..), HValueRef, RemotePtr, localRef, toRemotePtr)
import GHCi.ResolvedBCO
  ( BCOByteArray
  , ResolvedBCO(..)
  , ResolvedBCOPtr(..)
  , isLittleEndian
  , mkBCOByteArray
  )
import qualified GHC.Exts.Heap as Heap

type UTree :: UnliftedType
data UTree where
  LeafA :: UTree
  LeafB :: UTree
  Bin :: UTree -> UTree -> UTree

data Boxed where
  RootBox :: UTree -> Boxed
  PairBox :: UTree -> UTree -> Boxed

main :: IO ()
main = do
  leafAInfo <- mkInfoTable 1 0 "LeafA"
  leafBInfo <- mkInfoTable 2 0 "LeafB"
  binInfo   <- mkInfoTable 3 2 "Bin"
  rootInfo  <- mkInfoTable 1 1 "RootBox"
  pairInfo  <- mkInfoTable 2 2 "PairBox"
  -- This test tests the topological sorting done to unlifted constructor
  -- applications in `createBCOs` (call to `createUnliftedStaticCons`)
  -- When the topological sort isn't done, this test fails with weird results.
  refs <- createBCOs
    [ rootBoxCon rootInfo 1
    , binCon binInfo 1 2
    , binCon binInfo 3 4
    , binCon binInfo 4 3 -- recall ResolvedUnliftedStaticConRef are indices into the unl-objs-only-array
    , rootBoxCon rootInfo 0
    , leafCon leafAInfo
    , leafCon leafBInfo
    , pairBoxCon pairInfo 3 1 -- these are indices into unl-objs-array too (ResolvedUnliftedStaticConRef)
    ]
  actual <- mapM (\(unbx, ref) -> if unbx then extractUTree ref else extractBoxedRef ref)
    -- test also that output order of references from createBCOs matches the
    -- input order of Resolved objects
    $ zip [False, True, True, True, False, True, True, False] refs
  let expected =
        ["RootBox (Bin LeafA LeafB)"
        ,"Bin (Bin LeafA LeafB) (Bin LeafB LeafA)"
        ,"Bin LeafA LeafB"
        ,"Bin LeafB LeafA"
        ,"RootBox (Bin (Bin LeafA LeafB) (Bin LeafB LeafA))"
        ,"LeafA"
        ,"LeafB"
        ,"PairBox (LeafA) (Bin LeafA LeafB)"]
  unless (actual == expected) $
    putStrLn "expected result of createBCOs differ from actual!"
  print actual
  where
    leafCon leafInfo =
      ResolvedStaticCon
        { resolvedBCOIsLE = isLittleEndian
        , resolvedStaticConInfoPtr = leafInfo
        , resolvedStaticConArity = 0
        , resolvedStaticConLits = wordArray []
        , resolvedStaticConPtrs = smallArrayFromList []
        , resolvedStaticConIsUnlifted = True
        }

    binCon binInfo left right =
      ResolvedStaticCon
        { resolvedBCOIsLE = isLittleEndian
        , resolvedStaticConInfoPtr = binInfo
        , resolvedStaticConArity = 2
        , resolvedStaticConLits = wordArray []
        , resolvedStaticConPtrs =
            smallArrayFromList
              [ ResolvedUnliftedStaticConRef left
              , ResolvedUnliftedStaticConRef right
              ]
        , resolvedStaticConIsUnlifted = True
        }

    rootBoxCon rootBoxInfo tree =
      ResolvedStaticCon
        { resolvedBCOIsLE = isLittleEndian
        , resolvedStaticConInfoPtr = rootBoxInfo
        , resolvedStaticConArity = 1
        , resolvedStaticConLits = wordArray []
        , resolvedStaticConPtrs =
            smallArrayFromList [ResolvedUnliftedStaticConRef tree]
        , resolvedStaticConIsUnlifted = False
        }

    pairBoxCon pairBoxInfo left right =
      ResolvedStaticCon
        { resolvedBCOIsLE = isLittleEndian
        , resolvedStaticConInfoPtr = pairBoxInfo
        , resolvedStaticConArity = 2
        , resolvedStaticConLits = wordArray []
        , resolvedStaticConPtrs =
            smallArrayFromList
              [ ResolvedUnliftedStaticConRef left
              , ResolvedUnliftedStaticConRef right
              ]
        , resolvedStaticConIsUnlifted = False
        }

mkInfoTable :: Int -> Int -> String -> IO (RemotePtr Heap.StgInfoTable)
mkInfoTable tag ptrs desc =
  toRemotePtr <$> mkConInfoTable True ptrs 0 (tag - 1) tag (BS.pack desc)

extractBoxedRef :: HValueRef -> IO String
extractBoxedRef ref = do
  HValue hv <- localRef ref
  pure $ case unsafeCoerce# hv of
    boxed -> flattenBoxed boxed

extractUTree :: HValueRef -> IO String
extractUTree ref = do
  HValue hv <- localRef ref
  pure $ case unsafeCoerce# hv of
    utree -> flattenUTree utree

flattenBoxed :: Boxed -> String
flattenBoxed = \case
  RootBox tree -> "RootBox (" ++ flattenUTree tree ++ ")"
  PairBox left right ->
    "PairBox (" ++ flattenUTree left ++ ") (" ++ flattenUTree right ++ ")"

flattenUTree :: UTree -> String
flattenUTree = \case
  LeafA -> "LeafA"
  LeafB -> "LeafB"
  Bin left right -> "Bin " ++ par (flattenUTree left) ++ " " ++ par (flattenUTree right)
  where
    par s
      | s == "LeafA" || s == "LeafB" = s
      | otherwise = "(" ++ s ++ ")"

wordArray :: [Word] -> BCOByteArray Word
wordArray ws = mkBCOByteArray (listArray (0, length ws - 1) ws)
