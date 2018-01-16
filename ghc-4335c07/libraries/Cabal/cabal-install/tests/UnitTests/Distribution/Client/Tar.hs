module UnitTests.Distribution.Client.Tar (
  tests
  ) where

import Distribution.Client.Tar ( filterEntries
                               , filterEntriesM
                               )
import Codec.Archive.Tar       ( Entries(..)
                               , foldEntries
                               )
import Codec.Archive.Tar.Entry ( EntryContent(..)
                               , simpleEntry
                               , Entry(..)
                               , toTarPath
                               )

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS.Char8
import Control.Monad.Writer.Lazy (runWriterT, tell)

tests :: [TestTree]
tests = [ testCase "filterEntries" filterTest
        , testCase "filterEntriesM" filterMTest
        ]

filterTest :: Assertion
filterTest = do
  let e1 = getFileEntry "file1" "x"
      e2 = getFileEntry "file2" "y"
      p = (\e -> let (NormalFile dta _) = entryContent e
                     str = BS.Char8.unpack dta
                 in str /= "y")
  assertEqual "Unexpected result for filter" "xz" $
    entriesToString $ filterEntries p $ Next e1 $ Next e2 Done
  assertEqual "Unexpected result for filter" "z" $
    entriesToString $ filterEntries p $ Done
  assertEqual "Unexpected result for filter" "xf" $
    entriesToString $ filterEntries p $ Next e1 $ Next e2 $ Fail "f"

filterMTest :: Assertion
filterMTest = do
  let e1 = getFileEntry "file1" "x"
      e2 = getFileEntry "file2" "y"
      p = (\e -> let (NormalFile dta _) = entryContent e
                     str = BS.Char8.unpack dta
                 in tell "t" >> return (str /= "y"))

  (r, w) <- runWriterT $ filterEntriesM p $ Next e1 $ Next e2 Done
  assertEqual "Unexpected result for filterM" "xz" $ entriesToString r
  assertEqual "Unexpected result for filterM w" "tt" w

  (r1, w1) <- runWriterT $ filterEntriesM p $ Done
  assertEqual "Unexpected result for filterM" "z" $ entriesToString r1
  assertEqual "Unexpected result for filterM w" "" w1

  (r2, w2) <- runWriterT $ filterEntriesM p $ Next e1 $ Next e2 $ Fail "f"
  assertEqual "Unexpected result for filterM" "xf" $ entriesToString r2
  assertEqual "Unexpected result for filterM w" "tt" w2

getFileEntry :: FilePath -> [Char] -> Entry
getFileEntry pth dta =
  simpleEntry tp $ NormalFile dta' $ BS.length dta'
  where  tp = case toTarPath False pth of
           Right tp' -> tp'
           Left e -> error e
         dta' = BS.Char8.pack dta

entriesToString :: Entries String -> String
entriesToString =
  foldEntries (\e acc -> let (NormalFile dta _) = entryContent e
                             str = BS.Char8.unpack dta
                          in str ++ acc) "z" id
