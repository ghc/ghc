
module Main where

import Test.Tasty
import Test.Tasty.HUnit
import System.Exit

import qualified Bits
import qualified Builders
import qualified Datatype
import qualified Ext1
import qualified Ext2
import qualified FoldTree
import qualified FreeNames
import qualified GEq
import qualified GMapQAssoc
import qualified GRead
import qualified GShow
import qualified GShow2
import qualified GZip
import qualified GenUpTo
import qualified GetC
import qualified HList
import qualified HOPat
import qualified Labels
import qualified Newtype
import qualified Paradise
import qualified Perm
import qualified Reify
import qualified Strings
import qualified Tree
import qualified Twin
import qualified Typecase1
import qualified Typecase2
import qualified Where
import qualified XML

import qualified Encode           -- no tests, should compile
import qualified Ext              -- no tests, should compile
import qualified GRead2           -- no tests, should compile
import qualified LocalQuantors    -- no tests, should compile
import qualified NestedDatatypes  -- no tests, should compile
import qualified Polymatch        -- no tests, should compile

main = defaultMain $ testGroup "All"
  [ testCase "Datatype"   Datatype.tests
  , testCase "FoldTree"   FoldTree.tests
  , testCase "GetC"       GetC.tests
  , testCase "GMapQAssoc" GMapQAssoc.tests
  , testCase "GRead"      GRead.tests
  , testCase "GShow"      GShow.tests
  , testCase "GShow2"     GShow2.tests
  , testCase "HList"      HList.tests
  , testCase "HOPat"      HOPat.tests
  , testCase "Labels"     Labels.tests
  , testCase "Newtype"    Newtype.tests
  , testCase "Perm"       Perm.tests
  , testCase "Twin"       Twin.tests
  , testCase "Typecase1"  Typecase1.tests
  , testCase "Typecase2"  Typecase2.tests
  , testCase "Where"      Where.tests
  , testCase "XML"        XML.tests
  , testCase "Tree"       Tree.tests
  , testCase "Strings"    Strings.tests
  , testCase "Reify"      Reify.tests
  , testCase "Paradise"   Paradise.tests
  , testCase "GZip"       GZip.tests
  , testCase "GEq"        GEq.tests
  , testCase "GenUpTo"    GenUpTo.tests
  , testCase "FreeNames"  FreeNames.tests
  , testCase "Ext1"       Ext1.tests
  , testCase "Ext2"       Ext2.tests
  , testCase "Bits"       Bits.tests
  , testCase "Builders"   Builders.tests
  ]
