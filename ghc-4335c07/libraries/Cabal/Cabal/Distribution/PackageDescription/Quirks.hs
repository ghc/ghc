{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
--
-- @since 2.2.0.0
module Distribution.PackageDescription.Quirks (patchQuirks) where

import           Prelude ()
import           Distribution.Compat.Prelude
import           GHC.Fingerprint (Fingerprint (..), fingerprintData)
import           Foreign.Ptr (castPtr)
import           System.IO.Unsafe (unsafeDupablePerformIO)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Map as Map

-- | Patch legacy @.cabal@ file contents to allow parsec parser to accept
-- all of Hackage.
--
-- Bool part of the result tells whether the output is modified.
--
-- @since 2.2.0.0
patchQuirks :: BS.ByteString -> (Bool, BS.ByteString)
patchQuirks bs = case Map.lookup (BS.take 256 bs, md5 bs) patches of
    Nothing -> (False, bs)
    Just (post, f)
        | post /= md5 output -> (False, bs)
        | otherwise          -> (True, output)
      where
        output = f bs

md5 :: BS.ByteString -> Fingerprint
md5 bs = unsafeDupablePerformIO $ BS.unsafeUseAsCStringLen bs $ \(ptr, len) ->
    fingerprintData (castPtr ptr) len

-- | 'patches' contains first 256 bytes, pre- and post-fingerprints and a patch function.
--
--
patches :: Map.Map (BS.ByteString, Fingerprint) (Fingerprint, BS.ByteString -> BS.ByteString)
patches = Map.fromList
    -- http://hackage.haskell.org/package/unicode-transforms-0.3.3
    -- other-modules: .
    -- ReadP assumed dot is empty line
    [ mk "-- This file has been generated from package.yaml by hpack version 0.17.0.\n--\n-- see: https://github.com/sol/hpack\n\nname:                unicode-transforms\nversion:             0.3.3\nsynopsis:            Unicode normalization\ndescription:         Fast Unic"
         (Fingerprint 15958160436627155571 10318709190730872881)
         (Fingerprint 11008465475756725834 13815629925116264363)
         (bsRemove "  other-modules:\n      .\n") -- TODO: remove traling \n to test structural-diff
    -- http://hackage.haskell.org/package/DSTM-0.1.2
    -- http://hackage.haskell.org/package/DSTM-0.1.1
    -- http://hackage.haskell.org/package/DSTM-0.1
    -- Other Modules: no dash
    -- ReadP parsed as section
    , mk "Name: DSTM\nVersion: 0.1.2\nCopyright: (c) 2010, Frank Kupke\nLicense: LGPL\nLicense-File: LICENSE\nAuthor: Frank Kupke\nMaintainer: frk@informatik.uni-kiel.de\nCabal-Version: >= 1.2.3\nStability: provisional\nSynopsis: A framework for using STM within distributed "
         (Fingerprint 6919263071548559054 9050746360708965827)
         (Fingerprint 17015177514298962556 11943164891661867280)
         (bsReplace "Other modules:" "-- ")
    , mk "Name: DSTM\nVersion: 0.1.1\nCopyright: (c) 2010, Frank Kupke\nLicense: LGPL\nLicense-File: LICENSE\nAuthor: Frank Kupke\nMaintainer: frk@informatik.uni-kiel.de\nCabal-Version: >= 1.2.3\nStability: provisional\nSynopsis: A framework for using STM within distributed "
         (Fingerprint 17313105789069667153 9610429408495338584)
         (Fingerprint 17250946493484671738 17629939328766863497)
         (bsReplace "Other modules:" "-- ")
    , mk "Name: DSTM\nVersion: 0.1\nCopyright: (c) 2010, Frank Kupke\nLicense: LGPL\nLicense-File: LICENSE\nAuthor: Frank Kupke\nMaintainer: frk@informatik.uni-kiel.de\nCabal-Version: >= 1.2.3\nStability: provisional\nSynopsis: A framework for using STM within distributed sy"
         (Fingerprint 10502599650530614586 16424112934471063115)
         (Fingerprint 13562014713536696107 17899511905611879358)
         (bsReplace "Other modules:" "-- ")
    -- http://hackage.haskell.org/package/control-monad-exception-mtl-0.10.3
    , mk "name: control-monad-exception-mtl\nversion: 0.10.3\nCabal-Version:  >= 1.10\nbuild-type: Simple\nlicense: PublicDomain\nauthor: Pepe Iborra\nmaintainer: pepeiborra@gmail.com\nhomepage: http://pepeiborra.github.com/control-monad-exception\nsynopsis: MTL instances f"
         (Fingerprint 18274748422558568404 4043538769550834851)
         (Fingerprint 11395257416101232635 4303318131190196308)
         (bsReplace " default- extensions:" "unknown-section")
    -- http://hackage.haskell.org/package/vacuum-opengl-0.0
    -- \DEL character
    , mk "Name:                vacuum-opengl\nVersion:             0.0\nSynopsis:            Visualize live Haskell data structures using vacuum, graphviz and OpenGL.\nDescription:         \DELVisualize live Haskell data structures using vacuum, graphviz and OpenGL.\n     "
         (Fingerprint 5946760521961682577 16933361639326309422)
         (Fingerprint 14034745101467101555 14024175957788447824)
         (bsRemove "\DEL")
    , mk "Name:                vacuum-opengl\nVersion:             0.0.1\nSynopsis:            Visualize live Haskell data structures using vacuum, graphviz and OpenGL.\nDescription:         \DELVisualize live Haskell data structures using vacuum, graphviz and OpenGL.\n   "
         (Fingerprint 10790950110330119503 1309560249972452700)
         (Fingerprint 1565743557025952928 13645502325715033593)
         (bsRemove "\DEL")
    -- http://hackage.haskell.org/package/ixset-1.0.4
    -- {- comments -}
    , mk "Name:                ixset\nVersion:             1.0.4\nSynopsis:            Efficient relational queries on Haskell sets.\nDescription:\n    Create and query sets that are indexed by multiple indices.\nLicense:             BSD3\nLicense-file:        COPYING\nAut"
         (Fingerprint 11886092342440414185 4150518943472101551)
         (Fingerprint 5731367240051983879 17473925006273577821)
         (bsRemoveStarting "{-")
    -- : after section
    -- http://hackage.haskell.org/package/ds-kanren
    , mk "name:                ds-kanren\nversion:             0.2.0.0\nsynopsis:            A subset of the miniKanren language\ndescription:\n  ds-kanren is an implementation of the <http://minikanren.org miniKanren> language.\n  .\n  == What's in ds-kanren?\n  .\n  ['dis"
         (Fingerprint 2804006762382336875 9677726932108735838)
         (Fingerprint 9830506174094917897 12812107316777006473)
         (bsReplace "Test-Suite test-unify:" "Test-Suite \"test-unify:\"" . bsReplace "Test-Suite test-list-ops:" "Test-Suite \"test-list-ops:\"")
    , mk "name:                ds-kanren\nversion:             0.2.0.1\nsynopsis:            A subset of the miniKanren language\ndescription:\n  ds-kanren is an implementation of the <http://minikanren.org miniKanren> language.\n\nlicense:             MIT\nlicense-file:  "
         (Fingerprint 9130259649220396193 2155671144384738932)
         (Fingerprint 1847988234352024240 4597789823227580457)
         (bsReplace "Test-Suite test-unify:" "Test-Suite \"test-unify:\"" . bsReplace "Test-Suite test-list-ops:" "Test-Suite \"test-list-ops:\"")
    , mk "name:                metric\nversion:             0.1.4\nsynopsis:            Metric spaces.\nlicense:             MIT\nlicense-file:        LICENSE\nauthor:              Vikram Verma\nmaintainer:          me@vikramverma.com\ncategory:            Data\nbuild-type:"
         (Fingerprint 6150019278861565482 3066802658031228162)
         (Fingerprint 9124826020564520548 15629704249829132420)
         (bsReplace "test-suite metric-tests:" "test-suite \"metric-tests:\"")
    , mk "name:                metric\nversion:             0.2.0\nsynopsis:            Metric spaces.\nlicense:             MIT\nlicense-file:        LICENSE\nauthor:              Vikram Verma\nmaintainer:          me@vikramverma.com\ncategory:            Data\nbuild-type:"
         (Fingerprint 4639805967994715694 7859317050376284551)
         (Fingerprint 5566222290622325231 873197212916959151)
         (bsReplace "test-suite metric-tests:" "test-suite \"metric-tests:\"")
    , mk "name:          phasechange\ncategory:      Data\nversion:       0.1\nauthor:        G\195\161bor Lehel\nmaintainer:    G\195\161bor Lehel <illissius@gmail.com>\nhomepage:      http://github.com/glehel/phasechange\ncopyright:     Copyright (C) 2012 G\195\161bor Lehel\nlicense:     "
         (Fingerprint 10546509771395401582 245508422312751943)
         (Fingerprint 5169853482576003304 7247091607933993833)
         (bsReplace "impl(ghc >= 7.4):" "erroneous-section" . bsReplace "impl(ghc >= 7.6):" "erroneous-section")
    , mk "Name:                smartword\nSynopsis:            Web based flash card for Word Smart I and II vocabularies\nVersion:             0.0.0.5\nHomepage:            http://kyagrd.dyndns.org/~kyagrd/project/smartword/\nCategory:            Web,Education\nLicense: "
         (Fingerprint 7803544783533485151 10807347873998191750)
         (Fingerprint 1665635316718752601 16212378357991151549)
         (bsReplace "build depends:" "--")
    , mk "name:           shelltestrunner\n-- sync with README.md, ANNOUNCE:\nversion:        1.3\ncategory:       Testing\nsynopsis:       A tool for testing command-line programs.\ndescription:\n shelltestrunner is a cross-platform tool for testing command-line\n program"
         (Fingerprint 4403237110790078829 15392625961066653722)
         (Fingerprint 10218887328390239431 4644205837817510221)
         (bsReplace "other modules:" "--")
    -- &&!
    -- http://hackage.haskell.org/package/hblas-0.3.0.0
    , mk "-- Initial hblas.cabal generated by cabal init.  For further \n-- documentation, see http://haskell.org/cabal/users-guide/\n\n-- The name of the package.\nname:                hblas\n\n-- The package version.  See the Haskell package versioning policy (PVP) \n-- "
         (Fingerprint 8570120150072467041 18315524331351505945)
         (Fingerprint 10838007242302656005 16026440017674974175)
         (bsReplace "&&!" "&& !")
    , mk "-- Initial hblas.cabal generated by cabal init.  For further \n-- documentation, see http://haskell.org/cabal/users-guide/\n\n-- The name of the package.\nname:                hblas\n\n-- The package version.  See the Haskell package versioning policy (PVP) \n-- "
         (Fingerprint 5262875856214215155 10846626274067555320)
         (Fingerprint 3022954285783401045 13395975869915955260)
         (bsReplace "&&!" "&& !")
    , mk "-- Initial hblas.cabal generated by cabal init.  For further \n-- documentation, see http://haskell.org/cabal/users-guide/\n\n-- The name of the package.\nname:                hblas\n\n-- The package version.  See the Haskell package versioning policy (PVP) \n-- "
         (Fingerprint 54222628930951453 5526514916844166577)
         (Fingerprint 1749630806887010665 8607076506606977549)
         (bsReplace "&&!" "&& !")
    , mk "-- Initial hblas.cabal generated by cabal init.  For further\n-- documentation, see http://haskell.org/cabal/users-guide/\n\n-- The name of the package.\nname:                hblas\n\n-- The package version.  See the Haskell package versioning policy (PVP)\n-- fo"
         (Fingerprint 6817250511240350300 15278852712000783849)
         (Fingerprint 15757717081429529536 15542551865099640223)
         (bsReplace "&&!" "&& !")
    , mk "-- Initial hblas.cabal generated by cabal init.  For further\n-- documentation, see http://haskell.org/cabal/users-guide/\n\n-- The name of the package.\nname:                hblas\n\n-- The package version.  See the Haskell package versioning policy (PVP)\n-- fo"
         (Fingerprint 8310050400349211976 201317952074418615)
         (Fingerprint 10283381191257209624 4231947623042413334)
         (bsReplace "&&!" "&& !")
    , mk "-- Initial hblas.cabal generated by cabal init.  For further\n-- documentation, see http://haskell.org/cabal/users-guide/\n\n-- The name of the package.\nname:                hblas\n\n-- The package version.  See the Haskell package versioning policy (PVP)\n-- fo"
         (Fingerprint 7010988292906098371 11591884496857936132)
         (Fingerprint 6158672440010710301 6419743768695725095)
         (bsReplace "&&!" "&& !")
    , mk "-- Initial hblas.cabal generated by cabal init.  For further\r\n-- documentation, see http://haskell.org/cabal/users-guide/\r\n\r\n-- The name of the package.\r\nname:                hblas\r\n\r\n-- The package version.  See the Haskell package versioning policy (PVP)"
         (Fingerprint 2076850805659055833 16615160726215879467)
         (Fingerprint 10634706281258477722 5285812379517916984)
         (bsReplace "&&!" "&& !")
    , mk "-- Initial hblas.cabal generated by cabal init.  For further\r\n-- documentation, see http://haskell.org/cabal/users-guide/\r\n\r\n-- The name of the package.\r\nname:                hblas\r\n\r\n-- The package version.  See the Haskell package versioning policy (PVP)"
         (Fingerprint 11850020631622781099 11956481969231030830)
         (Fingerprint 13702868780337762025 13383526367149067158)
         (bsReplace "&&!" "&& !")
    , mk "-- Initial hblas.cabal generated by cabal init.  For further\n-- documentation, see http://haskell.org/cabal/users-guide/\n\n-- The name of the package.\nname:                hblas\n\n-- The package version.  See the Haskell package versioning policy (PVP)\n-- fo"
         (Fingerprint 13690322768477779172 19704059263540994)
         (Fingerprint 11189374824645442376 8363528115442591078)
         (bsReplace "&&!" "&& !")
    ]
  where
    mk a b c d = ((a, b), (c, d))

-- | Helper to create entries in patches
_makePatchKey :: FilePath -> (BS.ByteString -> BS.ByteString) -> NoCallStackIO ()
_makePatchKey fp transform = do
    contents <- BS.readFile fp
    let output = transform contents
    let Fingerprint hi lo = md5 contents
    let Fingerprint hi' lo' = md5 output
    putStrLn
        $ showString "    , mk "
        . shows (BS.take 256 contents)
        . showString "\n         (Fingerprint "
        . shows hi
        . showString " "
        . shows lo
        . showString ")\n         (Fingerprint "
        . shows hi'
        . showString " "
        . shows lo'
        . showString ")"
        $ ""

-------------------------------------------------------------------------------
-- Patch helpers
-------------------------------------------------------------------------------

bsRemove
    :: BS.ByteString  -- ^ needle
    -> BS.ByteString -> BS.ByteString
bsRemove needle haystack = case BS.breakSubstring needle haystack of
    (h, t) -> BS.append h (BS.drop (BS.length needle) t)

bsReplace
    :: BS.ByteString -- ^ needle
    -> BS.ByteString -- ^ replacement
    -> BS.ByteString -> BS.ByteString
bsReplace needle repl haystack = case BS.breakSubstring needle haystack of
    (h, t)
        | not (BS.null t) -> BS.append h (BS.append repl (BS.drop (BS.length needle) t))
        | otherwise -> haystack

bsRemoveStarting
    :: BS.ByteString  -- ^ needle
    -> BS.ByteString -> BS.ByteString
bsRemoveStarting needle haystack = case BS.breakSubstring needle haystack of
    (h, _) -> h
