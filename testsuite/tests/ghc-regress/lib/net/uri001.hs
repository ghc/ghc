--------------------------------------------------------------------------------
--  $Id: uri001.hs,v 1.3 2004/10/20 11:34:53 simonmar Exp $
--
--  Copyright (c) 2004, G. KLYNE.  All rights reserved.
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  URITest
--  Copyright   :  (c) 2004, Graham Klyne
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Graham Klyne
--  Stability   :  provisional
--  Portability :  H98
--
--  This Module contains test cases for module URI.
--
--  Using GHC, I compile with this command line:
--  ghc --make -fglasgow-exts
--      -i..\;C:\Dev\Haskell\Lib\HUnit;C:\Dev\Haskell\Lib\Parsec
--      -o URITest.exe URITest
--  The -i line may need changing for alternative installations.
--
--------------------------------------------------------------------------------

-- module Network.URITest where -- Use this when GHC -is-main bug is fixed
module Main where

import Network.URI
    ( URI(..), URIAuth(..)
    , parseURI
    , isUri, isUriReference, isRelativeUri, isAbsoluteUri
    , isIPv6address, isIPv4address
    , relativeTo
    , relativeFrom
    , normalizeCase, normalizeEscape, normalizePathSegments
    )

import Test.HUnit

import IO ( Handle, openFile, IOMode(WriteMode), hClose, hPutStr, hPutStrLn )

-- Test supplied string for valid URI reference syntax
--   isValidURIRef :: String -> Bool
-- Test supplied string for valid absolute URI reference syntax
--   isAbsoluteURIRef :: String -> Bool
-- Test supplied string for valid absolute URI syntax
--   isAbsoluteURI :: String -> Bool

data URIType = AbsId    -- URI form (absolute, no fragment)
             | AbsRf    -- Absolute URI reference
             | RelRf    -- Relative URI reference
             | InvRf    -- Invalid URI reference
isValidT :: URIType -> Bool
isValidT InvRf = False
isValidT _     = True

isAbsRfT :: URIType -> Bool
isAbsRfT AbsId = True
isAbsRfT AbsRf = True
isAbsRfT _     = False

isRelRfT :: URIType -> Bool
isRelRfT RelRf = True
isRelRfT _     = False

isAbsIdT :: URIType -> Bool
isAbsIdT AbsId = True
isAbsIdT _     = False

testEq :: (Eq a, Show a) => String -> a -> a -> Test
testEq lab a1 a2 = TestCase ( assertEqual lab a1 a2 )

testURIRef :: URIType -> String -> Test
testURIRef t u = TestList
  [ testEq ("test_isURIReference:"++u) (isValidT t) (isUriReference u)
  , testEq ("test_isRelativeURI:"++u)  (isRelRfT t) (isRelativeUri  u)
  , testEq ("test_isAbsoluteURI:"++u)  (isAbsIdT t) (isAbsoluteUri  u)
  ]

testURIRefComponents :: String -> (Maybe URI) -> String -> Test
testURIRefComponents lab uv us =
    testEq ("testURIRefComponents:"++us) uv (parseURI us)


testURIRef001 = testURIRef AbsRf "http://example.org/aaa/bbb#ccc"
testURIRef002 = testURIRef AbsId "mailto:local@domain.org"
testURIRef003 = testURIRef AbsRf "mailto:local@domain.org#frag"
testURIRef004 = testURIRef AbsRf "HTTP://EXAMPLE.ORG/AAA/BBB#CCC"
testURIRef005 = testURIRef RelRf "//example.org/aaa/bbb#ccc"
testURIRef006 = testURIRef RelRf "/aaa/bbb#ccc"
testURIRef007 = testURIRef RelRf "bbb#ccc"
testURIRef008 = testURIRef RelRf "#ccc"
testURIRef009 = testURIRef RelRf "#"
testURIRef010 = testURIRef RelRf "/"
-- escapes
testURIRef011 = testURIRef AbsRf "http://example.org/aaa%2fbbb#ccc"
testURIRef012 = testURIRef AbsRf "http://example.org/aaa%2Fbbb#ccc"
testURIRef013 = testURIRef RelRf "%2F"
testURIRef014 = testURIRef RelRf "aaa%2Fbbb"
-- ports
testURIRef015 = testURIRef AbsRf "http://example.org:80/aaa/bbb#ccc"
testURIRef016 = testURIRef AbsRf "http://example.org:/aaa/bbb#ccc"
testURIRef017 = testURIRef AbsRf "http://example.org./aaa/bbb#ccc"
testURIRef018 = testURIRef AbsRf "http://example.123./aaa/bbb#ccc"
-- bare authority
testURIRef019 = testURIRef AbsId "http://example.org"
-- IPv6 literals (from RFC2732):
testURIRef021 = testURIRef AbsId "http://[FEDC:BA98:7654:3210:FEDC:BA98:7654:3210]:80/index.html"
testURIRef022 = testURIRef AbsId "http://[1080:0:0:0:8:800:200C:417A]/index.html"
testURIRef023 = testURIRef AbsId "http://[3ffe:2a00:100:7031::1]"
testURIRef024 = testURIRef AbsId "http://[1080::8:800:200C:417A]/foo"
testURIRef025 = testURIRef AbsId "http://[::192.9.5.5]/ipng"
testURIRef026 = testURIRef AbsId "http://[::FFFF:129.144.52.38]:80/index.html"
testURIRef027 = testURIRef AbsId "http://[2010:836B:4179::836B:4179]"
testURIRef028 = testURIRef RelRf "//[2010:836B:4179::836B:4179]"
testURIRef029 = testURIRef InvRf "[2010:836B:4179::836B:4179]"
-- RFC2396 test cases
testURIRef031 = testURIRef RelRf "./aaa"
testURIRef032 = testURIRef RelRf "../aaa"
testURIRef033 = testURIRef AbsId "g:h"
testURIRef034 = testURIRef RelRf "g"
testURIRef035 = testURIRef RelRf "./g"
testURIRef036 = testURIRef RelRf "g/"
testURIRef037 = testURIRef RelRf "/g"
testURIRef038 = testURIRef RelRf "//g"
testURIRef039 = testURIRef RelRf "?y"
testURIRef040 = testURIRef RelRf "g?y"
testURIRef041 = testURIRef RelRf "#s"
testURIRef042 = testURIRef RelRf "g#s"
testURIRef043 = testURIRef RelRf "g?y#s"
testURIRef044 = testURIRef RelRf ";x"
testURIRef045 = testURIRef RelRf "g;x"
testURIRef046 = testURIRef RelRf "g;x?y#s"
testURIRef047 = testURIRef RelRf "."
testURIRef048 = testURIRef RelRf "./"
testURIRef049 = testURIRef RelRf ".."
testURIRef050 = testURIRef RelRf "../"
testURIRef051 = testURIRef RelRf "../g"
testURIRef052 = testURIRef RelRf "../.."
testURIRef053 = testURIRef RelRf "../../"
testURIRef054 = testURIRef RelRf "../../g"
testURIRef055 = testURIRef RelRf "../../../g"
testURIRef056 = testURIRef RelRf "../../../../g"
testURIRef057 = testURIRef RelRf "/./g"
testURIRef058 = testURIRef RelRf "/../g"
testURIRef059 = testURIRef RelRf "g."
testURIRef060 = testURIRef RelRf ".g"
testURIRef061 = testURIRef RelRf "g.."
testURIRef062 = testURIRef RelRf "..g"
testURIRef063 = testURIRef RelRf "./../g"
testURIRef064 = testURIRef RelRf "./g/."
testURIRef065 = testURIRef RelRf "g/./h"
testURIRef066 = testURIRef RelRf "g/../h"
testURIRef067 = testURIRef RelRf "g;x=1/./y"
testURIRef068 = testURIRef RelRf "g;x=1/../y"
testURIRef069 = testURIRef RelRf "g?y/./x"
testURIRef070 = testURIRef RelRf "g?y/../x"
testURIRef071 = testURIRef RelRf "g#s/./x"
testURIRef072 = testURIRef RelRf "g#s/../x"
testURIRef073 = testURIRef RelRf ""
testURIRef074 = testURIRef RelRf "A'C"
testURIRef075 = testURIRef RelRf "A$C"
testURIRef076 = testURIRef RelRf "A@C"
testURIRef077 = testURIRef RelRf "A,C"
-- Invalid
testURIRef080 = testURIRef InvRf "http://foo.org:80Path/More"
testURIRef081 = testURIRef InvRf "::"
testURIRef082 = testURIRef InvRf " "
testURIRef083 = testURIRef InvRf "%"
testURIRef084 = testURIRef InvRf "A%Z"
testURIRef085 = testURIRef InvRf "%ZZ"
testURIRef086 = testURIRef InvRf "%AZ"
testURIRef087 = testURIRef InvRf "A C"
-- testURIRef088 = -- (case removed)
-- testURIRef089 = -- (case removed)
testURIRef090 = testURIRef InvRf "A\"C"
testURIRef091 = testURIRef InvRf "A`C"
testURIRef092 = testURIRef InvRf "A<C"
testURIRef093 = testURIRef InvRf "A>C"
testURIRef094 = testURIRef InvRf "A^C"
testURIRef095 = testURIRef InvRf "A\\C"
testURIRef096 = testURIRef InvRf "A{C"
testURIRef097 = testURIRef InvRf "A|C"
testURIRef098 = testURIRef InvRf "A}C"
-- From RFC2396:
-- rel_segment   = 1*( unreserved | escaped |
--                     ";" | "@" | "&" | "=" | "+" | "$" | "," )
-- unreserved    = alphanum | mark
-- mark          = "-" | "_" | "." | "!" | "~" | "*" | "'" |
--                 "(" | ")"
-- Note RFC 2732 allows '[', ']' ONLY for reserved purpose of IPv6 literals,
-- or does it?
testURIRef101 = testURIRef InvRf "A[C"
testURIRef102 = testURIRef InvRf "A]C"
testURIRef103 = testURIRef InvRf "A[**]C"
testURIRef104 = testURIRef InvRf "http://[xyz]/"
testURIRef105 = testURIRef InvRf "http://]/"
testURIRef106 = testURIRef InvRf "http://example.org/[2010:836B:4179::836B:4179]"
testURIRef107 = testURIRef InvRf "http://example.org/abc#[2010:836B:4179::836B:4179]"
testURIRef108 = testURIRef InvRf "http://example.org/xxx/[qwerty]#a[b]"
-- Random other things that crop up (valid URIs)
testURIRef111 = testURIRef AbsRf "http://example/Andr&#567;"
testURIRef112 = testURIRef AbsId "file:///C:/DEV/Haskell/lib/HXmlToolbox-3.01/examples/"
testURIRef113 = testURIRef AbsId "http://46229EFFE16A9BD60B9F1BE88B2DB047ADDED785/demo.mp3"
testURIRef114 = testURIRef InvRf "http://example.org/xxx/qwerty#a#b"

testURIRefSuite = TestLabel "Test URIrefs" testURIRefList
testURIRefList = TestList
  [
    testURIRef001, testURIRef002, testURIRef003, testURIRef004,
    testURIRef005, testURIRef006, testURIRef007, testURIRef008,
    testURIRef009, testURIRef010,
    --
    testURIRef011, testURIRef012, testURIRef013, testURIRef014,
    testURIRef015, testURIRef016, testURIRef017, testURIRef018,
    --
    testURIRef019,
    --
    testURIRef021, testURIRef022, testURIRef023, testURIRef024,
    testURIRef025, testURIRef026, testURIRef027, testURIRef028,
    testURIRef029,
    --
    testURIRef031, testURIRef032, testURIRef033, testURIRef034,
    testURIRef035, testURIRef036, testURIRef037, testURIRef038,
    testURIRef039,
    testURIRef040, testURIRef041, testURIRef042, testURIRef043,
    testURIRef044, testURIRef045, testURIRef046, testURIRef047,
    testURIRef048, testURIRef049,
    testURIRef050, testURIRef051, testURIRef052, testURIRef053,
    testURIRef054, testURIRef055, testURIRef056, testURIRef057,
    testURIRef058, testURIRef059,
    testURIRef060, testURIRef061, testURIRef062, testURIRef063,
    testURIRef064, testURIRef065, testURIRef066, testURIRef067,
    testURIRef068, testURIRef069,
    testURIRef070, testURIRef071, testURIRef072, testURIRef073,
    testURIRef074, testURIRef075, testURIRef076, testURIRef077,
    --
    testURIRef080,
    testURIRef081, testURIRef082, testURIRef083, testURIRef084,
    testURIRef085, testURIRef086, testURIRef087, -- testURIRef088,
    -- testURIRef089,
    testURIRef090, testURIRef091, testURIRef092, testURIRef093,
    testURIRef094, testURIRef095, testURIRef096, testURIRef097,
    testURIRef098, -- testURIRef099,
    --
    testURIRef101, testURIRef102, testURIRef103, testURIRef104,
    testURIRef105, testURIRef106, testURIRef107, testURIRef108,
    --
    testURIRef111, testURIRef112, testURIRef113, testURIRef114
  ]

-- test decomposition of URI into components
testComponent01 = testURIRefComponents "testComponent01"
        ( Just $ URI
            { uriScheme    = "http:"
            , uriAuthority = Just (URIAuth "user:pass@" "example.org" ":99")
            , uriPath      = "/aaa/bbb"
            , uriQuery     = "?qqq"
            , uriFragment  = "#fff"
            } )
        "http://user:pass@example.org:99/aaa/bbb?qqq#fff"
testComponent02 = testURIRefComponents "testComponent02"
        ( const Nothing
        ( Just $ URI
            { uriScheme    = "http:"
            , uriAuthority = Just (URIAuth "user:pass@" "example.org" ":99")
            , uriPath      = "aaa/bbb"
            , uriQuery     = ""
            , uriFragment  = ""
            } )
        )
        "http://user:pass@example.org:99aaa/bbb"
testComponent03 = testURIRefComponents "testComponent03"
        ( Just $ URI
            { uriScheme    = "http:"
            , uriAuthority = Just (URIAuth "user:pass@" "example.org" ":99")
            , uriPath      = ""
            , uriQuery     = "?aaa/bbb"
            , uriFragment  = ""
            } )
        "http://user:pass@example.org:99?aaa/bbb"
testComponent04 = testURIRefComponents "testComponent03"
        ( Just $ URI
            { uriScheme    = "http:"
            , uriAuthority = Just (URIAuth "user:pass@" "example.org" ":99")
            , uriPath      = ""
            , uriQuery     = ""
            , uriFragment  = "#aaa/bbb"
            } )
        "http://user:pass@example.org:99#aaa/bbb"

testComponentSuite = TestLabel "Test URIrefs" $ TestList
  [ testComponent01
  , testComponent02
  , testComponent03
  , testComponent04
  ]

-- Get reference relative to given base
--   relativeRef :: String -> String -> String
--
-- Get absolute URI given base and relative reference
--   absoluteURI :: String -> String -> String
--
-- Test cases taken from: http://www.w3.org/2000/10/swap/uripath.py
-- (Thanks, Dan Connolly)
--
-- NOTE:  absoluteURI base (relativeRef base u) is always equivalent to u.
-- cf. http://lists.w3.org/Archives/Public/uri/2003Jan/0008.html

testRelSplit :: String -> String -> String -> String -> Test
testRelSplit label base uabs urel =
    testEq label urel (mkrel puabs pubas)
    where
        mkrel (Just u1) (Just u2) = show (u1 `relativeFrom` u2)
        mkrel Nothing   _         = "Invalid URI: "++urel
        mkrel _         Nothing   = "Invalid URI: "++uabs
        puabs = parseURI uabs
        pubas = parseURI base

testRelJoin  :: String -> String -> String -> String -> Test
testRelJoin label base urel uabs =
    testEq label uabs (mkabs purel pubas)
    where
        mkabs (Just u1) (Just u2) = shabs (u1 `relativeTo` u2)
        mkabs Nothing   _         = "Invalid URI: "++urel
        mkabs _         Nothing   = "Invalid URI: "++uabs
        shabs (Just u) = show u
        shabs Nothing  = "No result"
        purel = parseURI urel
        pubas = parseURI base

testRelative :: String -> String -> String -> String -> Test
testRelative label base uabs urel = TestList
    [
    (testRelSplit (label++"(rel)") base uabs urel),
    (testRelJoin  (label++"(abs)") base urel uabs)
    ]

testRelative01 = testRelative "testRelative01"
                    "foo:xyz" "bar:abc" "bar:abc"
testRelative02 = testRelative "testRelative02"
                    "http://example/x/y/z" "http://example/x/abc" "../abc"
testRelative03 = testRelative "testRelative03"
                    "http://example2/x/y/z" "http://example/x/abc" "//example/x/abc"
                    -- "http://example2/x/y/z" "http://example/x/abc" "http://example/x/abc"
testRelative04 = testRelative "testRelative04"
                    "http://ex/x/y/z" "http://ex/x/r" "../r"
testRelative05 = testRelative "testRelative05"
                    "http://ex/x/y/z" "http://ex/r" "/r"
                    -- "http://ex/x/y/z" "http://ex/r" "../../r"
testRelative06 = testRelative "testRelative06"
                    "http://ex/x/y/z" "http://ex/x/y/q/r" "q/r"
testRelative07 = testRelative "testRelative07"
                    "http://ex/x/y" "http://ex/x/q/r#s" "q/r#s"
testRelative08 = testRelative "testRelative08"
                    "http://ex/x/y" "http://ex/x/q/r#s/t" "q/r#s/t"
testRelative09 = testRelative "testRelative09"
                    "http://ex/x/y" "ftp://ex/x/q/r" "ftp://ex/x/q/r"
testRelative10 = testRelative "testRelative10"
                    -- "http://ex/x/y" "http://ex/x/y" "y"
                    "http://ex/x/y" "http://ex/x/y" ""
testRelative11 = testRelative "testRelative11"
                    -- "http://ex/x/y/" "http://ex/x/y/" "./"
                    "http://ex/x/y/" "http://ex/x/y/" ""
testRelative12 = testRelative "testRelative12"
                    -- "http://ex/x/y/pdq" "http://ex/x/y/pdq" "pdq"
                    "http://ex/x/y/pdq" "http://ex/x/y/pdq" ""
testRelative13 = testRelative "testRelative13"
                    "http://ex/x/y/" "http://ex/x/y/z/" "z/"
testRelative14 = testRelative "testRelative14"
                    -- "file:/swap/test/animal.rdf" "file:/swap/test/animal.rdf#Animal" "animal.rdf#Animal"
                    "file:/swap/test/animal.rdf" "file:/swap/test/animal.rdf#Animal" "#Animal"
testRelative15 = testRelative "testRelative15"
                    "file:/e/x/y/z" "file:/e/x/abc" "../abc"
testRelative16 = testRelative "testRelative16"
                    "file:/example2/x/y/z" "file:/example/x/abc" "/example/x/abc"
testRelative17 = testRelative "testRelative17"
                    "file:/ex/x/y/z" "file:/ex/x/r" "../r"
testRelative18 = testRelative "testRelative18"
                    "file:/ex/x/y/z" "file:/r" "/r"
testRelative19 = testRelative "testRelative19"
                    "file:/ex/x/y" "file:/ex/x/q/r" "q/r"
testRelative20 = testRelative "testRelative20"
                    "file:/ex/x/y" "file:/ex/x/q/r#s" "q/r#s"
testRelative21 = testRelative "testRelative21"
                    "file:/ex/x/y" "file:/ex/x/q/r#" "q/r#"
testRelative22 = testRelative "testRelative22"
                    "file:/ex/x/y" "file:/ex/x/q/r#s/t" "q/r#s/t"
testRelative23 = testRelative "testRelative23"
                    "file:/ex/x/y" "ftp://ex/x/q/r" "ftp://ex/x/q/r"
testRelative24 = testRelative "testRelative24"
                    -- "file:/ex/x/y" "file:/ex/x/y" "y"
                    "file:/ex/x/y" "file:/ex/x/y" ""
testRelative25 = testRelative "testRelative25"
                    -- "file:/ex/x/y/" "file:/ex/x/y/" "./"
                    "file:/ex/x/y/" "file:/ex/x/y/" ""
testRelative26 = testRelative "testRelative26"
                    -- "file:/ex/x/y/pdq" "file:/ex/x/y/pdq" "pdq"
                    "file:/ex/x/y/pdq" "file:/ex/x/y/pdq" ""
testRelative27 = testRelative "testRelative27"
                    "file:/ex/x/y/" "file:/ex/x/y/z/" "z/"
testRelative28 = testRelative "testRelative28"
                    "file:/devel/WWW/2000/10/swap/test/reluri-1.n3"
                    "file://meetings.example.com/cal#m1" "//meetings.example.com/cal#m1"
                    -- "file:/devel/WWW/2000/10/swap/test/reluri-1.n3"
                    -- "file://meetings.example.com/cal#m1" "file://meetings.example.com/cal#m1"
testRelative29 = testRelative "testRelative29"
                    "file:/home/connolly/w3ccvs/WWW/2000/10/swap/test/reluri-1.n3"
                    "file://meetings.example.com/cal#m1" "//meetings.example.com/cal#m1"
                    -- "file:/home/connolly/w3ccvs/WWW/2000/10/swap/test/reluri-1.n3"
                    -- "file://meetings.example.com/cal#m1" "file://meetings.example.com/cal#m1"
testRelative30 = testRelative "testRelative30"
                    "file:/some/dir/foo" "file:/some/dir/#blort" "./#blort"
testRelative31 = testRelative "testRelative31"
                    "file:/some/dir/foo" "file:/some/dir/#" "./#"
testRelative32 = testRelative "testRelative32"
                    "http://ex/x/y" "http://ex/x/q:r" "./q:r"
                    -- see RFC2396bis, section 5       ^^
testRelative33 = testRelative "testRelative33"
                    "http://ex/x/y" "http://ex/x/p=q:r" "./p=q:r"
                    -- "http://ex/x/y" "http://ex/x/p=q:r" "p=q:r"
testRelative34 = testRelative "testRelative34"
                    "http://ex/x/y?pp/qq" "http://ex/x/y?pp/rr" "?pp/rr"
testRelative35 = testRelative "testRelative35"
                    "http://ex/x/y?pp/qq" "http://ex/x/y/z" "y/z"
testRelative36 = testRelative "testRelative36"
                    "mailto:local"
                    "mailto:local/qual@domain.org#frag"
                    "local/qual@domain.org#frag"
testRelative37 = testRelative "testRelative37"
                    "mailto:local/qual1@domain1.org"
                    "mailto:local/more/qual2@domain2.org#frag"
                    "more/qual2@domain2.org#frag"
testRelative38 = testRelative "testRelative38"
                    "http://ex/x/z?q" "http://ex/x/y?q" "y?q"
testRelative39 = testRelative "testRelative39"
                    "http://ex?p" "http://ex/x/y?q" "/x/y?q"
testRelative40 = testRelative "testRelative40"
                    "foo:a/b" "foo:a/c/d" "c/d"
testRelative41 = testRelative "testRelative41"
                    "foo:a/b" "foo:/c/d" "/c/d"
testRelative42 = testRelative "testRelative42"
                    "foo:a/b?c#d" "foo:a/b?c" ""
testRelative43 = testRelative "testRelative42"
                    "foo:a" "foo:b/c" "b/c"
testRelative44 = testRelative "testRelative44"
                    "foo:/a/y/z" "foo:/a/b/c" "../b/c"
testRelative45 = testRelJoin "testRelative45"
                    "foo:a" "./b/c" "foo:b/c"
testRelative46 = testRelJoin "testRelative46"
                    "foo:a" "/./b/c" "foo:/b/c"
testRelative47 = testRelJoin "testRelative47"
                    "foo://a//b/c" "../../d" "foo://a/d"
testRelative48 = testRelJoin "testRelative48"
                    "foo:a" "." "foo:"
testRelative49 = testRelJoin "testRelative49"
                    "foo:a" ".." "foo:"

-- add escape tests
testRelative50 = testRelative "testRelative50"
                    "http://example/x/y%2Fz" "http://example/x/abc" "abc"
testRelative51 = testRelative "testRelative51"
                    "http://example/a/x/y/z" "http://example/a/x%2Fabc" "../../x%2Fabc"
testRelative52 = testRelative "testRelative52"
                    "http://example/a/x/y%2Fz" "http://example/a/x%2Fabc" "../x%2Fabc"
testRelative53 = testRelative "testRelative53"
                    "http://example/x%2Fy/z" "http://example/x%2Fy/abc" "abc"
testRelative54 = testRelative "testRelative54"
                    "http://ex/x/y" "http://ex/x/q%3Ar" "q%3Ar"
testRelative55 = testRelative "testRelative55"
                    "http://example/x/y%2Fz" "http://example/x%2Fabc" "/x%2Fabc"
-- Apparently, TimBL prefers the following way to 41, 42 above
-- cf. http://lists.w3.org/Archives/Public/uri/2003Feb/0028.html
-- He also notes that there may be different relative fuctions
-- that satisfy the basic equivalence axiom:
-- cf. http://lists.w3.org/Archives/Public/uri/2003Jan/0008.html
testRelative56 = testRelative "testRelative56"
                    "http://example/x/y/z" "http://example/x%2Fabc" "/x%2Fabc"
testRelative57 = testRelative "testRelative57"
                    "http://example/x/y%2Fz" "http://example/x%2Fabc" "/x%2Fabc"

-- Other oddball tests
    -- Check segment normalization code:
testRelative60 = testRelJoin "testRelative60"
                    "ftp://example/x/y" "http://example/a/b/../../c" "http://example/c"
testRelative61 = testRelJoin "testRelative61"
                    "ftp://example/x/y" "http://example/a/b/c/../../" "http://example/a/"
testRelative62 = testRelJoin "testRelative62"
                    "ftp://example/x/y" "http://example/a/b/c/./" "http://example/a/b/c/"
testRelative63 = testRelJoin "testRelative63"
                    "ftp://example/x/y" "http://example/a/b/c/.././" "http://example/a/b/"
testRelative64 = testRelJoin "testRelative64"
                    "ftp://example/x/y" "http://example/a/b/c/d/../../../../e" "http://example/e"
testRelative65 = testRelJoin "testRelative65"
                    "ftp://example/x/y" "http://example/a/b/c/d/../.././../../e" "http://example/e"
    -- Check handling of queries and fragments with non-relative paths
testRelative70 = testRelative "testRelative70"
                    "mailto:local1@domain1?query1" "mailto:local2@domain2"
                    "local2@domain2"
testRelative71 = testRelative "testRelative71"
                    "mailto:local1@domain1" "mailto:local2@domain2?query2"
                    "local2@domain2?query2"
testRelative72 = testRelative "testRelative72"
                    "mailto:local1@domain1?query1" "mailto:local2@domain2?query2"
                    "local2@domain2?query2"
testRelative73 = testRelative "testRelative73"
                    "mailto:local@domain?query1" "mailto:local@domain?query2"
                    "?query2"
testRelative74 = testRelative "testRelative74"
                    "mailto:?query1" "mailto:local@domain?query2"
                    "local@domain?query2"
testRelative75 = testRelative "testRelative75"
                    "mailto:local@domain?query1" "mailto:local@domain?query2"
                    "?query2"
testRelative76 = testRelative "testRelative76"
                    "foo:bar" "http://example/a/b?c/../d"  "http://example/a/b?c/../d"
testRelative77 = testRelative "testRelative77"
                    "foo:bar" "http://example/a/b#c/../d"  "http://example/a/b#c/../d"


-- testRelative  base abs rel
-- testRelSplit  base abs rel
-- testRelJoin   base rel abs
testRelative81 = testRelSplit "testRelative81"
                    "http://example.org/base/uri" "http:this"
                    "this"
testRelative82 = testRelJoin "testRelative82"
                    "http://example.org/base/uri" "http:this"
                    "http:this"
testRelative83 = testRelJoin "testRelative83"
                    "http:base" "http:this"
                    "http:this"
testRelative84 = testRelJoin "testRelative84"
                    "f:/a" ".//g"
                    "f://g"
testRelative85 = testRelJoin "testRelative85"
                    "f://example.org/base/a" "b/c//d/e"
                    "f://example.org/base/b/c//d/e"
testRelative86 = testRelJoin "testRelative86"
                    "mid:m@example.ord/c@example.org" "m2@example.ord/c2@example.org"
                    "mid:m@example.ord/m2@example.ord/c2@example.org"
testRelative87 = testRelJoin "testRelative87"
                    "file:///C:/DEV/Haskell/lib/HXmlToolbox-3.01/examples/" "mini1.xml"
                    "file:///C:/DEV/Haskell/lib/HXmlToolbox-3.01/examples/mini1.xml"
testRelative88 = testRelative "testRelative88"
                    "foo:a/y/z" "foo:a/b/c" "../b/c"


testRelativeSuite = TestLabel "Test Relative URIs" testRelativeList
testRelativeList  = TestList
  [ testRelative01, testRelative02, testRelative03, testRelative04
  , testRelative05, testRelative06, testRelative07, testRelative08
  , testRelative09
  , testRelative10, testRelative11, testRelative12, testRelative13
  , testRelative14, testRelative15, testRelative16, testRelative17
  , testRelative18, testRelative19
  , testRelative20, testRelative21, testRelative22, testRelative23
  , testRelative24, testRelative25, testRelative26, testRelative27
  , testRelative28, testRelative29
  , testRelative30, testRelative31, testRelative32, testRelative33
  , testRelative34, testRelative35, testRelative36, testRelative37
  , testRelative38, testRelative39
  , testRelative40, testRelative41, testRelative42, testRelative43
  , testRelative44, testRelative45, testRelative46, testRelative47
  , testRelative48, testRelative49
    --
  , testRelative50, testRelative51, testRelative52, testRelative53
  , testRelative54, testRelative55, testRelative56, testRelative57
    --
  , testRelative60, testRelative61, testRelative62, testRelative63
  , testRelative64, testRelative65
    --
  , testRelative70, testRelative71, testRelative72, testRelative73
  , testRelative74, testRelative75, testRelative76, testRelative77
    --
  -- , testRelative80
  , testRelative81, testRelative82, testRelative83
  , testRelative84, testRelative85, testRelative86
  , testRelative87, testRelative88
  ]

-- RFC2396 relative-to-absolute URI tests

rfcbase  = "http://a/b/c/d;p?q"
-- normal cases, RFC2396bis 5.4.1
testRFC01 = testRelJoin "testRFC01" rfcbase "g:h" "g:h"
testRFC02 = testRelJoin "testRFC02" rfcbase "g" "http://a/b/c/g"
testRFC03 = testRelJoin "testRFC03" rfcbase "./g" "http://a/b/c/g"
testRFC04 = testRelJoin "testRFC04" rfcbase "g/" "http://a/b/c/g/"
testRFC05 = testRelJoin "testRFC05" rfcbase "/g" "http://a/g"
testRFC06 = testRelJoin "testRFC06" rfcbase "//g" "http://g"
testRFC07 = testRelJoin "testRFC07" rfcbase "?y" "http://a/b/c/d;p?y"
testRFC08 = testRelJoin "testRFC08" rfcbase "g?y" "http://a/b/c/g?y"
testRFC09 = testRelJoin "testRFC09" rfcbase "?q#s" "http://a/b/c/d;p?q#s"
testRFC23 = testRelJoin "testRFC10" rfcbase "#s" "http://a/b/c/d;p?q#s"
testRFC10 = testRelJoin "testRFC11" rfcbase "g#s" "http://a/b/c/g#s"
testRFC11 = testRelJoin "testRFC12" rfcbase "g?y#s" "http://a/b/c/g?y#s"
testRFC12 = testRelJoin "testRFC13" rfcbase ";x" "http://a/b/c/;x"
testRFC13 = testRelJoin "testRFC14" rfcbase "g;x" "http://a/b/c/g;x"
testRFC14 = testRelJoin "testRFC15" rfcbase "g;x?y#s" "http://a/b/c/g;x?y#s"
testRFC24 = testRelJoin "testRFC16" rfcbase "" "http://a/b/c/d;p?q"
testRFC15 = testRelJoin "testRFC17" rfcbase "." "http://a/b/c/"
testRFC16 = testRelJoin "testRFC18" rfcbase "./" "http://a/b/c/"
testRFC17 = testRelJoin "testRFC19" rfcbase ".." "http://a/b/"
testRFC18 = testRelJoin "testRFC20" rfcbase "../" "http://a/b/"
testRFC19 = testRelJoin "testRFC21" rfcbase "../g" "http://a/b/g"
testRFC20 = testRelJoin "testRFC22" rfcbase "../.." "http://a/"
testRFC21 = testRelJoin "testRFC23" rfcbase "../../" "http://a/"
testRFC22 = testRelJoin "testRFC24" rfcbase "../../g" "http://a/g"
-- abnormal cases, RFC2396bis 5.4.2
testRFC31 = testRelJoin "testRFC31" rfcbase "?q" rfcbase
testRFC32 = testRelJoin "testRFC32" rfcbase "../../../g" "http://a/g"
testRFC33 = testRelJoin "testRFC33" rfcbase "../../../../g" "http://a/g"
testRFC34 = testRelJoin "testRFC34" rfcbase "/./g" "http://a/g"
testRFC35 = testRelJoin "testRFC35" rfcbase "/../g" "http://a/g"
testRFC36 = testRelJoin "testRFC36" rfcbase "g." "http://a/b/c/g."
testRFC37 = testRelJoin "testRFC37" rfcbase ".g" "http://a/b/c/.g"
testRFC38 = testRelJoin "testRFC38" rfcbase "g.." "http://a/b/c/g.."
testRFC39 = testRelJoin "testRFC39" rfcbase "..g" "http://a/b/c/..g"
testRFC40 = testRelJoin "testRFC40" rfcbase "./../g" "http://a/b/g"
testRFC41 = testRelJoin "testRFC41" rfcbase "./g/." "http://a/b/c/g/"
testRFC42 = testRelJoin "testRFC42" rfcbase "g/./h" "http://a/b/c/g/h"
testRFC43 = testRelJoin "testRFC43" rfcbase "g/../h" "http://a/b/c/h"
testRFC44 = testRelJoin "testRFC44" rfcbase "g;x=1/./y" "http://a/b/c/g;x=1/y"
testRFC45 = testRelJoin "testRFC45" rfcbase "g;x=1/../y" "http://a/b/c/y"
testRFC46 = testRelJoin "testRFC46" rfcbase "g?y/./x" "http://a/b/c/g?y/./x"
testRFC47 = testRelJoin "testRFC47" rfcbase "g?y/../x" "http://a/b/c/g?y/../x"
testRFC48 = testRelJoin "testRFC48" rfcbase "g#s/./x" "http://a/b/c/g#s/./x"
testRFC49 = testRelJoin "testRFC49" rfcbase "g#s/../x" "http://a/b/c/g#s/../x"
testRFC50 = testRelJoin "testRFC50" rfcbase "http:x" "http:x"

-- Null path tests
-- See RFC2396bis, section 5.2,
-- "If the base URI's path component is the empty string, then a single
--  slash character is copied to the buffer"
testRFC60 = testRelative "testRFC60" "http://ex"     "http://ex/x/y?q" "/x/y?q"
testRFC61 = testRelJoin  "testRFC61" "http://ex"     "x/y?q"           "http://ex/x/y?q"
testRFC62 = testRelative "testRFC62" "http://ex?p"   "http://ex/x/y?q" "/x/y?q"
testRFC63 = testRelJoin  "testRFC63" "http://ex?p"   "x/y?q"           "http://ex/x/y?q"
testRFC64 = testRelative "testRFC64" "http://ex#f"   "http://ex/x/y?q" "/x/y?q"
testRFC65 = testRelJoin  "testRFC65" "http://ex#f"   "x/y?q"           "http://ex/x/y?q"
testRFC66 = testRelative "testRFC66" "http://ex?p"   "http://ex/x/y#g" "/x/y#g"
testRFC67 = testRelJoin  "testRFC67" "http://ex?p"   "x/y#g"           "http://ex/x/y#g"
testRFC68 = testRelative "testRFC68" "http://ex"     "http://ex/"      "/"
testRFC69 = testRelJoin  "testRFC69" "http://ex"     "./"              "http://ex/"
testRFC70 = testRelative "testRFC70" "http://ex"     "http://ex/a/b"   "/a/b"
testRFC71 = testRelative "testRFC71" "http://ex/a/b" "http://ex"       "./"

testRFC2396Suite = TestLabel "Test RFC2396 examples" testRFC2396List
testRFC2396List  = TestList
  [
    testRFC01, testRFC02, testRFC03, testRFC04,
    testRFC05, testRFC06, testRFC07, testRFC08,
    testRFC09,
    testRFC10, testRFC11, testRFC12, testRFC13,
    testRFC14, testRFC15, testRFC16, testRFC17,
    testRFC18, testRFC19,
    testRFC20, testRFC21, testRFC22, testRFC23,
    testRFC24,
    -- testRFC30,
    testRFC31, testRFC32, testRFC33,
    testRFC34, testRFC35, testRFC36, testRFC37,
    testRFC38, testRFC39,
    testRFC40, testRFC41, testRFC42, testRFC43,
    testRFC44, testRFC45, testRFC46, testRFC47,
    testRFC48, testRFC49,
    testRFC50,
    --
    testRFC60, testRFC61, testRFC62, testRFC63,
    testRFC64, testRFC65, testRFC66, testRFC67,
    testRFC68, testRFC69,
    testRFC70
  ]

-- And some other oddballs:
mailbase = "mailto:local/option@domain.org?notaquery#frag"
testMail01 = testRelJoin "testMail01"
            mailbase "more@domain"
            "mailto:local/more@domain"
testMail02 = testRelJoin "testMail02"
            mailbase "#newfrag"
            "mailto:local/option@domain.org?notaquery#newfrag"
testMail03 = testRelJoin "testMail03"
            mailbase "l1/q1@domain"
            "mailto:local/l1/q1@domain"

testMail11 = testRelJoin "testMail11"
             "mailto:local1@domain1?query1" "mailto:local2@domain2"
             "mailto:local2@domain2"
testMail12 = testRelJoin "testMail12"
             "mailto:local1@domain1" "mailto:local2@domain2?query2"
             "mailto:local2@domain2?query2"
testMail13 = testRelJoin "testMail13"
             "mailto:local1@domain1?query1" "mailto:local2@domain2?query2"
             "mailto:local2@domain2?query2"
testMail14 = testRelJoin "testMail14"
             "mailto:local@domain?query1" "mailto:local@domain?query2"
             "mailto:local@domain?query2"
testMail15 = testRelJoin "testMail15"
             "mailto:?query1" "mailto:local@domain?query2"
             "mailto:local@domain?query2"
testMail16 = testRelJoin "testMail16"
             "mailto:local@domain?query1" "?query2"
             "mailto:local@domain?query2"
testInfo17 = testRelJoin "testInfo17"
             "info:name/1234/../567" "name/9876/../543"
             "info:name/name/543"
testInfo18 = testRelJoin "testInfo18"
             "info:/name/1234/../567" "name/9876/../543"
             "info:/name/name/543"

testOddballSuite = TestLabel "Test oddball examples" testOddballList
testOddballList  = TestList
  [ testMail01, testMail02, testMail03
  , testMail11, testMail12, testMail13, testMail14, testMail15, testMail16
  , testInfo17
  ]

--  Normalization tests

--  Case normalization; cf. RFC2396bis section 6.2.2.1
--  NOTE:  authority case normalization is not performed
testNormalize01 = testEq "testNormalize01"
                  "http://EXAMPLE.com/Root/%2A?%2B#%2C"
                  (normalizeCase "HTTP://EXAMPLE.com/Root/%2a?%2b#%2c")

--  Encoding normalization; cf. RFC2396bis section 6.2.2.2
testNormalize11 = testEq "testNormalize11"
                  "HTTP://EXAMPLE.com/Root/~Me/"
                  (normalizeEscape "HTTP://EXAMPLE.com/Root/%7eMe/")
testNormalize12 = testEq "testNormalize12"
                  "foo:%40AZ%5b%60az%7b%2f09%3a-._~"
                  (normalizeEscape "foo:%40%41%5a%5b%60%61%7a%7b%2f%30%39%3a%2d%2e%5f%7e")
testNormalize13 = testEq "testNormalize13"
                  "foo:%3a%2f%3f%23%5b%5d%40"
                  (normalizeEscape "foo:%3a%2f%3f%23%5b%5d%40")

--  Path segment normalization; cf. RFC2396bis section 6.2.2.4
testNormalize21 = testEq "testNormalize21"
                    "http://example/c"
                    (normalizePathSegments "http://example/a/b/../../c")
testNormalize22 = testEq "testNormalize22"
                    "http://example/a/"
                    (normalizePathSegments "http://example/a/b/c/../../")
testNormalize23 = testEq "testNormalize23"
                    "http://example/a/b/c/"
                    (normalizePathSegments "http://example/a/b/c/./")
testNormalize24 = testEq "testNormalize24"
                    "http://example/a/b/"
                    (normalizePathSegments "http://example/a/b/c/.././")
testNormalize25 = testEq "testNormalize25"
                    "http://example/e"
                    (normalizePathSegments "http://example/a/b/c/d/../../../../e")
testNormalize26 = testEq "testNormalize26"
                    "http://example/e"
                    (normalizePathSegments "http://example/a/b/c/d/../.././../../e")
testNormalize27 = testEq "testNormalize27"
                    "http://example/e"
                    (normalizePathSegments "http://example/a/b/../.././../../e")
testNormalize28 = testEq "testNormalize28"
                    "foo:e"
                    (normalizePathSegments "foo:a/b/../.././../../e")

testNormalizeSuite = TestList
  [ testNormalize01
  , testNormalize11
  , testNormalize12
  , testNormalize13
  , testNormalize21, testNormalize22, testNormalize23, testNormalize24
  , testNormalize25, testNormalize26, testNormalize27, testNormalize28
  ]

-- Full test suite
allTests = TestList
  [ testURIRefSuite
  , testComponentSuite
  , testRelativeSuite
  , testRFC2396Suite
  , testOddballSuite
  , testNormalizeSuite
  ]

main = runTestTT allTests

runTestFile t = do
    h <- openFile "a.tmp" WriteMode
    runTestText (putTextToHandle h False) t
    hClose h
tf = runTestFile
tt = runTestTT

uref = testURIRefSuite
tr01 = testRelative01
tr02 = testRelative02
tr03 = testRelative03
tr04 = testRelative04
rel  = testRelativeSuite
rfc  = testRFC2396Suite
oddb = testOddballSuite

(Just bu02) = parseURI "http://example/x/y/z"
(Just ou02) = parseURI "../abc"
(Just ru02) = parseURI "http://example/x/abc"
-- fileuri = testURIReference "file:///C:/DEV/Haskell/lib/HXmlToolbox-3.01/examples/"

cu02 = ou02 `relativeTo` bu02

--------------------------------------------------------------------------------
--
--  Copyright (c) 2004, G. KLYNE.  All rights reserved.
--  Distributed as free software under the following license.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions
--  are met:
--
--  - Redistributions of source code must retain the above copyright notice,
--  this list of conditions and the following disclaimer.
--
--  - Redistributions in binary form must reproduce the above copyright
--  notice, this list of conditions and the following disclaimer in the
--  documentation and/or other materials provided with the distribution.
--
--  - Neither name of the copyright holders nor the names of its
--  contributors may be used to endorse or promote products derived from
--  this software without specific prior written permission.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND THE CONTRIBUTORS
--  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
--  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
--  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
--  HOLDERS OR THE CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
--  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
--  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
--  OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
--  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
--  TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
--  USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--
--------------------------------------------------------------------------------
-- $Source: /srv/cvs/cvs.haskell.org/fptools/testsuite/tests/ghc-regress/lib/net/uri001.hs,v $
-- $Author: simonmar $
-- $Revision: 1.3 $
-- $Log: uri001.hs,v $
-- Revision 1.3  2004/10/20 11:34:53  simonmar
-- Update Network.URI test for new library.
--
-- Revision 1.1  2004/10/14 16:11:30  gklyne
-- Add URI unit test to cvs.haskell.org repository
--
-- Revision 1.17  2004/10/14 11:51:09  graham
-- Confirm that URITest runs with GHC.
-- Fix up some comments and other minor details.
--
-- Revision 1.16  2004/10/14 11:45:30  graham
-- Use moduke name main for GHC 6.2
--
-- Revision 1.15  2004/08/11 11:07:39  graham
-- Add new test case.
--
-- Revision 1.14  2004/06/30 11:35:27  graham
-- Update URI code to use hierarchical libraries for Parsec and Network.
--
-- Revision 1.13  2004/06/22 16:19:16  graham
-- New URI test case added.
--
-- Revision 1.12  2004/04/21 15:13:29  graham
-- Add test case
--
-- Revision 1.11  2004/04/21 14:54:05  graham
-- Fix up some tests
--
-- Revision 1.10  2004/04/20 14:54:13  graham
-- Fix up test cases related to port number in authority,
-- and add some more URI decomposition tests.
--
-- Revision 1.9  2004/04/07 15:06:17  graham
-- Add extra test case
-- Revise syntax in line with changes to RFC2396bis
--
-- Revision 1.8  2004/03/17 14:34:58  graham
-- Add Network.HTTP files to CVS
--
-- Revision 1.7  2004/03/16 14:19:38  graham
-- Change licence to BSD style;  add nullURI definition; new test cases.
--
-- Revision 1.6  2004/02/20 12:12:00  graham
-- Add URI normalization functions
--
-- Revision 1.5  2004/02/19 23:19:35  graham
-- Network.URI module passes all test cases
--
-- Revision 1.4  2004/02/17 20:06:02  graham
-- Revised URI parser to reflect latest RFC2396bis (-04)
--
-- Revision 1.3  2004/02/11 14:32:14  graham
-- Added work-in-progress notes.
--
-- Revision 1.2  2004/02/02 14:00:39  graham
-- Fix optional host name in URI.  Add test cases.
--
-- Revision 1.1  2004/01/27 21:13:45  graham
-- New URI module and test suite added,
-- implementing the GHC Network.URI interface.
--
