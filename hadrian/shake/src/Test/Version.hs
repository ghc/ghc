{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}

module Test.Version(main) where

import Development.Shake
import Development.Shake.Classes
import General.GetOpt
import Data.List.Extra
import Text.Read
import Test.Type


newtype Opts = Ver Int
opts = [Option "" ["ver"] (ReqArg (fmap Ver . readEither) "INT") ""]

newtype Oracle = Oracle ()
    deriving (Show,Eq,Hashable,Binary,NFData,Typeable)
type instance RuleResult Oracle = Int

main = testBuildArgs test opts $ \opts -> do
    want ["foo.txt","ver.txt","oracle.txt"]

    "foo.txt" %> \file -> liftIO $ appendFile file "x"

    let ver = headDef 0 [x | Ver x <- opts]
    versioned ver $ "ver.txt" %> \out -> liftIO $ appendFile out $ show ver

    versioned ver $ addOracleCache $ \(Oracle ()) -> do
        liftIO $ appendFile "oracle.in" $ show ver
        pure $ ver `mod` 2
    "oracle.txt" %> \out -> do
        v <- askOracle $ Oracle ()
        liftIO $ appendFile out $ show v


test build = do
    writeFile "foo.txt" ""
    v1 <- getHashedShakeVersion ["foo.txt"]
    writeFile "foo.txt" "y"
    v2 <- getHashedShakeVersion ["foo.txt"]
    assertBool (v1 /= v2) "Hashes must not be equal"

    build ["clean"]
    build []
    assertContents "foo.txt" "x"
    build ["--rule-version=new"]
    assertContents "foo.txt" "xx"
    build ["--rule-version=new"]
    assertContents "foo.txt" "xx"
    build ["--rule-version=extra","--silent"]
    assertContents "foo.txt" "xxx"
    build ["--rule-version=more","--no-rule-version"]
    assertContents "foo.txt" "xxx"
    build ["--rule-version=more"]
    assertContents "foo.txt" "xxx"
    build ["--rule-version=final","--silent"]
    assertContents "foo.txt" "xxxx"

    build ["clean"]
    build []
    assertContents "ver.txt" "0"
    assertContents "foo.txt" "x"
    build ["--ver=0","--silent"]
    assertContents "ver.txt" "0"
    build ["--ver=8"]
    build ["--ver=9","--silent"]
    build ["--ver=9","--silent"]
    build ["--ver=3","--silent"]
    assertContents "ver.txt" "0893"
    assertContents "oracle.in" "0893"
    -- when you change version you don't do cutoff
    assertContents "oracle.txt" "0011"
    assertContents "foo.txt" "x"
