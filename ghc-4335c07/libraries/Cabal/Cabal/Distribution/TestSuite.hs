{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.TestSuite
-- Copyright   :  Thomas Tuegel 2010
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module defines the detailed test suite interface which makes it
-- possible to expose individual tests to Cabal or other test agents.

module Distribution.TestSuite
    ( TestInstance(..)
    , OptionDescr(..)
    , OptionType(..)
    , Test(..)
    , Options
    , Progress(..)
    , Result(..)
    , testGroup
    ) where

import Prelude ()
import Distribution.Compat.Prelude

data TestInstance = TestInstance
    { run       :: IO Progress      -- ^ Perform the test.
    , name      :: String           -- ^ A name for the test, unique within a
                                    -- test suite.
    , tags      :: [String]         -- ^ Users can select groups of tests by
                                    -- their tags.
    , options   :: [OptionDescr]    -- ^ Descriptions of the options recognized
                                    -- by this test.
    , setOption :: String -> String -> Either String TestInstance
        -- ^ Try to set the named option to the given value. Returns an error
        -- message if the option is not supported or the value could not be
        -- correctly parsed; otherwise, a 'TestInstance' with the option set to
        -- the given value is returned.
    }

data OptionDescr = OptionDescr
    { optionName        :: String
    , optionDescription :: String       -- ^ A human-readable description of the
                                        -- option to guide the user setting it.
    , optionType        :: OptionType
    , optionDefault     :: Maybe String
    }
  deriving (Eq, Read, Show)

data OptionType
    = OptionFile
        { optionFileMustExist   :: Bool
        , optionFileIsDir       :: Bool
        , optionFileExtensions  :: [String]
        }
    | OptionString
        { optionStringMultiline :: Bool
        }
    | OptionNumber
        { optionNumberIsInt     :: Bool
        , optionNumberBounds    :: (Maybe String, Maybe String)
        }
    | OptionBool
    | OptionEnum [String]
    | OptionSet [String]
    | OptionRngSeed
  deriving (Eq, Read, Show)

data Test
    = Test TestInstance
    | Group
        { groupName     :: String
        , concurrently  :: Bool
            -- ^ If true, then children of this group may be run in parallel.
            -- Note that this setting is not inherited by children. In
            -- particular, consider a group F with "concurrently = False" that
            -- has some children, including a group T with "concurrently =
            -- True". The children of group T may be run concurrently with each
            -- other, as long as none are run at the same time as any of the
            -- direct children of group F.
        , groupTests    :: [Test]
        }
    | ExtraOptions [OptionDescr] Test

type Options = [(String, String)]

data Progress = Finished Result
              | Progress String (IO Progress)

data Result = Pass
            | Fail String
            | Error String
  deriving (Eq, Read, Show)

-- | Create a named group of tests, which are assumed to be safe to run in
-- parallel.
testGroup :: String -> [Test] -> Test
testGroup n ts = Group { groupName = n, concurrently = True, groupTests = ts }
