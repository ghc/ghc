module HeaderInfoTests
    ( prop_optionsIdentity
    , prop_languageParse
    , prop_languageError
    ) where

import Test.QuickCheck
import Test.QuickCheck.Batch
import Data.Char

import Control.Monad
import System.IO.Unsafe

import HeaderInfo
import StringBuffer
import SrcLoc

import Language.Haskell.Extension

newtype CmdOptions = CmdOptions {cmdOptions :: [String]}
    deriving Show

instance Arbitrary CmdOptions where
    arbitrary = resize 30 $ liftM CmdOptions arbitrary
    coarbitrary = undefined

instance Arbitrary Char where
    arbitrary = elements $ ['a'..'z']++['A'..'Z']
    coarbitrary = undefined

data Options = Options
             | Options_GHC
               deriving Show

instance Arbitrary Options where
    arbitrary = elements [Options,Options_GHC]
    coarbitrary = undefined

-- Test that OPTIONS are correctly extracted from a buffer
-- with comments and garbage.
prop_optionsIdentity lowercase options cmds
    = not (null cmds) ==>
      all (all (not.null).cmdOptions) cmds ==>
      concatMap cmdOptions cmds == map unLoc (getOptions buffer "somefile")
    where buffer = unsafePerformIO $ stringToStringBuffer str
          str = concatMap mkPragma cmds ++
                "\n @#@# garbage #@#@ \n"
          mkPragma (CmdOptions cmd)
              = unlines [ "-- Pragma: "
                        , unwords $ ["{-#", pragma]++cmd++["#-}"]
                        , "{- End of pragma -}" ]
          pragma = (if lowercase then map toLower else map toUpper) $ 
                   case options of
                     Options -> "OPTIONS"
                     Options_GHC -> "OPTIONS_GHC"

newtype Extensions = Extensions [Extension]
    deriving Show

instance Arbitrary Extensions where
    arbitrary = resize 30 $ liftM Extensions arbitrary
    coarbitrary = undefined

extensions :: [Extension]
extensions = [ OverlappingInstances
             , UndecidableInstances
             , IncoherentInstances
             , RecursiveDo
             , ParallelListComp
             , MultiParamTypeClasses
             , NoMonomorphismRestriction
             , FunctionalDependencies
             , Rank2Types
             , RankNTypes
             , PolymorphicComponents
             , ExistentialQuantification
             , ScopedTypeVariables
             , ImplicitParams
             , FlexibleContexts
             , FlexibleInstances
             , EmptyDataDecls
             , CPP
             , TypeSynonymInstances
             , TemplateHaskell
             , ForeignFunctionInterface
             , InlinePhase
             , ContextStack
             , Arrows
             , Generics
             , NoImplicitPrelude
             , NamedFieldPuns
             , PatternGuards
             , GeneralizedNewtypeDeriving
             , ExtensibleRecords
             , RestrictedTypeSynonyms
             , HereDocuments ]

-- derive Enum for Extension?
instance Arbitrary Extension where
    arbitrary = elements extensions
    coarbitrary = undefined

-- Test that we can parse all known extensions.
prop_languageParse lowercase (Extensions exts)
    = not (null exts) ==>
      not (isBottom (getOptions buffer "somefile"))
    where buffer = unsafePerformIO $ stringToStringBuffer str
          str = unlines [ "-- Pragma: "
                        , unwords $ ["{-#", pragma, ppExts exts "" , "#-}"]
                        , "{- End of pragma -}"
                        , "garbage#@$#$" ]
          ppExts [e] = shows e
          ppExts (x:xs) = shows x . showChar ',' . ppExts xs
          ppExts [] = id
          pragma = (if lowercase then map toLower else map toUpper)
                   "LANGUAGE"

-- Test that invalid extensions cause exceptions.
prop_languageError lowercase ext
    = not (null ext) ==>
      ext `notElem` map show extensions ==>
      isBottom (foldr seq () (getOptions buffer "somefile"))
    where buffer = unsafePerformIO $ stringToStringBuffer str
          str = unlines [ "-- Pragma: "
                        , unwords $ ["{-#", pragma, ext , "#-}"]
                        , "{- End of pragma -}"
                        , "garbage#@$#$" ]
          pragma = (if lowercase then map toLower else map toUpper)
                   "LANGUAGE"
