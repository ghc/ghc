-- | A test for ensuring that GHC's supporting language extensions remains in
-- sync with Cabal's own extension list.
--
-- If you have ended up here due to a test failure, please see
-- Note [Adding a language extension] in compiler/GHC/Driver/Session.hs.

module Main (main) where

import Control.Monad
import Data.List ( (\\) )
import GHC.Driver.Session
import Language.Haskell.Extension

main :: IO ()
main = do
    let ghcExtensions = map flagSpecName xFlags
        cabalExtensions = map show [ toEnum 0 :: KnownExtension .. ]
        ghcOnlyExtensions = ghcExtensions \\ cabalExtensions
        cabalOnlyExtensions = cabalExtensions \\ ghcExtensions
    check "GHC-only flags" expectedGhcOnlyExtensions ghcOnlyExtensions
    check "Cabal-only flags" expectedCabalOnlyExtensions cabalOnlyExtensions

check :: String -> [String] -> [String] -> IO ()
check title expected got
    = do let unexpected = got \\ expected
             missing = expected \\ got
             showProblems problemType problems
                 = unless (null problems) $
                       do putStrLn (title ++ ": " ++ problemType)
                          putStrLn "-----"
                          mapM_ putStrLn problems
                          putStrLn "-----"
                          putStrLn ""
         showProblems "Unexpected flags" unexpected
         showProblems "Missing flags" missing

-- See Note [Adding a language extension] in compiler/GHC/Driver/Session.hs.
expectedGhcOnlyExtensions :: [String]
expectedGhcOnlyExtensions =
    [ "RelaxedLayout"
    , "AlternativeLayoutRule"
    , "AlternativeLayoutRuleTransitional"
    , "OverloadedRecordUpdate"
    ]

expectedCabalOnlyExtensions :: [String]
expectedCabalOnlyExtensions = ["Generics",
                               "ExtensibleRecords",
                               "RestrictedTypeSynonyms",
                               "HereDocuments",
                               "NewQualifiedOperators",
                               "XmlSyntax",
                               "RegularPatterns",
                               "SafeImports",
                               "Safe",
                               "Unsafe",
                               "Trustworthy",
                               "MonadFailDesugaring",
                               "MonoPatBinds"
                              ]
