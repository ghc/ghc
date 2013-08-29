
module Main (main) where

import Control.Monad
import Data.List
import DynFlags
import Language.Haskell.Extension

main :: IO ()
main = do
    let ghcExtensions = [ ext | (ext, _, _) <- xFlags ]
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

expectedGhcOnlyExtensions :: [String]
expectedGhcOnlyExtensions = [
                             "AllowAmbiguousTypes",
                             "RelaxedLayout",
                             "AlternativeLayoutRule",
                             "AlternativeLayoutRuleTransitional",
                             "ExplicitNamespaces",
                             "TypeHoles",
                             "OverloadedLists",
                             "EmptyCase",
                             "AutoDeriveTypeable",
                             "NegativeLiterals",
                             "NumDecimals",
                             "NullaryTypeClasses"]

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
                               "Trustworthy"]

