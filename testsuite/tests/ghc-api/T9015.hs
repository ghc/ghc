module Main where

import GHC
import GHC.Driver.Session
import GHC.Driver.Monad
import GHC.Parser.Lexer (mkParserFlags)
import System.Environment

testStrings = [
    "import Data.Maybe"
  , "import qualified Data.Maybe"
  , "import Data.Maybe (isJust)"

  , "add a b = a+b"
  , "data Foo = Foo String"
  , "deriving instance Show Foo"
  , "{-# WARNING Foo \"Just a warning\" #-}"
  , "{-# ANN foo (Just \"Hello\") #-}"
  , "{-# RULES \"map/map\" forall f g xs. map f (map g xs) = map (f.g) xs #-}"
  , "class HasString a where\n\
    \  update :: a -> (String -> String) -> a\n\
    \  upcase :: a -> a\n\
    \  upcase x = update x (fmap toUpper)\n\
    \  content :: a -> String\n\
    \  default content :: Show a => a -> String\n\
    \  content = show"
  , "instance HasString Foo where\n\
    \  update (Foo s) f = Foo (f s)\n\
    \  content (Foo s) = s"

  , "add a b"
  , "let foo = add a b"
  , "x <- foo y"
  , "5 + 8"

  , "a <-"
  , "2 +"
  , "@#"
  ]

main = do
  [libdir] <- getArgs
  runGhc (Just libdir) $ do
    liftIO (putStrLn "Is import:")
    testWithParser isImport

    liftIO (putStrLn "Is declaration:")
    testWithParser isDecl

    liftIO (putStrLn "Is statement:")
    testWithParser isStmt

  where
    testWithParser parser = do
      dflags <- getSessionDynFlags
      let pflags = mkParserFlags dflags
      liftIO . putStrLn . unlines $ map (testExpr (parser pflags)) testStrings

    testExpr parser expr = do
      expr ++ ": " ++ show (parser expr)
