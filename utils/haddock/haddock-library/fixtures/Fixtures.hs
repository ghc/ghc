{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Control.Applicative ((<|>))
import Control.Exception (IOException, catch)
import Control.Monad (when)
import Data.Foldable (traverse_)
import qualified Data.List as List
import Data.Traversable (for)
import Data.TreeDiff
import Data.TreeDiff.Golden
import GHC.Generics (Generic)
import qualified Options.Applicative as O
import System.Directory (getDirectoryContents)
import System.Exit (exitFailure)
import System.FilePath
import System.IO
import Prelude

import qualified Documentation.Haddock.Parser as Parse
import Documentation.Haddock.Types

type Doc id = DocH () id

data Fixture = Fixture
  { fixtureName :: FilePath
  , fixtureOutput :: FilePath
  }
  deriving (Show)

data Result = Result
  { _resultSuccess :: !Int
  , _resultTotal :: !Int
  }
  deriving (Show)

combineResults :: Result -> Result -> Result
combineResults (Result s t) (Result s' t') = Result (s + s') (t + t')

readFixtures :: IO [Fixture]
readFixtures = do
  let dir = "fixtures/examples"
  files <- getDirectoryContents dir
  let inputs = filter (\fp -> takeExtension fp == ".input") files
  return $ flip map inputs $ \fp ->
    Fixture
      { fixtureName = dir </> fp
      , fixtureOutput = dir </> fp -<.> "parsed"
      }

goldenFixture
  :: String
  -> IO Expr
  -> IO Expr
  -> (Expr -> Expr -> IO (Maybe String))
  -> (Expr -> IO ())
  -> IO Result
goldenFixture name expect actual cmp wrt = do
  putStrLn $ "running " ++ name
  a <- actual
  e <- expect `catch` handler a
  mres <- cmp e a
  case mres of
    Nothing -> return (Result 1 1)
    Just str -> do
      putStrLn str
      return (Result 0 1)
  where
    handler :: Expr -> IOException -> IO Expr
    handler a exc = do
      putStrLn $ "Caught " ++ show exc
      putStrLn "Accepting the test"
      wrt a
      return a

runFixtures :: [Fixture] -> IO ()
runFixtures fixtures = do
  results <- for fixtures $ \(Fixture i o) -> do
    let name = takeBaseName i
    let readDoc = do
          input <- readFile i
          return (parseString input)
    ediffGolden goldenFixture name o readDoc
  case List.foldl' combineResults (Result 0 0) results of
    Result s t -> do
      putStrLn $ "Fixtures: success " ++ show s ++ "; total " ++ show t
      when (s /= t) exitFailure

listFixtures :: [Fixture] -> IO ()
listFixtures = traverse_ $ \(Fixture i _) -> do
  let name = takeBaseName i
  putStrLn name

acceptFixtures :: [Fixture] -> IO ()
acceptFixtures = traverse_ $ \(Fixture i o) -> do
  input <- readFile i
  let doc = parseString input
  let actual = show (prettyExpr $ toExpr doc) ++ "\n"
  writeFile o actual

parseString :: String -> Doc String
parseString = Parse.toRegular . _doc . Parse.parseParas Nothing

data Cmd = CmdRun | CmdAccept | CmdList

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- For interleaved output when debugging
  runCmd =<< O.execParser opts
  where
    opts = O.info (O.helper <*> cmdParser) O.fullDesc

    cmdParser :: O.Parser Cmd
    cmdParser = cmdRun <|> cmdAccept <|> cmdList <|> pure CmdRun

    cmdRun =
      O.flag' CmdRun $
        mconcat
          [ O.long "run"
          , O.help "Run parser fixtures"
          ]

    cmdAccept =
      O.flag' CmdAccept $
        mconcat
          [ O.long "accept"
          , O.help "Run & accept parser fixtures"
          ]

    cmdList =
      O.flag' CmdList $
        mconcat
          [ O.long "list"
          , O.help "List fixtures"
          ]

runCmd :: Cmd -> IO ()
runCmd CmdRun = readFixtures >>= runFixtures
runCmd CmdList = readFixtures >>= listFixtures
runCmd CmdAccept = readFixtures >>= acceptFixtures

-------------------------------------------------------------------------------
-- Orphans
-------------------------------------------------------------------------------

deriving instance Generic (DocH mod id)
instance (ToExpr mod, ToExpr id) => ToExpr (DocH mod id)

deriving instance Generic (Header id)
instance ToExpr id => ToExpr (Header id)

deriving instance Generic (Hyperlink id)
instance ToExpr id => ToExpr (Hyperlink id)

deriving instance Generic (ModLink id)
instance ToExpr id => ToExpr (ModLink id)

deriving instance Generic Picture
instance ToExpr Picture

deriving instance Generic Example
instance ToExpr Example

deriving instance Generic (Table id)
instance ToExpr id => ToExpr (Table id)

deriving instance Generic (TableRow id)
instance ToExpr id => ToExpr (TableRow id)

deriving instance Generic (TableCell id)
instance ToExpr id => ToExpr (TableCell id)
