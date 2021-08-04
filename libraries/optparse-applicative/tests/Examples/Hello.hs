{-# LANGUAGE CPP #-}
module Examples.Hello where

import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad (replicateM_)

data Sample = Sample
  { hello  :: String
  , quiet  :: Bool
  , repeat :: Int }
  deriving Show

sample :: Parser Sample
sample = Sample
      <$> strOption
          ( long "hello"
         <> metavar "TARGET"
         <> help "Target for the greeting" )
      <*> switch
          ( long "quiet"
         <> short 'q'
         <> help "Whether to be quiet" )
      <*> option auto
          ( long "repeat"
         <> help "Repeats for greeting"
         <> showDefault
         <> value 1
         <> metavar "INT" )

main :: IO ()
main = greet =<< execParser opts

opts :: ParserInfo Sample
opts = info (sample <**> helper)
  ( fullDesc
  <> progDesc "Print a greeting for TARGET"
  <> header "hello - a test for optparse-applicative" )

greet :: Sample -> IO ()
greet (Sample h False n) = replicateM_ n . putStrLn $ "Hello, " ++ h
greet _ = return ()
