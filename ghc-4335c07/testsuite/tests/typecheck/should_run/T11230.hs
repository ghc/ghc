{-# LANGUAGE RoleAnnotations #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}

module Main where

import Control.Exception

newtype Representational a = Representational ()
type role Representational representational

newtype Phantom a = Phantom ()
type role Phantom phantom

testRepresentational :: Representational Char -> Representational Bool
testRepresentational = id
{-# NOINLINE testRepresentational #-}

testPhantom :: Phantom Char -> Phantom Bool
testPhantom = id
{-# NOINLINE testPhantom #-}

throwsException :: String -> a -> IO ()
throwsException c v = do
  result <- try (evaluate v)
  case result of
    Right _ -> error (c ++ " (Failure): No exception!")
    Left (TypeError _) -> putStrLn (c ++ "(Success): exception found")

main = do
  throwsException "representational" testRepresentational
  throwsException "phantom" testPhantom
