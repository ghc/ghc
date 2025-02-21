{-# LANGUAGE BangPatterns #-}

module Main where

import GHC
import GHC.Paths
import Unsafe.Coerce
import Control.Monad.IO.Class
import System.Environment (getArgs)
import Control.Monad (unless)

main :: IO ()
main = do
    [libdir] <- getArgs
    runGhc (Just libdir) run

run :: Ghc ()
run = do
    dyn_flags <- getSessionDynFlags
    _ <- setSessionDynFlags dyn_flags

    setContext [ IIDecl . simpleImportDecl . mkModuleName $ "Prelude"
               , IIDecl . simpleImportDecl . mkModuleName $ "Unsafe.Coerce" ]

    wrong

expected :: Double
expected = 5.5626902089526504e-303

wrong :: Ghc ()
wrong = do
    let chck = "5.5626902089526504e-303 :: Double"
    v <- compileExpr chck
    let !v' = unsafeCoerce v :: Double
    unless (v' == expected) $ fail "case 1 failed"

    let chck2 = "5.5626902089526504e-303 :: Rational"
    v2 <- compileExpr chck2
    let !v2' = unsafeCoerce v2 :: Rational
    unless (realToFrac v2' == expected) $ fail "case 2 failed"
