module Main where

import GHC
import Unsafe.Coerce
import Control.Monad.IO.Class
import System.Environment (getArgs)

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

wrong :: Ghc ()
wrong = do
    let chck = "5.5626902089526504e-303 :: Double"
    v <- compileExpr chck

    liftIO $ do
        putStr "Direct: "
        print (5.5626902089526504e-303 :: Double)
        putStr "API:    "
        print (unsafeCoerce v :: Double)

    let chck2 = "5.56269020895265e-303 :: Double"
    v2 <- compileExpr chck2

    liftIO $ do
        putStr "Direct: "
        print (5.56269020895265e-303 :: Double)
        putStr "API:    "
        print (unsafeCoerce v2 :: Double)
