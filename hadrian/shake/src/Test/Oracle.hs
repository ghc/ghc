{-# LANGUAGE TypeFamilies, ConstraintKinds, ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable, TypeOperators, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Oracle(main) where

import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import General.GetOpt
import Data.List.Extra
import Data.Tuple.Extra
import Test.Type hiding (RandomType)
import qualified Test.Type as T
import Control.Monad
import General.Extra


-- These are instances we'll compute over
type instance RuleResult String = String
type instance RuleResult Int = String
type instance RuleResult () = String

type instance RuleResult Bool = Bool -- test results don't have to be a boolean

newtype FileLen = FileLen FilePath
    deriving (Eq,Show,NFData,Typeable,Hashable,Binary)
type instance RuleResult FileLen = Int

newtype RandomType = RandomType (BinarySentinel String)
    deriving (Eq,Show,NFData,Typeable,Hashable,Binary)

type instance RuleResult RandomType = Int
type instance RuleResult T.RandomType = Int

data Define = Define String String -- this type produces this result
opt = [Option "" ["def"] (ReqArg (Right . uncurry Define . second tailErr . breakOn "=") "type=value") ""]

main = testBuildArgs test opt $ \args -> do
    addOracle $ \(T.RandomType _) -> pure 42
    addOracle $ \(RandomType _) -> pure (-42)
    "randomtype.txt" %> \out -> do
        a <- askOracle $ T.RandomType $ BinarySentinel ()
        b <- askOracle $ RandomType $ BinarySentinel ()
        writeFile' out $ show (a,b)

    addOracle $ \b -> pure $ not b
    "true.txt" %> \out -> writeFile' out . show =<< askOracle False

    let add :: forall a . (ShakeValue a, RuleResult a ~ String) => String -> a -> Rules ()
        add name key = do
            name <.> "txt" %> \out -> do
                liftIO $ appendFile ".log" "."
                writeFile' out =<< askOracle key
            forM_ [val | Define nam val <- args, nam == name] $ \val ->
                addOracle $ \(_ :: a) -> pure val
    add "string" ""
    add "unit" ()
    add "int" (0 :: Int)

    addOracleCache $ \(FileLen file) -> do
        liftIO $ appendFile ".log" "#"
        length <$> readFile' file
    "cache_out.txt" %> \out -> do
        o <- askOracle (FileLen "cache_in.txt")
        liftIO $ appendFile ".log" "!"
        writeFile' out $ show o

test build = do
    build ["clean"]

    build ["randomtype.txt"]
    assertContents "randomtype.txt" "(42,-42)"

    -- check it rebuilds when it should
    writeFile ".log" ""
    build ["--def=string=name","string.txt"]
    assertContents "string.txt" "name"
    build ["--def=string=name","string.txt"]
    assertContents "string.txt" "name"
    build ["--def=string=test","string.txt"]
    assertContents "string.txt" "test"
    assertContents ".log" ".."

    -- check adding/removing redundant oracles does not trigger a rebuild
    build ["--def=string=test","string.txt","--def=unit=bob"]
    build ["--def=string=test","string.txt","--def=int=fred"]
    build ["--def=string=test","string.txt"]
    assertContents "string.txt" "test"
    assertContents ".log" ".."

    writeFile ".log" ""
    writeFile "cache_in.txt" "aaa"
    build ["cache_out.txt","--sleep"]
    assertContents "cache_out.txt" "3"
    assertContents ".log" "#!"
    writeFile "cache_in.txt" "zzz"
    build ["cache_out.txt","--sleep"]
    build ["cache_out.txt","--sleep"]
    assertContents "cache_out.txt" "3"
    assertContents ".log" "#!#"
    writeFile "cache_in.txt" "zzzz"
    build ["cache_out.txt","--sleep"]
    assertContents "cache_out.txt" "4"
    assertContents ".log" "#!##!"

    -- check error messages are good
    let errors args err = assertExceptionAfter (replace "\\" "/") err $ build $ "--quiet" : args

    build ["--def=unit=test","unit.txt"]
    errors ["unit.txt"] -- Building with an an Oracle that has been removed
        ["missing a call to addOracle"]

    errors ["int.txt"] -- Building with an Oracle that I know nothing about
        ["missing a call to addOracle"]

    errors ["--def=string=1","--def=string=1"] -- Two Oracles defined in one go
        ["oracle defined twice", "Test/Oracle.hs"]
