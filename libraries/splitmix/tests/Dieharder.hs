{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Prelude ()
import Prelude.Compat

import Control.Concurrent.QSem
import Control.DeepSeq         (force)
import Control.Monad           (when)
import Data.Bits               (shiftL, (.|.))
import Data.Char               (isSpace)
import Data.List               (isInfixOf, unfoldr)
import Data.Maybe              (fromMaybe)
import Data.Word               (Word64)
import Foreign.C               (Errno (..), ePIPE)
import Foreign.Ptr             (castPtr)
import GHC.IO.Exception        (IOErrorType (..), IOException (..))
import System.Environment      (getArgs)
import System.IO               (Handle, hGetContents, stdout)
import Text.Printf             (printf)

import qualified Control.Concurrent.Async     as A
import qualified Control.Exception            as E
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Unsafe       as BS (unsafePackCStringLen)
import qualified Data.Vector.Storable.Mutable as MSV
import qualified System.Process               as Proc
import qualified System.Random.SplitMix       as SM
import qualified System.Random.SplitMix32     as SM32
import qualified System.Random.TF             as TF
import qualified System.Random.TF.Gen         as TF
import qualified System.Random.TF.Init        as TF

main :: IO ()
main = do
    args <- getArgs
    if null args
    then return ()
    else do
        (cmd, runs, conc, seed, test, raw, _help) <- parseArgsIO args $ (,,,,,,)
            <$> arg
            <*> optDef "-n" 1
            <*> optDef "-j" 1
            <*> opt "-s"
            <*> opt "-d"
            <*> flag "-r"
            <*> flag "-h"

        let run :: RunType g
            run | raw       = runRaw
                | otherwise = runManaged

        case cmd of
              "splitmix"      -> do
                  g <- maybe SM.initSMGen (return . SM.mkSMGen) seed
                  run test runs conc SM.splitSMGen SM.nextWord64 g
              "splitmix32"      -> do
                  g <- maybe SM32.initSMGen (return . SM32.mkSMGen) (fmap fromIntegral seed)
                  run test runs conc SM32.splitSMGen SM32.nextWord64 g
              "tfrandom"      -> do
                  g <- TF.initTFGen
                  run test runs conc TF.split tfNext64 g
              _               -> return ()

tfNext64 :: TF.TFGen -> (Word64, TF.TFGen)
tfNext64 g =
    let (w, g')   = TF.next g
        (w', g'') = TF.next g'
    in (fromIntegral w `shiftL` 32 .|. fromIntegral w', g'')

-------------------------------------------------------------------------------
-- Dieharder
-------------------------------------------------------------------------------

type RunType g =
       Maybe Int
    -> Int
    -> Int
    -> (g -> (g, g))
    -> (g -> (Word64, g))
    -> g
    -> IO () 

runRaw :: RunType g
runRaw _test _runs _conc split word gen =
    generate word split gen stdout

runManaged :: RunType g
runManaged test runs conc split word gen = do
    qsem <- newQSem conc

    rs <- A.forConcurrently (take runs $ unfoldr (Just . split) gen) $ \g ->
        E.bracket_ (waitQSem qsem) (signalQSem qsem) $
            dieharder test (generate word split g)

    case mconcat rs of
        Result p w f -> do
            let total = fromIntegral (p + w + f) :: Double
            printf "PASSED %4d %6.02f%%\n" p (fromIntegral p / total * 100)
            printf "WEAK   %4d %6.02f%%\n" w (fromIntegral w / total * 100)
            printf "FAILED %4d %6.02f%%\n" f (fromIntegral f / total * 100)
{-# INLINE runManaged #-}

dieharder :: Maybe Int -> (Handle -> IO ()) -> IO Result
dieharder test gen = do
    let proc = Proc.proc "dieharder" $ ["-g", "200"] ++ maybe ["-a"] (\t -> ["-d", show t]) test
    (Just hin, Just hout, _, ph) <- Proc.createProcess proc
        { Proc.std_in  = Proc.CreatePipe
        , Proc.std_out = Proc.CreatePipe
        }

    out <- hGetContents hout
    waitOut <- A.async $ E.evaluate $ force out

    E.catch (gen hin) $ \e -> case e of
        IOError { ioe_type = ResourceVanished , ioe_errno = Just ioe }
            | Errno ioe == ePIPE -> return ()
        _ -> E.throwIO e

    res <- A.wait waitOut
    _ <- Proc.waitForProcess ph

    return $ parseOutput res
{-# INLINE dieharder #-}

parseOutput :: String -> Result
parseOutput = foldMap parseLine . lines where
    parseLine l
        | any (`isInfixOf` l) doNotUse = mempty
        | "PASSED" `isInfixOf` l = Result 1 0 0
        | "WEAK"   `isInfixOf` l = Result 0 1 0
        | "FAILED" `isInfixOf` l = Result 0 1 0
        | otherwise = mempty

    doNotUse = ["diehard_opso", "diehard_oqso", "diehard_dna", "diehard_weak"]

-------------------------------------------------------------------------------
-- Results
-------------------------------------------------------------------------------

data Result = Result
    { _passed :: Int
    , _weak   :: Int
    , _failed :: Int
    }
  deriving Show

instance Semigroup Result where
    Result p w f <> Result p' w' f' = Result (p + p') (w +  w') (f + f')

instance Monoid Result where
    mempty = Result 0 0 0
    mappend = (<>)

-------------------------------------------------------------------------------
-- Writer
-------------------------------------------------------------------------------

size :: Int
size = 512

generate
    :: forall g. (g -> (Word64, g))
    -> (g -> (g, g))
    -> g -> Handle -> IO ()
generate word split gen0 h = do
    vec <- MSV.new size
    go gen0 vec
  where
    go :: g -> MSV.IOVector Word64 -> IO ()
    go gen vec = do
        let (g1, g2) = split gen
        write g1 vec 0
        MSV.unsafeWith vec $ \ptr -> do
            bs <- BS.unsafePackCStringLen (castPtr ptr, size * 8)
            BS.hPutStr h bs
        go g2 vec

    write :: g -> MSV.IOVector Word64 -> Int -> IO ()
    write !gen !vec !i = do
        let (w64, gen') = word gen
        MSV.unsafeWrite vec i w64
        when (i < size) $
            write gen' vec (i + 1)
{-# INLINE generate #-}

-------------------------------------------------------------------------------
-- readMaybe
-------------------------------------------------------------------------------

readEither :: Read a => String -> Either String a
readEither s =
  case [ x | (x,rest) <- reads s, all isSpace rest ] of
    [x] -> Right x
    []  -> Left "Prelude.read: no parse"
    _   -> Left "Prelude.read: ambiguous parse"

readMaybe :: Read a => String -> Maybe a
readMaybe s = case readEither s of
                Left _  -> Nothing
                Right a -> Just a
-------------------------------------------------------------------------------
-- Do it yourself command line parsing
-------------------------------------------------------------------------------

-- | 'Parser' is not an 'Alternative', only a *commutative* 'Applicative'.
--
-- Useful for quick cli parsers, like parametrising tests.
data Parser a where
    Pure :: a -> Parser a
    Ap :: Arg b -> Parser (b -> a) -> Parser a

instance Functor Parser where
    fmap f (Pure a) = Pure (f a)
    fmap f (Ap x y) = Ap x (fmap (f .) y)

instance  Applicative Parser where
    pure = Pure

    Pure f <*> z = fmap f z
    Ap x y <*> z = Ap x (flip <$> y <*> z)

data Arg a where
    Flag :: String -> Arg Bool
    Opt  :: String -> (String -> Maybe a) -> Arg (Maybe a)
    Arg  :: Arg String

arg :: Parser String
arg = Ap Arg (Pure id)

flag :: String -> Parser Bool
flag n = Ap (Flag n) (Pure id)

opt :: Read a => String -> Parser (Maybe a)
opt n = Ap (Opt n readMaybe) (Pure id)

optDef :: Read a => String -> a -> Parser a
optDef n d = Ap (Opt n readMaybe) (Pure (fromMaybe d))

parseArgsIO :: [String] -> Parser a -> IO a
parseArgsIO args p = either fail pure (parseArgs args p)

parseArgs :: [String] -> Parser a -> Either String a
parseArgs []       p = parserToEither p
parseArgs (x : xs) p = do
    (xs', p') <- singleArg p x xs
    parseArgs xs' p'

singleArg :: Parser a -> String -> [String] -> Either String ([String], Parser a)
singleArg (Pure _)           x _  = Left $ "Extra argument " ++ x
singleArg (Ap Arg p)         x xs
    | null x || head x /= '-'     = Right (xs, fmap ($ x) p)
    | otherwise                   = fmap2 (Ap Arg) (singleArg p x xs)
singleArg (Ap f@(Flag n) p)  x xs
    | x == n                      = Right (xs, fmap ($ True) p)
    | otherwise                   = fmap2 (Ap f) (singleArg p x xs)
singleArg (Ap o@(Opt n r) p) x xs
    | x == n                      = case xs of
        [] -> Left $ "Expected an argument for " ++ n
        (x' : xs') -> case r x' of
            Nothing -> Left $ "Cannot read an argument of " ++ n ++ ": " ++ x'
            Just y  -> Right (xs', fmap ($ Just y) p)
    | otherwise                   = fmap2 (Ap o) (singleArg p x xs)

fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 = fmap . fmap

-- | Convert parser to 'Right' if there are only defaultable pieces left.
parserToEither :: Parser a -> Either String a
parserToEither (Pure x)         = pure x
parserToEither (Ap (Flag _) p)  = parserToEither $ fmap ($ False) p
parserToEither (Ap (Opt _ _) p) = parserToEither $ fmap ($ Nothing) p
parserToEither (Ap Arg _)       = Left "argument required"
