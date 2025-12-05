{-# LANGUAGE OverloadedStrings, GADTs, TypeAbstractions #-}
module Main (main) where

import qualified Data.Binary as Bin
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Control.Exception (bracket)
import Control.Monad (void)
import Data.Word (Word8)
import GHCi.Message
  ( Message(..)
  , mkPipeFromHandles
  , remoteCall
  , Pipe
  )
import GHCi.Server
  ( CustomMessageHandler
  , defaultServerWithCustom
  )
import System.Environment
  ( getArgs
  , getExecutablePath
  , getProgName
  , withArgs
  )
import System.Exit (exitFailure)
import System.IO
  ( Handle
  , BufferMode(..)
  , hSetBuffering
  , hSetBinaryMode
  , hClose
  , hPutStrLn
  , stderr
  )
import System.Posix.IO
  ( createPipe
  , fdToHandle
  , setFdOption
  , FdOption(CloseOnExec)
  )
import System.Process
  ( createProcess
  , proc
  , std_in
  , std_out
  , std_err
  , StdStream(Inherit)
  , terminateProcess
  , waitForProcess
  , ProcessHandle
  )
import Text.Read (readMaybe)

--------------------------------------------------------------------------------
-- Shared request/response definitions and helpers

data ClientCommand a where
  SquareCommand :: Int -> ClientCommand Int
  MulCommand :: Int -> Int -> ClientCommand Int

deriving instance (Show (ClientCommand a))

data Some c f where
  Some :: c a => f a -> Some c f


instance Bin.Binary (Some Bin.Binary ClientCommand) where
  put (Some i) =
    case i of
      SquareCommand n -> Bin.put (0 :: Word8) >> Bin.put n
      MulCommand m n -> Bin.put (1 :: Word8) >> Bin.put m >> Bin.put n

  get = do
    (tag :: Word8) <- Bin.get
    fmap Some $ case tag of
      0 -> SquareCommand <$> Bin.get
      1 -> MulCommand <$> Bin.get <*> Bin.get


customTag :: Word8
customTag = 0x42

encodeLazy :: Bin.Binary a => a -> BS.ByteString
encodeLazy = BL.toStrict . Bin.encode

decodeLazy :: Bin.Binary a => BS.ByteString -> Either String a
decodeLazy bs =
  case Bin.decodeOrFail (BL.fromStrict bs) of
    Left (_, _, err) -> Left err
    Right (_, _, a)  -> Right a

--------------------------------------------------------------------------------
-- Mode selection

data Mode
  = RunClient Int
  | RunServer [String] -- forwarded to GHCi.Server

defaultInput :: Int
defaultInput = 12

parseMode :: [String] -> Either String Mode
parseMode [] = Right (RunClient defaultInput)
parseMode ["client"] = Right (RunClient defaultInput)
parseMode ["client", nStr] =
  case readMaybe nStr of
    Just n -> Right (RunClient n)
    Nothing -> Left $ "Unable to parse integer argument: " ++ nStr
parseMode ("client":_) = Left "Too many arguments for client mode."
parseMode ("server":rest) = Right (RunServer rest)
parseMode args = Left "Unknown mode, use client/server"

usage :: IO ()
usage = do
  prog <- getProgName
  putStrLn $ unlines
    [ "Usage:"
    , "  " ++ prog ++ " [client [n]]   Run the client and square n (default 12)."
    , "  " ++ prog ++ " server <write-fd> <read-fd>  Run as an iserv process."
    ]

--------------------------------------------------------------------------------
-- Client/server drivers

main :: IO ()
main = do
  args <- getArgs
  case parseMode args of
    Left err -> do
      hPutStrLn stderr err
      usage
      exitFailure
    Right (RunClient n) -> runClient n
    Right (RunServer serverArgs) ->
      withArgs serverArgs (defaultServerWithCustom (customHandler handleClientCommand))

handleClientCommand :: ClientCommand a -> IO a
handleClientCommand (SquareCommand n) = pure $ n * n
handleClientCommand (MulCommand n m)  = pure $ n * m


customMessage :: (Show a, Bin.Binary a) => Pipe -> ClientCommand a -> IO a
customMessage pipe c = do
  let payload = encodeLazy (Some @Bin.Binary c)
  putStrLn $ "Sending: " ++ show c
  respBytes <- remoteCall pipe (CustomMessage customTag payload)
  case decodeLazy respBytes of
    Left err -> error $ "Decode error: " ++ err
    Right res -> pure res


runClient :: Int -> IO ()
runClient input = do
  serverExe <- getExecutablePath
  withServer serverExe $ \hFromServer hToServer -> do
    pipe <- mkPipeFromHandles hFromServer hToServer
    res  <- customMessage pipe (SquareCommand input)
    res2  <- customMessage pipe (MulCommand 2 res)
    putStrLn $ "Square returned: " ++ show res2

withServer :: FilePath -> (Handle -> Handle -> IO a) -> IO a
withServer serverExe action = do
  (ghcRead, serverWrite) <- createPipe
  (serverRead, ghcWrite) <- createPipe
  mapM_ (\h -> setFdOption h CloseOnExec False) [serverWrite, serverRead]
  let args = ["server", show serverWrite, show serverRead]
  (_, _, _, ph) <- createProcess (proc serverExe args)
    { std_in  = Inherit
    , std_out = Inherit
    , std_err = Inherit
    }
  bracket (mkHandles ghcRead ghcWrite)
          (\(hFromServer, hToServer) -> do
             hClose hFromServer
             hClose hToServer
             terminateProcess ph
             void (waitForProcess ph))
          (\(hFromServer, hToServer) -> action hFromServer hToServer)
  where
    mkHandles r w = do
      hR <- fdToHandle r
      hW <- fdToHandle w
      mapM_ (`hSetBuffering` NoBuffering) [hR, hW]
      mapM_ (`hSetBinaryMode` True) [hR, hW]
      pure (hR, hW)

--------------------------------------------------------------------------------
-- Custom handler

customHandler :: (Bin.Binary (Some Bin.Binary f)) => (forall a . f a -> IO a) -> CustomMessageHandler
customHandler handler tag payload
  | tag == customTag =
      case decodeLazy payload of
        Left err -> do
          hPutStrLn stderr $ "Custom handler decode error: " ++ err
          pure Nothing
        Right (Some @Bin.Binary r) -> do
          res <- handler r
          pure . Just $ encodeLazy res
  | otherwise = pure Nothing
