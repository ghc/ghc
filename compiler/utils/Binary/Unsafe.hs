{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns #-}

module Binary.Unsafe where

import GhcPrelude

import Control.Monad.Reader
import Data.IORef
import Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as BS
import FastMutInt
import FastString
import Foreign
import {-# SOURCE #-} Name (Name)
import PlainPanic
import System.IO
import System.IO.Error
import System.IO.Unsafe


-- -----------------------------------------------------------------------------
-- Arrays
-- -----------------------------------------------------------------------------

type BinArray = ForeignPtr Word8

data BinData = BinData { binSize :: !Int, unarr :: !BinArray }

newtype Bin a = BinPtr Int
  deriving (Eq, Ord, Show, Bounded)

castBin :: Bin a -> Bin b
castBin (BinPtr !i) = BinPtr i

-- | Get access to the underlying buffer.
--
-- It is quite important that no references to the 'ByteString' leak out of the
-- continuation lest terrible things happen.
withBinBuffer :: BinData -> (ByteString -> IO a) -> IO a
withBinBuffer (BinData sz arr) action =
  withForeignPtr arr $ \ptr ->
    BS.unsafePackCStringLen (castPtr ptr, sz) >>= action

-- -----------------------------------------------------------------------------
-- Put
-- -----------------------------------------------------------------------------

newtype Put a = Put { unput :: ReaderT EnvP IO a }
  deriving (Functor, Applicative, Monad)

-- Internal reader data for `Put` monad.
data EnvP
   = EnvP {
       put_user   ::  UserDataP,
       put_offset :: !FastMutInt,
       put_size   :: !FastMutInt,
       put_arr    :: !(IORef BinArray)
     }

allocate :: Int -> IO EnvP
allocate initialSize = do
  arr <- mallocForeignPtrBytes initialSize
  EnvP noUserData <$> initInt 0 <*> initInt initialSize <*> newIORef arr
  where
    initInt n   = do
      int <- newFastMutInt
      writeFastMutInt int n
      return int

expand :: Int -> Put ()
expand off = do
  sz  <- putSize
  arr <- putArr
  let !sz' = getSize sz
  Put $ do
    sz_r  <- put_size <$> ask
    arr_r <- put_arr  <$> ask
    liftIO $ do
      arr' <- mallocForeignPtrBytes sz'
      withForeignPtr arr $ \old ->
        withForeignPtr arr' $ \new ->
          copyBytes new old sz
      writeFastMutInt sz_r sz'
      writeIORef arr_r arr'
  where
    getSize !sz | sz >= off = sz
                | otherwise = getSize (sz * 2)

reallocate :: Put ()
reallocate = expand . (2 *) =<< putSize

runBuffer :: Int -> Put () -> IO BinData
runBuffer initialSize (Put m) = do
  bin <- runReaderT (m >> ask) =<< allocate initialSize
  off <- readFastMutInt (put_offset bin)
  BinData (off + 1) <$> readIORef (put_arr bin)

runPutIO :: Put () -> IO BinData
runPutIO = runBuffer (1024 * 1024)

runPut :: Put () -> BinData
runPut = unsafePerformIO . runPutIO

askP :: Put EnvP
askP = Put ask

putEnv :: (EnvP -> IO a) -> Put a
putEnv f = Put $ liftIO . f =<< ask

putOffset, putSize :: Put Int
putOffset = putEnv (readFastMutInt . put_offset)
putSize   = putEnv (readFastMutInt . put_size  )

putArr :: Put BinArray
putArr = putEnv (readIORef . put_arr)

-- -----------------------------------------------------------------------------
-- Putting
-- -----------------------------------------------------------------------------

putPrim :: Int -> (Ptr Word8 -> IO ()) -> Put ()
putPrim n f = do
  ix <- putOffset
  sz <- putSize
  when (ix + n >= sz) reallocate
  arr <- putArr
  go ix arr
  where
    go ix arr =
      Put $ do
        ixr <- put_offset <$> ask
        liftIO $ do
          withForeignPtr arr $ \op -> f (op `plusPtr` ix)
          writeFastMutInt ixr (ix + n)

ioP :: IO a -> Put a
ioP m = Put (liftIO m)

userDataP :: Put UserDataP
userDataP = Put $ put_user <$> ask

offsetP :: Put Int
offsetP = do EnvP _ off _ _ <- askP; Put . liftIO $ readFastMutInt off

seekP :: Bin a -> Put ()
seekP (BinPtr !i) = do
  EnvP _ ixr _ _ <- askP
  Put . liftIO $ writeFastMutInt ixr i

tellP :: Put (Bin a)
tellP = BinPtr <$> offsetP

-- -----------------------------------------------------------------------------
-- Get
-- -----------------------------------------------------------------------------

newtype Get a = Get { unget :: ReaderT EnvG IO a }
  deriving (Functor, Applicative, Monad)

-- Internal reader data for `Get` monad.
data EnvG
   = EnvG {
       get_user   ::  UserDataG,
       get_offset :: !FastMutInt,
       get_end    :: !Int,
       get_arr    :: !BinData
     }

runGetIO :: BinData -> Get a -> IO a
runGetIO bd m = (runReaderT (unget m) =<< mkEnvG bd)

runGet :: BinData -> Get a -> a
runGet bd = unsafePerformIO . runGetIO bd

askG :: Get EnvG
askG = Get ask

offsetG :: Get Int
offsetG = do EnvG _ off _ _ <- askG; Get . liftIO $ readFastMutInt off

seekG :: Bin a -> Get ()
seekG (BinPtr !i) = do
  EnvG _ ixr _ _ <- askG
  Get . liftIO $ writeFastMutInt ixr i

tellG :: Get (Bin a)
tellG = BinPtr <$> offsetG

getEnv :: (EnvG -> IO a) -> Get a
getEnv f = Get $ liftIO . f =<< ask

getOffset, getEnd :: Get Int
getOffset = getEnv (readFastMutInt . get_offset)
getEnd    = Get $ get_end <$> ask

getSize :: Get Int
getSize = Get $ binSize . get_arr <$> ask

getArr :: Get BinArray
getArr =  Get $ unarr . get_arr <$> ask

mkEnvG :: BinData -> IO EnvG
mkEnvG bd@(BinData size _) = do
  i <- newFastMutInt
  writeFastMutInt i 0
  return $ EnvG noUserData i size bd

-- -----------------------------------------------------------------------------
-- Getting
-- -----------------------------------------------------------------------------

getPrim :: Int -> (Ptr Word8 -> IO a) -> Get a
getPrim n f = do
  ix  <- getOffset
  end <- getEnd
  when (ix + n >= end) $
    ioG $ ioError (mkIOError eofErrorType "Binary.Internal.getPrim" Nothing Nothing)
  arr <- getArr
  do
    ixr <- get_offset <$> askG
    ioG $ do
      w <- withForeignPtr arr $ \op -> f (op `plusPtr` ix)
      writeFastMutInt ixr (ix + n)
      return w

interleaveG :: Get a -> Get a
interleaveG m = do
  env <- askG
  ioG $ unsafeInterleaveIO . runReaderT (unget m) =<< dup env
  where
    dup :: EnvG -> IO EnvG
    dup (EnvG dat off end arr) = do
      off' <- newFastMutInt
      writeFastMutInt off' =<< readFastMutInt off
      return $ EnvG dat off' end arr

getSlice :: Bin b -> Get a -> Get a
getSlice (BinPtr !end) (Get m) = Get $ local (\x -> x {get_end = end}) m

ioG :: IO a -> Get a
ioG m = Get (liftIO m)

userDataG :: Get UserDataG
userDataG = Get $ get_user <$> ask

-- -----------------------------------------------------------------------------
-- File IO
-- -----------------------------------------------------------------------------

writeBinData :: BinData -> FilePath -> IO ()
writeBinData (BinData sz arr) fp =
  withBinaryFile fp WriteMode $ \h ->
    withForeignPtr arr $ \p ->
      hPutBuf h p sz

readBinData :: FilePath -> IO BinData
readBinData fp =
  withBinaryFile fp ReadMode $ \h -> do
    sz  <- fromIntegral <$> hFileSize h
    arr <- mallocForeignPtrBytes sz
    count <- withForeignPtr arr $ \p -> hGetBuf h p sz
    when (count /= sz) $
      error ("Binary.Internal.readBinMem: only read " ++ show count ++ " bytes")
    return $ BinData sz arr

-- -----------------------------------------------------------------------------
-- User Data
-- -----------------------------------------------------------------------------

data UserDataP
   = UserDataP {
       put_nonbinding_name :: Name       -> Put (),
       put_binding_name    :: Name       -> Put (),
       put_fs              :: FastString -> Put ()
     }

writeState :: (Name -> Put ())
           -> (Name -> Put ())
           -> (FastString -> Put ())
           -> Put a
           -> Put a
writeState nonbind bind fs (Put m) = Put $ local setWriteState m
  where
    setWriteState env = env { put_user = UserDataP nonbind bind fs }

data UserDataG
   = UserDataG {
       get_name :: Get Name,
       get_fs   :: Get FastString
     }

noUserData :: a
noUserData = panic "Binary.UserData not defined"

readState :: Get Name
          -> Get FastString
          -> Get a
          -> Get a
readState name fs (Get m) = Get $ local setReadState m
  where
    setReadState env = env { get_user = UserDataG name fs }
