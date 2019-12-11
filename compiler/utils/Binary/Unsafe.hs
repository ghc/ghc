{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns, CPP #-}

module Binary.Unsafe where

#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail(..))
#endif

import GhcPrelude hiding (fail)

import Control.Monad (when)
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


{-# INLINE castBin #-}
castBin :: Bin a -> Bin b
castBin (BinPtr !i) = BinPtr i

-- | Get access to the underlying buffer.
--
-- It is quite important that no references to the 'ByteString' leak out of the
-- continuation lest terrible things happen.
{-# INLINE withBinBuffer #-}
withBinBuffer :: BinData -> (ByteString -> IO a) -> IO a
withBinBuffer (BinData sz arr) action =
  withForeignPtr arr $ \ptr ->
    BS.unsafePackCStringLen (castPtr ptr, sz) >>= action

-- -----------------------------------------------------------------------------
-- Put
-- -----------------------------------------------------------------------------

data EnvP = EnvP !UserDataP !FastMutInt !FastMutInt !(IORef BinArray)

newtype Put a = Put { unput :: EnvP -> IO a }

instance Functor Put where
  {-# INLINE fmap #-}
  fmap f m = Put $ \env ->
    fmap f (unput m env)

instance Applicative Put where
  {-# INLINE pure #-}
  pure x = Put $ \_ -> pure x

  {-# INLINE (<*>) #-}
  f <*> x = Put $ \env ->
    unput f env <*> unput x env

instance Monad Put where
  {-# INLINE (>>=) #-}
  x >>= f = Put $ \env -> do
    a <- unput x env
    unput (f a) env

instance MonadFail Put where
  {-# INLINE fail #-}
  fail = ioP . fail

{-# INLINE expand #-}
expand :: Int -> FastMutInt -> IORef BinArray -> IO ()
expand off szr binr = do
  sz  <- readFastMutInt szr
  arr <- readIORef binr
  let !sz' = getSize sz
  arr' <- mallocForeignPtrBytes sz'
  withForeignPtr arr $ \old ->
    withForeignPtr arr' $ \new ->
      copyBytes new old sz
  writeFastMutInt szr sz'
  writeIORef binr arr'
  where
    getSize !sz | sz >= off = sz
                | otherwise = getSize (sz * 2)

{-# INLINE runBuffer #-}
runBuffer :: Int -> Put () -> IO BinData
runBuffer initialSize m = do
  ixr  <- newFastMutInt
  szr  <- newFastMutInt
  writeFastMutInt ixr 0
  writeFastMutInt szr initialSize
  arr  <- mallocForeignPtrBytes initialSize
  binr <- newIORef arr
  unput m (EnvP noUserData ixr szr binr)
  BinData <$> readFastMutInt ixr <*> readIORef binr

{-# INLINE runPutIO #-}
runPutIO :: Put () -> IO BinData
runPutIO = runBuffer (1024 * 1024)

{-# INLINE runPut #-}
runPut :: Put () -> BinData
runPut = unsafePerformIO . runPutIO

-- -----------------------------------------------------------------------------
-- Putting
-- -----------------------------------------------------------------------------

{-# INLINE putPrim #-}
putPrim :: Int -> (Ptr Word8 -> IO ()) -> Put ()
putPrim n f = Put $ \(EnvP _ ixr szr binr) -> do
  ix <- readFastMutInt ixr
  sz <- readFastMutInt szr
  arr <- readIORef binr
  when (ix + n > sz) (expand (ix + n) szr binr)
  withForeignPtr arr $ \op -> f (op `plusPtr` ix)
  writeFastMutInt ixr (ix + n)

{-# INLINE ioP #-}
ioP :: IO a -> Put a
ioP m = Put $ \_ -> m

{-# INLINE seekP #-}
seekP :: Bin a -> Put ()
seekP (BinPtr !i) = Put $ \(EnvP _ ixr _ _) ->
  writeFastMutInt ixr i

{-# INLINE tellP #-}
tellP :: Put (Bin a)
tellP = Put $ \(EnvP _ ixr _ _) ->
  BinPtr <$> readFastMutInt ixr

{-# INLINE userDataP #-}
userDataP :: Put UserDataP
userDataP = Put $ \(EnvP ud _ _ _) -> return ud

-- -----------------------------------------------------------------------------
-- Get
-- -----------------------------------------------------------------------------

data EnvG = EnvG !UserDataG !FastMutInt !Int !BinData

newtype Get a = Get { unget :: EnvG -> IO a }

instance Functor Get where
  {-# INLINE fmap #-}
  fmap f m = Get $ \env ->
    fmap f (unget m env)

instance Applicative Get where
  {-# INLINE pure #-}
  pure x = Get $ \_ -> pure x

  {-# INLINE (<*>) #-}
  f <*> x = Get $ \env ->
    unget f env <*> unget x env

instance Monad Get where
  {-# INLINE (>>=) #-}
  x >>= f = Get $ \env -> do
    a <- unget x env
    unget (f a) env

instance MonadFail Get where
  {-# INLINE fail #-}
  fail = ioG . fail

{-# INLINE runGetIO #-}
runGetIO :: BinData -> Get a -> IO a
runGetIO bd@(BinData size _) m = do
  ixr <- newFastMutInt
  writeFastMutInt ixr 0
  unget m (EnvG noUserData ixr size bd)

{-# INLINE runGet #-}
runGet :: BinData -> Get a -> a
runGet bd = unsafePerformIO . runGetIO bd

{-# INLINE seekG #-}
seekG :: Bin a -> Get ()
seekG (BinPtr !i) = Get $ \(EnvG _ ixr _ _) ->
  writeFastMutInt ixr i

{-# INLINE tellG #-}
tellG :: Get (Bin a)
tellG = Get $ \(EnvG _ ixr _ _) ->
  BinPtr <$> readFastMutInt ixr

-- -----------------------------------------------------------------------------
-- Getting
-- -----------------------------------------------------------------------------

{-# INLINE getPrim #-}
getPrim :: Int -> (Ptr Word8 -> IO a) -> Get a
getPrim n f = Get $ \(EnvG _ ixr end (BinData _ arr)) -> do
  ix <- readFastMutInt ixr
  when (ix + n > end) $
    ioError (mkIOError eofErrorType "Binary.Internal.getPrim" Nothing Nothing)
  w <- withForeignPtr arr $ \op -> f (op `plusPtr` ix)
  writeFastMutInt ixr (ix + n)
  return w

{-# INLINE interleaveG #-}
interleaveG :: Get a -> Get a
interleaveG m = Get $ \(EnvG ud ixr end bd) -> do
 ixr' <- newFastMutInt
 writeFastMutInt ixr' =<< readFastMutInt ixr
 unsafeInterleaveIO $ unget m (EnvG ud ixr' end bd)

{-# INLINE getSlice #-}
getSlice :: Bin b -> Get a -> Get a
getSlice (BinPtr !end) m = Get $ \(EnvG ud ixr _ bd) ->
  unget m (EnvG ud ixr end bd)

{-# INLINE ioG #-}
ioG :: IO a -> Get a
ioG m = Get $ \_ -> m

{-# INLINE userDataG #-}
userDataG :: Get UserDataG
userDataG = Get $ \(EnvG ud _ _ _) -> return ud

-- -----------------------------------------------------------------------------
-- File IO
-- -----------------------------------------------------------------------------

{-# INLINE writeBinData #-}
writeBinData :: BinData -> FilePath -> IO ()
writeBinData (BinData sz arr) fp =
  withBinaryFile fp WriteMode $ \h ->
    withForeignPtr arr $ \p ->
      hPutBuf h p sz

{-# INLINE readBinData #-}
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

{-# INLINE writeState #-}
writeState :: (Name -> Put ())
           -> (Name -> Put ())
           -> (FastString -> Put ())
           -> Put a
           -> Put a
writeState nonbind bind fs m = Put $ \(EnvP _ ixr szr binr) ->
  unput m (EnvP (UserDataP nonbind bind fs) ixr szr binr)

data UserDataG
   = UserDataG {
       get_name :: Get Name,
       get_fs   :: Get FastString
     }

{-# INLINE noUserData #-}
noUserData :: a
noUserData = panic "Binary.UserData not defined"

{-# INLINE readState #-}
readState :: Get Name
          -> Get FastString
          -> Get a
          -> Get a
readState name fs m = Get $ \(EnvG _ ixr end bd) ->
  unget m (EnvG (UserDataG name fs) ixr end bd)
