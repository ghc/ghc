{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns, CPP #-}

module Binary.Unsafe where

#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail)
import qualified Control.Monad.Fail
#endif

import GhcPrelude
import GHC.Exts

import Control.Monad
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

newtype Put a = Put { unput :: EnvP -> IO a }

instance Functor Put where
  {-# INLINE fmap #-}
  fmap f m = Put . oneShot $ \env -> fmap f (unput m env)

instance Applicative Put where
  {-# INLINE pure #-}
  pure x = Put . oneShot $ \_ -> pure x

  {-# INLINE (<*>) #-}
  f <*> x = Put . oneShot $ \env ->
    unput f env <*> unput x env

instance Monad Put where
  {-# INLINE (>>=) #-}
  x >>= f = Put . oneShot $ \env -> do
    a <- unput x env
    unput (f a) env

#if !MIN_VERSION_base(4,13,0)
  {-# INLINE fail #-}
  fail s = Put . oneShot $ \_ -> fail s
#endif

instance MonadFail Put where
  {-# INLINE fail #-}
  fail s = Put . oneShot $ \_ -> fail s

-- Internal reader data for `Put` monad.
data EnvP
   = EnvP {
       put_user   ::  UserDataP,
       put_offset :: !FastMutInt,
       put_size   :: !FastMutInt,
       put_arr    :: !(IORef BinArray)
     }

{-# INLINE expand #-}
expand :: Int -> FastMutInt -> IORef BinArray -> IO ()
expand off szr binr = do
  sz   <- readFastMutInt szr
  let !sz' = getSize sz
  arr' <- mallocForeignPtrBytes sz'
  arr  <- readIORef binr
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
putPrim n f = Put . oneShot $ \(EnvP _ ixr szr binr) -> do
  ix <- readFastMutInt ixr
  sz <- readFastMutInt szr
  when (ix + n > sz) (expand (ix + n) szr binr)
  arr <- readIORef binr
  withForeignPtr arr $ \op -> f (op `plusPtr` ix)
  writeFastMutInt ixr (ix + n)

{-# INLINE ioP #-}
ioP :: IO a -> Put a
ioP m = Put . oneShot $ \_ -> m

{-# INLINE userDataP #-}
userDataP :: Put UserDataP
userDataP = Put . oneShot $ \(EnvP ud _ _ _) -> return ud

{-# INLINE seekP #-}
seekP :: Bin a -> Put ()
seekP (BinPtr !i) = Put . oneShot $ \(EnvP _ ixr _ _) ->
  writeFastMutInt ixr i

{-# INLINE tellP #-}
tellP :: Put (Bin a)
tellP = Put . oneShot $ \(EnvP _ ixr _ _) ->
  BinPtr <$> readFastMutInt ixr

-- -----------------------------------------------------------------------------
-- Get
-- -----------------------------------------------------------------------------

newtype Get a = Get { unget :: EnvG -> IO a }

instance Functor Get where
  {-# INLINE fmap #-}
  fmap f m = Get . oneShot $ \env -> fmap f (unget m env)

instance Applicative Get where
  {-# INLINE pure #-}
  pure x = Get . oneShot $ \_ -> pure x

  {-# INLINE (<*>) #-}
  f <*> x = Get . oneShot $ \env ->
    unget f env <*> unget x env

instance Monad Get where
  {-# INLINE (>>=) #-}
  x >>= f = Get . oneShot $ \env -> do
    a <- unget x env
    unget (f a) env

#if !MIN_VERSION_base(4,13,0)
  {-# INLINE fail #-}
  fail s = Get . oneShot $ \_ -> fail s
#endif

instance MonadFail Get where
  {-# INLINE fail #-}
  fail s = Get . oneShot $ \_ -> fail s

-- Internal reader data for `Get` monad.
data EnvG
   = EnvG {
       get_user   ::  UserDataG,
       get_offset :: !FastMutInt,
       get_end    :: !Int,
       get_arr    :: !BinData
     }

{-# INLINE runGetIO #-}
runGetIO :: BinData -> Get a -> IO a
runGetIO bd@(BinData sz _) m = do
  ixr <- newFastMutInt
  writeFastMutInt ixr 0
  unget m (EnvG noUserData ixr sz bd)


{-# INLINE runGet #-}
runGet :: BinData -> Get a -> a
runGet bd = unsafePerformIO . runGetIO bd

{-# INLINE seekG #-}
seekG :: Bin a -> Get ()
seekG (BinPtr !i) = Get . oneShot $ \(EnvG _ ixr _ _) ->
  writeFastMutInt ixr i

{-# INLINE tellG #-}
tellG :: Get (Bin a)
tellG = Get . oneShot $ \(EnvG _ ixr _ _) ->
  BinPtr <$> readFastMutInt ixr

-- -----------------------------------------------------------------------------
-- Getting
-- -----------------------------------------------------------------------------

{-# INLINE getPrim #-}
getPrim :: Int -> (Ptr Word8 -> IO a) -> Get a
getPrim n f = Get . oneShot $ \(EnvG _ ixr end (BinData _ arr)) -> do
  ix <- readFastMutInt ixr
  when (ix + n > end) $
    ioError (mkIOError eofErrorType "Binary.Unsafe.getPrim" Nothing Nothing)
  w <- withForeignPtr arr $ \op -> f (op `plusPtr` ix)
  writeFastMutInt ixr (ix + n)
  return w

{-# INLINE interleaveG #-}
interleaveG :: Get a -> Get a
interleaveG m = Get . oneShot $ \(EnvG ud ixr end bd) -> do
  ixr' <- newFastMutInt
  writeFastMutInt ixr' =<< readFastMutInt ixr
  unget m (EnvG ud ixr' end bd)

{-# INLINE getSlice #-}
getSlice :: Bin b -> Get a -> Get a
getSlice (BinPtr !end) m = Get . oneShot $ \(EnvG ud ixr _ bd) ->
  unget m (EnvG ud ixr end bd)

{-# INLINE ioG #-}
ioG :: IO a -> Get a
ioG m = Get . oneShot $ \_ -> m

{-# INLINE userDataG #-}
userDataG :: Get UserDataG
userDataG = Get . oneShot $ \(EnvG ud _ _ _) -> return ud

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
      error ("Binary.Unsafe.readBinData: only read " ++ show count ++ " bytes")
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
writeState nonbind bind fs m = Put . oneShot $ \(EnvP _ ixr szr binr) ->
  unput m (EnvP (UserDataP nonbind bind fs) ixr szr binr)

data UserDataG
   = UserDataG {
       get_name :: Get Name,
       get_fs   :: Get FastString
     }

{-# INLINE noUserData #-}
noUserData :: a
noUserData = panic "Binary.Unsafe.noUserData"

{-# INLINE readState #-}
readState :: Get Name
          -> Get FastString
          -> Get a
          -> Get a
readState name fs m = Get . oneShot $ \(EnvG _ ixr end bd) ->
  unget m (EnvG (UserDataG name fs) ixr end bd)
