module GHC.Iface.Ext.Fields
   ( ExtensibleFields (..)
   , FieldName
   , emptyExtensibleFields
   -- * Reading
   , readField
   , readFieldWith
   -- * Writing
   , writeField
   , writeFieldWith
   -- * Deletion
   , deleteField
   )
where

import GHC.Prelude
import GHC.Utils.Binary

import Control.Monad
import Data.Map         ( Map )
import qualified Data.Map as Map
import Control.DeepSeq

type FieldName = String

newtype ExtensibleFields = ExtensibleFields { getExtensibleFields :: (Map FieldName BinData) }

instance Binary ExtensibleFields where
  put_ bh (ExtensibleFields fs) = do
    put_ bh (Map.size fs :: Int)

    -- Put the names of each field, and reserve a space
    -- for a payload pointer after each name:
    header_entries <- forM (Map.toList fs) $ \(name, dat) -> do
      put_ bh name
      field_p_p <- tellBin bh
      put_ bh field_p_p
      return (field_p_p, dat)

    -- Now put the payloads and use the reserved space
    -- to point to the start of each payload:
    forM_ header_entries $ \(field_p_p, dat) -> do
      field_p <- tellBin bh
      putAt bh field_p_p field_p
      seekBin bh field_p
      put_ bh dat

  get bh = do
    n <- get bh :: IO Int

    -- Get the names and field pointers:
    header_entries <- replicateM n $
      (,) <$> get bh <*> get bh

    -- Seek to and get each field's payload:
    fields <- forM header_entries $ \(name, field_p) -> do
      seekBin bh field_p
      dat <- get bh
      return (name, dat)

    return . ExtensibleFields . Map.fromList $ fields

instance NFData ExtensibleFields where
  rnf (ExtensibleFields fs) = rnf fs

emptyExtensibleFields :: ExtensibleFields
emptyExtensibleFields = ExtensibleFields Map.empty

--------------------------------------------------------------------------------
-- | Reading

readField :: Binary a => FieldName -> ExtensibleFields -> IO (Maybe a)
readField name = readFieldWith name get

readFieldWith :: FieldName -> (BinHandle -> IO a) -> ExtensibleFields -> IO (Maybe a)
readFieldWith name read fields = sequence $ ((read =<<) . dataHandle) <$>
  Map.lookup name (getExtensibleFields fields)

--------------------------------------------------------------------------------
-- | Writing

writeField :: Binary a => FieldName -> a -> ExtensibleFields -> IO ExtensibleFields
writeField name x = writeFieldWith name (`put_` x)

writeFieldWith :: FieldName -> (BinHandle -> IO ()) -> ExtensibleFields -> IO ExtensibleFields
writeFieldWith name write fields = do
  bh <- openBinMem (1024 * 1024)
  write bh
  --
  bd <- handleData bh
  return $ ExtensibleFields (Map.insert name bd $ getExtensibleFields fields)

deleteField :: FieldName -> ExtensibleFields -> ExtensibleFields
deleteField name (ExtensibleFields fs) = ExtensibleFields $ Map.delete name fs
