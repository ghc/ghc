module GHC.Iface.Extended (

  FieldName,

  readIfaceField, readIfaceFieldWith,
  readField, readFieldWith,

  writeIfaceField, writeIfaceFieldWith,
  writeField, writeFieldWith,

  deleteField, deleteIfaceField,

) where

import GhcPrelude
import Binary
import HscTypes

import Data.Map as Map


type FieldName = String

--------------------------------------------------------------------------------
-- | Reading

readIfaceField :: Binary a => FieldName -> ModIface -> IO (Maybe a)
readIfaceField name = readIfaceFieldWith name get

readField :: Binary a => FieldName -> ExtendedFields -> IO (Maybe a)
readField name = readFieldWith name get

readIfaceFieldWith :: FieldName -> (BinHandle -> IO a) -> ModIface -> IO (Maybe a)
readIfaceFieldWith name read iface = readFieldWith name read (mi_ext_fields iface)

readFieldWith :: FieldName -> (BinHandle -> IO a) -> ExtendedFields -> IO (Maybe a)
readFieldWith name read fields = sequence $ ((read =<<) . dataHandle) <$>
  Map.lookup name (getExtendedFields fields)

--------------------------------------------------------------------------------
-- | Writing

writeIfaceField :: Binary a => FieldName -> a -> ModIface -> IO ModIface
writeIfaceField name x = writeIfaceFieldWith name (`put_` x)

writeField :: Binary a => FieldName -> a -> ExtendedFields -> IO ExtendedFields
writeField name x = writeFieldWith name (`put_` x)

writeIfaceFieldWith :: FieldName -> (BinHandle -> IO ()) -> ModIface -> IO ModIface
writeIfaceFieldWith name write iface = do
  fields <- writeFieldWith name write (mi_ext_fields iface)
  return iface{ mi_ext_fields = fields }

writeFieldWith :: FieldName -> (BinHandle -> IO ()) -> ExtendedFields -> IO ExtendedFields
writeFieldWith name write fields = do
  bh <- openBinMem (1024 * 1024)
  write bh
  --
  bd <- handleData bh
  return $ ExtendedFields (Map.insert name bd $ getExtendedFields fields)

deleteField :: FieldName -> ExtendedFields -> ExtendedFields
deleteField name (ExtendedFields fs) = ExtendedFields $ delete name fs

deleteIfaceField :: FieldName -> ModIface -> ModIface
deleteIfaceField name iface = iface { mi_ext_fields = deleteField name (mi_ext_fields iface) }
