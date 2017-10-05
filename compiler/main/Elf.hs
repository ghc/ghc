{-
-----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 2015
--
-- ELF format tools
--
-----------------------------------------------------------------------------
-}

module Elf (
    readElfSectionByName,
    readElfNoteAsString,
    makeElfNote
  ) where

import GhcPrelude

import AsmUtils
import Exception
import DynFlags
import ErrUtils
import Maybes     (MaybeT(..),runMaybeT)
import Util       (charToC)
import Outputable (text,hcat,SDoc)

import Control.Monad (when)
import Data.Binary.Get
import Data.Word
import Data.Char (ord)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as B8

{- Note [ELF specification]
   ~~~~~~~~~~~~~~~~~~~~~~~~

   ELF (Executable and Linking Format) is described in the System V Application
   Binary Interface (or ABI). The latter is composed of two parts: a generic
   part and a processor specific part. The generic ABI describes the parts of
   the interface that remain constant across all hardware implementations of
   System V.

   The latest release of the specification of the generic ABI is the version
   4.1 from March 18, 1997:

     - http://www.sco.com/developers/devspecs/gabi41.pdf

   Since 1997, snapshots of the draft for the "next" version are published:

     - http://www.sco.com/developers/gabi/

   Quoting the notice on the website: "There is more than one instance of these
   chapters to permit references to older instances to remain valid. All
   modifications to these chapters are forward-compatible, so that correct use
   of an older specification will not be invalidated by a newer instance.
   Approximately on a yearly basis, a new instance will be saved, as it reaches
   what appears to be a stable state."

   Nevertheless we will see that since 1998 it is not true for Note sections.

   Many ELF sections
   -----------------

   ELF-4.1: the normal section number fields in ELF are limited to 16 bits,
   which runs out of bits when you try to cram in more sections than that. Two
   fields are concerned: the one containing the number of the sections and the
   one containing the index of the section that contains section's names. (The
   same thing applies to the field containing the number of segments, but we
   don't care about it here).

   ELF-next: to solve this, theses fields in the ELF header have an escape
   value (different for each case), and the actual section number is stashed
   into unused fields in the first section header.

   We support this extension as it is forward-compatible with ELF-4.1.
   Moreover, GHC may generate objects with a lot of sections with the
   "function-sections" feature (one section per function).

   Note sections
   -------------

   Sections with type "note" (SHT_NOTE in the specification) are used to add
   arbitrary data into an ELF file. An entry in a note section is composed of a
   name, a type and a value.

   ELF-4.1: "The note information in sections and program header elements holds
   any number of entries, each of which is an array of 4-byte words in the
   format of the target processor." Each entry has the following format:
         | namesz |   Word32: size of the name string (including the ending \0)
         | descsz |   Word32: size of the value
         |  type  |   Word32: type of the note
         |  name  |   Name string (with \0 padding to ensure 4-byte alignment)
         |  ...   |
         |  desc  |   Value (with \0 padding to ensure 4-byte alignment)
         |  ...   |

   ELF-next: "The note information in sections and program header elements
   holds a variable amount of entries. In 64-bit objects (files with
   e_ident[EI_CLASS] equal to ELFCLASS64), each entry is an array of 8-byte
   words in the format of the target processor. In 32-bit objects (files with
   e_ident[EI_CLASS] equal to ELFCLASS32), each entry is an array of 4-byte
   words in the format of the target processor." (from 1998-2015 snapshots)

   This is not forward-compatible with ELF-4.1. In practice, for almost all
   platforms namesz, descz and type fields are 4-byte words for both 32-bit and
   64-bit objects (see elf.h and readelf source code).

   The only exception in readelf source code is for IA_64 machines with OpenVMS
   OS: "This OS has so many departures from the ELF standard that we test it at
   many places" (comment for is_ia64_vms() in readelf.c). In this case, namesz,
   descsz and type fields are 8-byte words and name and value fields are padded
   to ensure 8-byte alignment.

   We don't support this platform in the following code. Reading a note section
   could be done easily (by testing Machine and OS fields in the ELF header).
   Writing a note section, however, requires that we generate a different
   assembly code for GAS depending on the target platform and this is a little
   bit more involved.

-}


-- | ELF header
--
-- The ELF header indicates the native word size (32-bit or 64-bit) and the
-- endianness of the target machine. We directly store getters for words of
-- different sizes as it is more convenient to use. We also store the word size
-- as it is useful to skip some uninteresting fields.
--
-- Other information such as the target machine and OS are left out as we don't
-- use them yet. We could add them in the future if we ever need them.
data ElfHeader = ElfHeader
   { gw16     :: Get Word16   -- ^ Get a Word16 with the correct endianness
   , gw32     :: Get Word32   -- ^ Get a Word32 with the correct endianness
   , gwN      :: Get Word64   -- ^ Get a Word with the correct word size
                              --   and endianness
   , wordSize :: Int          -- ^ Word size in bytes
   }


-- | Read the ELF header
readElfHeader :: DynFlags -> ByteString -> IO (Maybe ElfHeader)
readElfHeader dflags bs = runGetOrThrow getHeader bs `catchIO` \_ -> do
    debugTraceMsg dflags 3 $
      text ("Unable to read ELF header")
    return Nothing
  where
    getHeader = do
      magic    <- getWord32be
      ws       <- getWord8
      endian   <- getWord8
      version  <- getWord8
      skip 9  -- skip OSABI, ABI version and padding
      when (magic /= 0x7F454C46 || version /= 1) $ fail "Invalid ELF header"

      case (ws, endian) of
          -- ELF 32, little endian
          (1,1) -> return . Just $ ElfHeader
                           getWord16le
                           getWord32le
                           (fmap fromIntegral getWord32le) 4
          -- ELF 32, big endian
          (1,2) -> return . Just $ ElfHeader
                           getWord16be
                           getWord32be
                           (fmap fromIntegral getWord32be) 4
          -- ELF 64, little endian
          (2,1) -> return . Just $ ElfHeader
                           getWord16le
                           getWord32le
                           (fmap fromIntegral getWord64le) 8
          -- ELF 64, big endian
          (2,2) -> return . Just $ ElfHeader
                           getWord16be
                           getWord32be
                           (fmap fromIntegral getWord64be) 8
          _     -> fail "Invalid ELF header"


------------------
-- SECTIONS
------------------


-- | Description of the section table
data SectionTable = SectionTable
  { sectionTableOffset :: Word64  -- ^ offset of the table describing sections
  , sectionEntrySize   :: Word16  -- ^ size of an entry in the section table
  , sectionEntryCount  :: Word64  -- ^ number of sections
  , sectionNameIndex   :: Word32  -- ^ index of a special section which
                                  --   contains section's names
  }

-- | Read the ELF section table
readElfSectionTable :: DynFlags
                    -> ElfHeader
                    -> ByteString
                    -> IO (Maybe SectionTable)

readElfSectionTable dflags hdr bs = action `catchIO` \_ -> do
    debugTraceMsg dflags 3 $
      text ("Unable to read ELF section table")
    return Nothing
  where
    getSectionTable :: Get SectionTable
    getSectionTable = do
      skip (24 + 2*wordSize hdr) -- skip header and some other fields
      secTableOffset <- gwN hdr
      skip 10
      entrySize      <- gw16 hdr
      entryCount     <- gw16 hdr
      secNameIndex   <- gw16 hdr
      return (SectionTable secTableOffset entrySize
                           (fromIntegral entryCount)
                           (fromIntegral secNameIndex))

    action = do
      secTable <- runGetOrThrow getSectionTable bs
      -- In some cases, the number of entries and the index of the section
      -- containing section's names must be found in unused fields of the first
      -- section entry (see Note [ELF specification])
      let
        offSize0 = fromIntegral $ sectionTableOffset secTable + 8
                                  + 3 * fromIntegral (wordSize hdr)
        offLink0 = fromIntegral $ offSize0 + fromIntegral (wordSize hdr)

      entryCount'     <- if sectionEntryCount secTable /= 0
                          then return (sectionEntryCount secTable)
                          else runGetOrThrow (gwN hdr) (LBS.drop offSize0 bs)
      entryNameIndex' <- if sectionNameIndex secTable /= 0xffff
                          then return (sectionNameIndex secTable)
                          else runGetOrThrow (gw32 hdr) (LBS.drop offLink0 bs)
      return (Just $ secTable
        { sectionEntryCount = entryCount'
        , sectionNameIndex  = entryNameIndex'
        })


-- | A section
data Section = Section
  { entryName :: ByteString   -- ^ Name of the section
  , entryBS   :: ByteString   -- ^ Content of the section
  }

-- | Read a ELF section
readElfSectionByIndex :: DynFlags
                      -> ElfHeader
                      -> SectionTable
                      -> Word64
                      -> ByteString
                      -> IO (Maybe Section)

readElfSectionByIndex dflags hdr secTable i bs = action `catchIO` \_ -> do
    debugTraceMsg dflags 3 $
      text ("Unable to read ELF section")
    return Nothing
  where
    -- read an entry from the section table
    getEntry = do
      nameIndex <- gw32 hdr
      skip (4+2*wordSize hdr)
      offset    <- fmap fromIntegral $ gwN hdr
      size      <- fmap fromIntegral $ gwN hdr
      let bs' = LBS.take size (LBS.drop offset bs)
      return (nameIndex,bs')

    -- read the entry with the given index in the section table
    getEntryByIndex x = runGetOrThrow getEntry bs'
      where
        bs' = LBS.drop off bs
        off = fromIntegral $ sectionTableOffset secTable +
                             x * fromIntegral (sectionEntrySize secTable)

    -- Get the name of a section
    getEntryName nameIndex = do
      let idx = fromIntegral (sectionNameIndex secTable)
      (_,nameTable) <- getEntryByIndex idx
      let bs' = LBS.drop nameIndex nameTable
      runGetOrThrow getLazyByteStringNul bs'

    action = do
      (nameIndex,bs') <- getEntryByIndex (fromIntegral i)
      name            <- getEntryName (fromIntegral nameIndex)
      return (Just $ Section name bs')


-- | Find a section from its name. Return the section contents.
--
-- We do not perform any check on the section type.
findSectionFromName :: DynFlags
                    -> ElfHeader
                    -> SectionTable
                    -> String
                    -> ByteString
                    -> IO (Maybe ByteString)
findSectionFromName dflags hdr secTable name bs =
    rec [0..sectionEntryCount secTable - 1]
  where
    -- convert the required section name into a ByteString to perform
    -- ByteString comparison instead of String comparison
    name' = B8.pack name

    -- compare recursively each section name and return the contents of
    -- the matching one, if any
    rec []     = return Nothing
    rec (x:xs) = do
      me <- readElfSectionByIndex dflags hdr secTable x bs
      case me of
        Just e | entryName e == name' -> return (Just (entryBS e))
        _                             -> rec xs


-- | Given a section name, read its contents as a ByteString.
--
-- If the section isn't found or if there is any parsing error, we return
-- Nothing
readElfSectionByName :: DynFlags
                     -> ByteString
                     -> String
                     -> IO (Maybe LBS.ByteString)

readElfSectionByName dflags bs name = action `catchIO` \_ -> do
    debugTraceMsg dflags 3 $
      text ("Unable to read ELF section \"" ++ name ++ "\"")
    return Nothing
  where
    action = runMaybeT $ do
      hdr      <- MaybeT $ readElfHeader dflags bs
      secTable <- MaybeT $ readElfSectionTable dflags hdr bs
      MaybeT $ findSectionFromName dflags hdr secTable name bs

------------------
-- NOTE SECTIONS
------------------

-- | read a Note as a ByteString
--
-- If you try to read a note from a section which does not support the Note
-- format, the parsing is likely to fail and Nothing will be returned
readElfNoteBS :: DynFlags
              -> ByteString
              -> String
              -> String
              -> IO (Maybe LBS.ByteString)

readElfNoteBS dflags bs sectionName noteId = action `catchIO`  \_ -> do
    debugTraceMsg dflags 3 $
         text ("Unable to read ELF note \"" ++ noteId ++
               "\" in section \"" ++ sectionName ++ "\"")
    return Nothing
  where
    -- align the getter on n bytes
    align n = do
      m <- bytesRead
      if m `mod` n == 0
        then return ()
        else skip 1 >> align n

    -- noteId as a bytestring
    noteId' = B8.pack noteId

    -- read notes recursively until the one with a valid identifier is found
    findNote hdr = do
      align 4
      namesz <- gw32 hdr
      descsz <- gw32 hdr
      _      <- gw32 hdr -- we don't use the note type
      name   <- if namesz == 0
                  then return LBS.empty
                  else getLazyByteStringNul
      align 4
      desc  <- if descsz == 0
                  then return LBS.empty
                  else getLazyByteString (fromIntegral descsz)
      if name == noteId'
        then return $ Just desc
        else findNote hdr


    action = runMaybeT $ do
      hdr  <- MaybeT $ readElfHeader dflags bs
      sec  <- MaybeT $ readElfSectionByName dflags bs sectionName
      MaybeT $ runGetOrThrow (findNote hdr) sec

-- | read a Note as a String
--
-- If you try to read a note from a section which does not support the Note
-- format, the parsing is likely to fail and Nothing will be returned
readElfNoteAsString :: DynFlags
                    -> FilePath
                    -> String
                    -> String
                    -> IO (Maybe String)

readElfNoteAsString dflags path sectionName noteId = action `catchIO`  \_ -> do
    debugTraceMsg dflags 3 $
         text ("Unable to read ELF note \"" ++ noteId ++
               "\" in section \"" ++ sectionName ++ "\"")
    return Nothing
  where
    action = do
      bs   <- LBS.readFile path
      note <- readElfNoteBS dflags bs sectionName noteId
      return (fmap B8.unpack note)


-- | Generate the GAS code to create a Note section
--
-- Header fields for notes are 32-bit long (see Note [ELF specification]).
--
-- It seems there is no easy way to force GNU AS to generate a 32-bit word in
-- every case. Hence we use .int directive to create them: however "The byte
-- order and bit size of the number depends on what kind of target the assembly
-- is for." (https://sourceware.org/binutils/docs/as/Int.html#Int)
--
-- If we add new target platforms, we need to check that the generated words
-- are 32-bit long, otherwise we need to use platform specific directives to
-- force 32-bit .int in asWord32.
makeElfNote :: String -> String -> Word32 -> String -> SDoc
makeElfNote sectionName noteName typ contents = hcat [
    text "\t.section ",
    text sectionName,
    text ",\"\",",
    sectionType "note",
    text "\n",

    -- note name length (+ 1 for ending \0)
    asWord32 (length noteName + 1),

    -- note contents size
    asWord32 (length contents),

    -- note type
    asWord32 typ,

    -- note name (.asciz for \0 ending string) + padding
    text "\t.asciz \"",
    text noteName,
    text "\"\n",
    text "\t.align 4\n",

    -- note contents (.ascii to avoid ending \0) + padding
    text "\t.ascii \"",
    text (escape contents),
    text "\"\n",
    text "\t.align 4\n"]
  where
    escape :: String -> String
    escape = concatMap (charToC.fromIntegral.ord)

    asWord32 :: Show a => a -> SDoc
    asWord32 x = hcat [
      text "\t.int ",
      text (show x),
      text "\n"]


------------------
-- Helpers
------------------

-- | runGet in IO monad that throws an IOException on failure
runGetOrThrow :: Get a -> LBS.ByteString -> IO a
runGetOrThrow g bs = case runGetOrFail g bs of
  Left _        -> fail "Error while reading file"
  Right (_,_,a) -> return a
