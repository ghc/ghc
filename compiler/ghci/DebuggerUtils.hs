{-# LANGUAGE CPP #-}

module DebuggerUtils (
       dataConInfoPtrToName,
  ) where

import CmmInfo ( stdInfoTableSizeB )
import ByteCodeItbls
import DynFlags
import FastString
import TcRnTypes
import TcRnMonad
import IfaceEnv
import Module
import OccName
import Name
import Outputable
import Platform
import Util

import Data.Char
import Foreign
import Data.List

#include "HsVersions.h"

-- | Given a data constructor in the heap, find its Name.
--   The info tables for data constructors have a field which records
--   the source name of the constructor as a Ptr Word8 (UTF-8 encoded
--   string). The format is:
--
--   > Package:Module.Name
--
--   We use this string to lookup the interpreter's internal representation of the name
--   using the lookupOrig.    
--
dataConInfoPtrToName :: Ptr () -> TcM (Either String Name)
dataConInfoPtrToName x = do 
   dflags <- getDynFlags
   theString <- liftIO $ do
      let ptr = castPtr x :: Ptr StgInfoTable
      conDescAddress <- getConDescAddress dflags ptr
      peekArray0 0 conDescAddress  
   let (pkg, mod, occ) = parse theString 
       pkgFS = mkFastStringByteList pkg
       modFS = mkFastStringByteList mod
       occFS = mkFastStringByteList occ
       occName = mkOccNameFS OccName.dataName occFS
       modName = mkModule (fsToPackageId pkgFS) (mkModuleNameFS modFS) 
   return (Left $ showSDoc dflags $ ppr modName <> dot <> ppr occName)
    `recoverM` (Right `fmap` lookupOrig modName occName)

   where

   {- To find the string in the constructor's info table we need to consider 
      the layout of info tables relative to the entry code for a closure.

      An info table can be next to the entry code for the closure, or it can
      be separate. The former (faster) is used in registerised versions of ghc, 
      and the latter (portable) is for non-registerised versions. 

      The diagrams below show where the string is to be found relative to 
      the normal info table of the closure.

      1) Code next to table:

         --------------
         |            |   <- pointer to the start of the string
         --------------
         |            |   <- the (start of the) info table structure
         |            |
         |            |
         --------------
         | entry code | 
         |    ....    |

         In this case the pointer to the start of the string can be found in
         the memory location _one word before_ the first entry in the normal info 
         table.

      2) Code NOT next to table:

                                 --------------
         info table structure -> |     *------------------> --------------
                                 |            |             | entry code |
                                 |            |             |    ....    | 
                                 --------------
         ptr to start of str ->  |            |   
                                 --------------

         In this case the pointer to the start of the string can be found
         in the memory location: info_table_ptr + info_table_size
   -}

   getConDescAddress :: DynFlags -> Ptr StgInfoTable -> IO (Ptr Word8)
   getConDescAddress dflags ptr
    | ghciTablesNextToCode = do
       let ptr' = ptr `plusPtr` (- wORD_SIZE dflags)
       -- offsetToString is really an StgWord, but we have to jump
       -- through some hoops due to the way that our StgWord Haskell
       -- type is the same on 32 and 64bit platforms
       offsetToString <- case platformWordSize (targetPlatform dflags) of
                         4 -> do w <- peek ptr'
                                 return (fromIntegral (w :: Word32))
                         8 -> do w <- peek ptr'
                                 return (fromIntegral (w :: Word64))
                         w -> panic ("getConDescAddress: Unknown platformWordSize: " ++ show w)
       return $ (ptr `plusPtr` stdInfoTableSizeB dflags) `plusPtr` offsetToString
    | otherwise =
       peek $ intPtrToPtr $ ptrToIntPtr ptr + fromIntegral (stdInfoTableSizeB dflags)
   -- parsing names is a little bit fiddly because we have a string in the form: 
   -- pkg:A.B.C.foo, and we want to split it into three parts: ("pkg", "A.B.C", "foo").
   -- Thus we split at the leftmost colon and the rightmost occurrence of the dot.
   -- It would be easier if the string was in the form pkg:A.B.C:foo, but alas
   -- this is not the conventional way of writing Haskell names. We stick with
   -- convention, even though it makes the parsing code more troublesome.
   -- Warning: this code assumes that the string is well formed.
   parse :: [Word8] -> ([Word8], [Word8], [Word8])
   parse input 
      = ASSERT(all (>0) (map length [pkg, mod, occ])) (pkg, mod, occ)
      where
      dot = fromIntegral (ord '.')
      (pkg, rest1) = break (== fromIntegral (ord ':')) input 
      (mod, occ) 
         = (concat $ intersperse [dot] $ reverse modWords, occWord)
         where
         (modWords, occWord) = ASSERT(length rest1 > 0) (parseModOcc [] (tail rest1))
      parseModOcc :: [[Word8]] -> [Word8] -> ([[Word8]], [Word8])
      -- We only look for dots if str could start with a module name,
      -- i.e. if it starts with an upper case character.
      -- Otherwise we might think that "X.:->" is the module name in
      -- "X.:->.+", whereas actually "X" is the module name and
      -- ":->.+" is a constructor name.
      parseModOcc acc str@(c : _)
       | isUpper $ chr $ fromIntegral c
         = case break (== dot) str of
              (top, []) -> (acc, top)
              (top, _ : bot) -> parseModOcc (top : acc) bot
      parseModOcc acc str = (acc, str)
