{-# LANGUAGE CPP, MagicHash #-}

module GHC.Exts.Heap.Utils (
    dataConNames
    ) where

#include "Rts.h"

import Prelude -- See note [Why do we import Prelude here?]
import GHC.Exts.Heap.Constants
import GHC.Exts.Heap.InfoTable

import Data.Char
import Data.List
import Foreign
import GHC.CString
import GHC.Exts

{- To find the string in the constructor's info table we need to consider
      the layout of info tables relative to the entry code for a closure.

      An info table can be next to the entry code for the closure, or it can
      be separate. The former (faster) is used in registerised versions of ghc,
      and the latter (portable) is for non-registerised versions.

      The diagrams below show where the string is to be found relative to
      the normal info table of the closure.

      1) Tables next to code:

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

      2) Tables NOT next to code:

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

-- Given a ptr to an 'StgInfoTable' for a data constructor
-- return (Package, Module, Name)
dataConNames :: Ptr StgInfoTable -> IO (String, String, String)
dataConNames ptr = do
    conDescAddress <- getConDescAddress
    pure $ parse conDescAddress
  where
    -- Retrieve the con_desc field address pointing to
    -- 'Package:Module.Name' string
    getConDescAddress :: IO (Ptr Word8)
    getConDescAddress
#if defined(TABLES_NEXT_TO_CODE)
      = do
        offsetToString <- peek (ptr `plusPtr` negate wORD_SIZE)
        pure $ (ptr `plusPtr` stdInfoTableSizeB)
                    `plusPtr` fromIntegral (offsetToString :: Int32)
#else
      = peek $ intPtrToPtr $ ptrToIntPtr ptr + fromIntegral stdInfoTableSizeB
#endif

    stdInfoTableSizeW :: Int
    -- The size of a standard info table varies with profiling/ticky etc,
    -- so we can't get it from Constants
    -- It must vary in sync with mkStdInfoTable
    stdInfoTableSizeW
      = size_fixed + size_prof
      where
        size_fixed = 2  -- layout, type
##if defined(PROFILING)
        size_prof = 2
##else
        size_prof = 0
##endif

    stdInfoTableSizeB :: Int
    stdInfoTableSizeB = stdInfoTableSizeW * wORD_SIZE

-- parsing names is a little bit fiddly because we have a string in the form:
-- pkg:A.B.C.foo, and we want to split it into three parts: ("pkg", "A.B.C", "foo").
-- Thus we split at the leftmost colon and the rightmost occurrence of the dot.
-- It would be easier if the string was in the form pkg:A.B.C:foo, but alas
-- this is not the conventional way of writing Haskell names. We stick with
-- convention, even though it makes the parsing code more troublesome.
-- Warning: this code assumes that the string is well formed.
parse :: Ptr Word8 -> (String, String, String)
parse (Ptr addr) = if not . all (>0) . fmap length $ [p,m,occ]
                     then ([], [], input)
                     else (p, m, occ)
  where
    input = unpackCStringUtf8## addr
    (p, rest1) = break (== ':') input
    (m, occ)
        = (intercalate "." $ reverse modWords, occWord)
        where
        (modWords, occWord) =
            if length rest1 < 1 --  XXXXXXXXx YUKX
                --then error "getConDescAddress:parse:length rest1 < 1"
                then parseModOcc [] []
                else parseModOcc [] (tail rest1)
    -- We only look for dots if str could start with a module name,
    -- i.e. if it starts with an upper case character.
    -- Otherwise we might think that "X.:->" is the module name in
    -- "X.:->.+", whereas actually "X" is the module name and
    -- ":->.+" is a constructor name.
    parseModOcc :: [String] -> String -> ([String], String)
    parseModOcc acc str@(c : _)
        | isUpper c =
            case break (== '.') str of
                (top, []) -> (acc, top)
                (top, _:bot) -> parseModOcc (top : acc) bot
    parseModOcc acc str = (acc, str)
