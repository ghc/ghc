{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

-- -----------------------------------------------------------------------------
-- | GHC LLVM Mangler
-- 
-- This script processes the assembly produced by LLVM, rearranging the code
-- so that an info table appears before its corresponding function.
module LlvmMangler ( llvmFixupAsm ) where

import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS

{-
  Configuration.
-}
newSection, oldSection, functionSuf, tableSuf, funDivider, eol :: ByteString
newSection  = BS.pack "\n.text\n"
oldSection  = BS.pack "__STRIP,__me"
functionSuf = BS.pack "_info:"
tableSuf    = BS.pack "_info_itable:"
funDivider  = BS.pack "\n\n"
eol         = BS.pack "\n"

eolPred :: Char -> Bool
eolPred = ((==) '\n')

-- | Read in assembly file and process
llvmFixupAsm :: FilePath -> FilePath -> IO ()
llvmFixupAsm f1 f2 = do
    asm <- BS.readFile f1
    BS.writeFile f2 BS.empty
    allTables f2 asm
    return ()

-- | Run over whole assembly file
allTables :: FilePath -> ByteString -> IO ()
allTables f str = do
    rem <- oneTable f str
    if BS.null rem
       then return ()
       else allTables f rem

{- |
  Look for the next function that needs to have its info table
  arranged to be before it and process it. This will print out
  any code before this function, then the info table, then the
  function. It will return the remainder of the assembly code
  to process.
 
  We rely here on the fact that LLVM prints all global variables
  at the end of the file, so an info table will always appear
  after its function.
  
  To try to help explain the string searches, here is some
  assembly code that would be processed by this program, with
  split markers placed in it like so, <split marker>:

    [ ...asm code... ]
    jmp *%eax
    <before|fheader>
    .def Main_main_info
    .section TEXT
    .globl _Main_main_info
    _Main_main<bl|al>_info:
        sub $12, %esp
        [ ...asm code... ]
        jmp *%eax
    <fun|after>
    .def .....

    [ ...asm code... ]

        .long 231231
    <bit'|itable_h>
    .section TEXT
    .global _Main_main_entry
    .align 4
    <bit|itable>_Main_main_entry:
        .long 0
        [ ...asm code... ]
    <itable'|ait>
    .section TEXT
-}
oneTable :: FilePath -> ByteString -> IO ByteString
oneTable f str =
    let last' xs = if (null xs) then 0 else last xs
        
        -- get the function
        (bl, al) = BS.breakSubstring functionSuf str
        start = last' $ BS.findSubstrings funDivider bl
        (before, fheader) = BS.splitAt start bl
        (fun, after) = BS.breakSubstring funDivider al
        label = snd $ BS.breakEnd eolPred bl

        -- get the info table
        ilabel = label `BS.append` tableSuf
        (bit, itable) = BS.breakSubstring ilabel after
        (itable', ait) = BS.breakSubstring funDivider itable
        istart = last' $ BS.findSubstrings funDivider bit
        (bit', iheader) = BS.splitAt istart bit

        -- fix up sections
        fheader' = replaceSection fheader
        iheader' = replaceSection iheader

        function = [before, eol, iheader', itable', eol, fheader', fun, eol]
        remainder = bit' `BS.append` ait
    in if BS.null al
          then do 
              BS.appendFile f bl
              return BS.empty

          else if BS.null itable
                  then error $ "Function without matching info table! ("
                              ++ (BS.unpack label) ++ ")"

                  else do
                      mapM_ (BS.appendFile f) function
                      return remainder

-- | Replace the current section in a function or table header with the
-- text section specifier.
replaceSection :: ByteString -> ByteString
replaceSection sec =
    let (s1, s2) = BS.breakSubstring oldSection sec
        s1' = fst $ BS.breakEnd eolPred s1
        s2' = snd $ BS.break eolPred s2
    in s1' `BS.append` newSection `BS.append` s2'

