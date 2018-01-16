module HaskelineExport where

import System.Console.Haskeline
import System.Console.Haskeline.IO

import Foreign
import Foreign.C.String

foreign export ccall initialize_input :: IO (StablePtr InputState)

foreign export ccall close_input :: StablePtr InputState -> IO ()

foreign export ccall cancel_input :: StablePtr InputState -> IO ()

foreign export ccall get_input_line :: StablePtr InputState -> CString
                                        -> IO CString

-- TODO: allocate string results with the malloc from stdlib 
-- so that c code can free it.

initialize_input = do
    hd <- initializeInput defaultSettings
    newStablePtr hd


close_input sptr = do
    hd <- deRefStablePtr sptr
    closeInput hd
    freeStablePtr sptr

cancel_input sptr = do
    hd <- deRefStablePtr sptr
    cancelInput hd
    freeStablePtr sptr


get_input_line sptr c_prefix = do
    hd <- deRefStablePtr sptr
    prefix <- peekCString c_prefix
    result <- queryInput hd (getInputLine prefix)
    case result of
        Nothing -> return nullPtr
        Just str -> newCString str

