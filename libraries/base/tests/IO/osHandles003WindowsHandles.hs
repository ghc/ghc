import Control.Monad (zipWithM_)
import Data.Functor (void)
import Data.Char (ord)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Storable (pokeElemOff)
import System.IO (stdin, stdout, stderr)
import System.IO.OS
       (
           withWindowsHandleReadingBiased,
           withWindowsHandleWritingBiased
       )

main :: IO ()
main = withWindowsHandleReadingBiased stdin  $ \ windowsStdin  ->
       withWindowsHandleWritingBiased stdout $ \ windowsStdout ->
       withWindowsHandleWritingBiased stderr $ \ windowsStderr ->
       do
           withBuffer inputSizeApproximation $ \ bufferPtr -> do
               inputSize <- win32_ReadFile windowsStdin
                                           bufferPtr
                                           inputSizeApproximation
                                           Nothing
               void $ win32_WriteFile windowsStdout
                                      bufferPtr
                                      inputSize
                                      Nothing
           withBuffer errorMsgSize $ \ bufferPtr -> do
               zipWithM_ (pokeElemOff bufferPtr)
                         [0 ..]
                         (map (fromIntegral . ord) errorMsg)
               void $ win32_WriteFile windowsStderr
                                      bufferPtr
                                      errorMsgSize
                                      Nothing
    where

    withBuffer :: DWORD -> (Ptr Word8 -> IO a) -> IO a
    withBuffer = allocaBytes . fromIntegral

    inputSizeApproximation :: DWORD
    inputSizeApproximation = 100

    errorMsg :: String
    errorMsg = "And every single door\n\
               \That I've walked through\n\
               \Brings me back, back here again\n"

    errorMsgSize :: DWORD
    errorMsgSize = fromIntegral (length errorMsg)
