import Data.Functor (void)
import Data.ByteString.Char8 (pack)
import System.Posix.Types (Fd (Fd), ByteCount)
import System.Posix.IO.ByteString (fdRead, fdWrite)
import System.IO (stdin, stdout, stderr)
import System.IO.OS
       (
           withFileDescriptorReadingBiased,
           withFileDescriptorWritingBiased
       )

main :: IO ()
main = withFileDescriptorReadingBiased stdin  $ \ stdinFD  ->
       withFileDescriptorWritingBiased stdout $ \ stdoutFD ->
       withFileDescriptorWritingBiased stderr $ \ stderrFD ->
       do
           regularMsg <- fdRead (Fd stdinFD) inputSizeApproximation
           void $ fdWrite (Fd stdoutFD) regularMsg
           void $ fdWrite (Fd stderrFD) (pack errorMsg)
    where

    inputSizeApproximation :: ByteCount
    inputSizeApproximation = 100

    errorMsg :: String
    errorMsg = "And every single door\n\
               \That I've walked through\n\
               \Brings me back, back here again\n"
