{- | This module provides a wrapper for I/O encoding for the "old" and "new" ways.
The "old" way uses iconv+utf8-string.
The "new" way uses the base library's built-in encoding functionality.
For the "new" way, we require ghc>=7.4.1 due to GHC bug #5436.

This module exports opaque Encoder/Decoder datatypes, along with several helper
functions that wrap the old/new ways.
-}
module System.Console.Haskeline.Backend.Posix.Encoder (
        ExternalHandle(eH),
        externalHandle,
        withCodingMode,
        openInCodingMode,
        ) where

import System.IO
import System.Console.Haskeline.Monads

import GHC.IO.Encoding (initLocaleEncoding)
import System.Console.Haskeline.Recover


-- | An 'ExternalHandle' is a handle which may or may not be in the correct
-- mode for Unicode input/output.  When the POSIX backend opens a file
-- (or /dev/tty) it sets it permanently to the correct mode.
-- However, when it uses an existing handle like stdin, it only temporarily
-- sets it to the correct mode (e.g., for the duration of getInputLine);
-- otherwise, we might interfere with the rest of the Haskell program.
--
-- The correct mode is the locale encoding, set to transliterate errors (rather
-- than crashing, as is the base library's default).  See Recover.hs.
data ExternalHandle = ExternalHandle
                        { externalMode :: ExternalMode
                        , eH :: Handle
                        }

data ExternalMode = CodingMode | OtherMode

externalHandle :: Handle -> ExternalHandle
externalHandle = ExternalHandle OtherMode

-- | Use to ensure that an external handle is in the correct mode
-- for the duration of the given action.
withCodingMode :: ExternalHandle -> IO a -> IO a
withCodingMode ExternalHandle {externalMode=CodingMode} act = act
withCodingMode (ExternalHandle OtherMode h) act = do
    bracket (liftIO $ hGetEncoding h)
            (liftIO . hSetBinOrEncoding h)
            $ const $ do
                hSetEncoding h haskelineEncoding
                act

hSetBinOrEncoding :: Handle -> Maybe TextEncoding -> IO ()
hSetBinOrEncoding h Nothing = hSetBinaryMode h True
hSetBinOrEncoding h (Just enc) = hSetEncoding h enc

haskelineEncoding :: TextEncoding
haskelineEncoding = transliterateFailure initLocaleEncoding

-- Open a file and permanently set it to the correct mode.
openInCodingMode :: FilePath -> IOMode -> IO ExternalHandle
openInCodingMode path iomode = do
    h <- openFile path iomode
    hSetEncoding h haskelineEncoding
    return $ ExternalHandle CodingMode h



