{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 703
{-# LANGUAGE Safe #-}
#endif
-- |
-- Maintainer  : judah.jacobson@gmail.com
-- Stability   : experimental
-- Portability : portable (FFI)
module System.Console.Terminfo.Edit where

import System.Console.Terminfo.Base

-- | Clear the screen, and move the cursor to the upper left.
clearScreen :: Capability (LinesAffected -> TermOutput)
clearScreen = fmap ($ []) $ tiGetOutput "clear" 

-- | Clear from beginning of line to cursor.
clearBOL :: TermStr s => Capability s
clearBOL = tiGetOutput1 "el1"

-- | Clear from cursor to end of line.
clearEOL :: TermStr s => Capability s
clearEOL = tiGetOutput1 "el"

-- | Clear display after cursor.
clearEOS :: Capability (LinesAffected -> TermOutput)
clearEOS = fmap ($ []) $ tiGetOutput "ed"

