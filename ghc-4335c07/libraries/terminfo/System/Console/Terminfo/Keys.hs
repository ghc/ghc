{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 703
{-# LANGUAGE Safe #-}
#endif
-- |
-- Maintainer  : judah.jacobson@gmail.com
-- Stability   : experimental
-- Portability : portable (FFI)
--
-- The string capabilities in this module are the character sequences
-- corresponding to user input such as arrow keys and function keys.
module System.Console.Terminfo.Keys(
                    -- * The keypad
                    -- | The following commands
                    -- turn the keypad on\/off (@smkx@ and @rmkx@).  
                    -- They have no effect if those capabilities are not defined.  
                    -- For portability between terminals, the keypad should be
                    -- explicitly turned on before accepting user key input.
                    keypadOn,
                    keypadOff,
                    -- * Arrow keys
                    keyUp,
                    keyDown,
                    keyLeft,
                    keyRight,
                    -- * Miscellaneous
                    functionKey,
                    keyBackspace,
                    keyDeleteChar,
                    keyHome,
                    keyEnd,
                    keyPageUp,
                    keyPageDown,
                    keyEnter,
                    ) where

import System.Console.Terminfo.Base

keypadOn :: TermStr s => Capability s
keypadOn = tiGetOutput1 "smkx"

keypadOff :: TermStr s => Capability s
keypadOff = tiGetOutput1 "rmkx"

keyUp :: Capability String
keyUp = tiGetOutput1 "kcuu1"

keyDown :: Capability String
keyDown = tiGetOutput1 "kcud1"

keyLeft :: Capability String
keyLeft = tiGetOutput1 "kcub1"

keyRight :: Capability String
keyRight = tiGetOutput1 "kcuf1"

-- | Look up the control sequence for a given function sequence.  For example, 
-- @functionKey 12@ retrieves the @kf12@ capability.
functionKey :: Int -> Capability String
functionKey n = tiGetOutput1 ("kf" ++ show n)

keyBackspace :: Capability String
keyBackspace = tiGetOutput1 "kbs"

keyDeleteChar :: Capability String
keyDeleteChar = tiGetOutput1 "kdch1"

keyHome :: Capability String
keyHome = tiGetOutput1 "khome"

keyEnd :: Capability String
keyEnd = tiGetOutput1 "kend"

keyPageUp :: Capability String
keyPageUp = tiGetOutput1 "kpp"

keyPageDown :: Capability String
keyPageDown = tiGetOutput1 "knp"

keyEnter :: Capability String
keyEnter = tiGetOutput1 "kent"
