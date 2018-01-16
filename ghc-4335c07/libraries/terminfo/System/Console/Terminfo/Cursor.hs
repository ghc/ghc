{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 703
{-# LANGUAGE Safe #-}
#endif
-- |
-- Maintainer  : judah.jacobson@gmail.com
-- Stability   : experimental
-- Portability : portable (FFI)
--
-- This module provides capabilities for moving the cursor on the terminal.
module System.Console.Terminfo.Cursor(
                        -- * Terminal dimensions
                        -- | Get the default size of the terminal.  For
                        -- resizeable terminals (e.g., @xterm@), these may not
                        -- correspond to the actual dimensions.
                        termLines, termColumns,
                        -- * Cursor flags
                        autoRightMargin,
                        autoLeftMargin,
                        wraparoundGlitch,
                        -- * Scrolling
                        carriageReturn,
                        newline,
                        scrollForward,
                        scrollReverse,
                        -- * Relative cursor movements
                        -- | The following functions for cursor movement will
                        -- combine the more primitive capabilities.  For example,
                        -- 'moveDown' may use either 'cursorDown' or
                        -- 'cursorDown1' depending on the parameter and which of
                        -- @cud@ and @cud1@ are defined.
                        moveDown, moveLeft, moveRight, moveUp,
                        
                        -- ** Primitive movement capabilities
                        -- | These capabilities correspond directly to @cub@, @cud@,
                        -- @cub1@, @cud1@, etc.
                        cursorDown1, 
                        cursorLeft1,
                        cursorRight1,
                        cursorUp1, 
                        cursorDown, 
                        cursorLeft,
                        cursorRight,
                        cursorUp, 
                        cursorHome,
                        cursorToLL,
                        -- * Absolute cursor movements
                        cursorAddress,
                        Point(..),
                        rowAddress,
                        columnAddress
                        ) where

import System.Console.Terminfo.Base
import Control.Monad

termLines :: Capability Int
termColumns :: Capability Int
termLines = tiGetNum "lines"
termColumns = tiGetNum "cols"

-- | This flag specifies that the cursor wraps automatically from the last 
-- column of one line to the first column of the next.
autoRightMargin :: Capability Bool
autoRightMargin = tiGetFlag "am"

-- | This flag specifies that a backspace at column 0 wraps the cursor to
-- the last column of the previous line.
autoLeftMargin :: Capability Bool
autoLeftMargin = tiGetFlag "bw"

-- | This flag specifies that the terminal does not perform
-- 'autoRightMargin'-style wrapping when the character which would cause the 
-- wraparound is a control character.
-- This is also known as the \"newline glitch\" or \"magic wrap\".  
-- 
-- For example, in an 80-column terminal with this behavior, the following 
-- will print single-spaced instead of double-spaced:
-- 
-- > replicateM_ 5 $ putStr $ replicate 80 'x' ++ "\n"
-- 
wraparoundGlitch :: Capability Bool
wraparoundGlitch = tiGetFlag "xenl"

{--
On many terminals, the @cud1@ ('cursorDown1') capability is the line feed 
character '\n'.  However, @stty@ settings may cause that character to have
other effects than intended; e.g. ONLCR turns LF into CRLF, and as a result 
@cud1@ will always move the cursor to the first column of the next line.  

Looking at the source code of curses (lib_mvcur.c) and other similar programs, 
they use @cud@ instead of @cud1@ if it's '\n' and ONLCR is turned on.  

Since there's no easy way to check for ONLCR at this point, I've just made
moveDown only use cud1 if it's not '\n'.
Suggestions are welcome.
--}
cursorDown1Fixed :: TermStr s => Capability s
cursorDown1Fixed = do
    str <- tiGetOutput1 "cud1"
    guard (str /= "\n")
    tiGetOutput1 "cud1"

cursorDown1 :: TermStr s => Capability s
cursorDown1 = tiGetOutput1 "cud1"

cursorLeft1 :: TermStr s => Capability s
cursorLeft1 = tiGetOutput1 "cub1"

cursorRight1 :: TermStr s => Capability s
cursorRight1 = tiGetOutput1 "cuf1"

cursorUp1 :: TermStr s => Capability s
cursorUp1 = tiGetOutput1 "cuu1"

cursorDown :: TermStr s => Capability (Int -> s)
cursorDown = tiGetOutput1 "cud"

cursorLeft :: TermStr s => Capability (Int -> s)
cursorLeft = tiGetOutput1 "cub"

cursorRight :: TermStr s => Capability (Int -> s)
cursorRight = tiGetOutput1 "cuf"

cursorUp :: TermStr s => Capability (Int -> s)
cursorUp = tiGetOutput1 "cuu"

cursorHome :: TermStr s => Capability s
cursorHome = tiGetOutput1 "home"

cursorToLL :: TermStr s => Capability s
cursorToLL = tiGetOutput1 "ll"


-- Movements are built out of parametrized and unparam'd movement
-- capabilities.
-- todo: more complicated logic like ncurses does.
move :: TermStr s => Capability s -> Capability (Int -> s)
                              -> Capability (Int -> s)
move single param = let
        tryBoth = do
                    s <- single
                    p <- param
                    return $ \n -> case n of
                        0 -> mempty
                        1 -> s
                        _ -> p n
        manySingle = do
                        s <- single
                        return $ \n -> mconcat $ replicate n s
        in tryBoth `mplus` param `mplus` manySingle

moveLeft :: TermStr s => Capability (Int -> s)
moveLeft = move cursorLeft1 cursorLeft

moveRight :: TermStr s => Capability (Int -> s)
moveRight = move cursorRight1 cursorRight

moveUp :: TermStr s => Capability (Int -> s)
moveUp = move cursorUp1 cursorUp

moveDown :: TermStr s => Capability (Int -> s)
moveDown = move cursorDown1Fixed cursorDown

-- | The @cr@ capability, which moves the cursor to the first column of the
-- current line.
carriageReturn :: TermStr s => Capability s
carriageReturn = tiGetOutput1 "cr"

-- | The @nel@ capability, which moves the cursor to the first column of
-- the next line.  It behaves like a carriage return followed by a line feed.
--
-- If @nel@ is not defined, this may be built out of other capabilities.
newline :: TermStr s => Capability s
newline = tiGetOutput1 "nel" 
    `mplus` (liftM2 mappend carriageReturn 
                            (scrollForward `mplus` tiGetOutput1 "cud1"))
        -- Note it's OK to use cud1 here, despite the stty problem referenced 
        -- above, because carriageReturn already puts us on the first column.

scrollForward :: TermStr s => Capability s
scrollForward = tiGetOutput1 "ind"

scrollReverse :: TermStr s => Capability s
scrollReverse = tiGetOutput1 "ri"


data Point = Point {row, col :: Int}

cursorAddress :: TermStr s => Capability (Point -> s)
cursorAddress = fmap (\g p -> g (row p) (col p)) $ tiGetOutput1 "cup"

columnAddress :: TermStr s => Capability (Int -> s)
columnAddress = tiGetOutput1 "hpa"

rowAddress :: TermStr s => Capability (Int -> s)
rowAddress = tiGetOutput1 "vpa"


