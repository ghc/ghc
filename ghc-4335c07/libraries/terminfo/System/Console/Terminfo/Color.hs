{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 703
{-# LANGUAGE Safe #-}
#endif
-- |
-- Maintainer  : judah.jacobson@gmail.com
-- Stability   : experimental
-- Portability : portable (FFI)
module System.Console.Terminfo.Color(
                    termColors,
                    Color(..),
                    -- ColorPair,
                    withForegroundColor,
                    withBackgroundColor,
                    -- withColorPair,
                    setForegroundColor,
                    setBackgroundColor,
                    -- setColorPair,
                    restoreDefaultColors
                    ) where

import System.Console.Terminfo.Base
import Control.Monad (mplus)

-- TODOs:
-- examples
-- try with xterm-256-colors (?)
-- Color pairs, and HP terminals.
-- TODO: this "white" looks more like a grey.  (What does ncurses do?)

-- NB: for all the terminals in ncurses' terminfo.src, colors>=8 when it's
-- set.  So we don't need to perform that check.

-- | The maximum number of of colors on the screen.
termColors :: Capability Int
termColors = tiGetNum "colors"

data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan
            | White | ColorNumber Int
        deriving (Show,Eq,Ord)



colorIntA, colorInt :: Color -> Int
colorIntA c = case c of
    Black -> 0
    Red -> 1
    Green -> 2
    Yellow -> 3
    Blue -> 4
    Magenta -> 5
    Cyan -> 6
    White -> 7
    ColorNumber n -> n
colorInt c = case c of
    Black -> 0
    Blue -> 1
    Green -> 2
    Cyan -> 3
    Red -> 4
    Magenta -> 5
    Yellow -> 6
    White -> 7
    ColorNumber n -> n


-- NB these aren't available on HP systems.
-- also do we want to handle case when they're not available?

-- | This capability temporarily sets the
-- terminal's foreground color while outputting the given text, and
-- then restores the terminal to its default foreground and background
-- colors.
withForegroundColor :: TermStr s => Capability (Color -> s -> s)
withForegroundColor = withColorCmd setForegroundColor

-- | This capability temporarily sets the
-- terminal's background color while outputting the given text, and
-- then restores the terminal to its default foreground and background
-- colors.
withBackgroundColor :: TermStr s => Capability (Color -> s -> s)
withBackgroundColor = withColorCmd setBackgroundColor

withColorCmd :: TermStr s => Capability (a -> s)
            -> Capability (a -> s -> s)
withColorCmd getSet = do
    set <- getSet
    restore <- restoreDefaultColors
    return $ \c t -> set c <#> t <#> restore

-- | Sets the foreground color of all further text output, using
-- either the @setaf@ or @setf@ capability.
setForegroundColor :: TermStr s => Capability (Color -> s)
setForegroundColor = setaf `mplus` setf
    where
        setaf = fmap (. colorIntA) $ tiGetOutput1 "setaf"
        setf = fmap (. colorInt) $ tiGetOutput1 "setf"

-- | Sets the background color of all further text output, using
-- either the @setab@ or @setb@ capability.
setBackgroundColor :: TermStr s => Capability (Color -> s)
setBackgroundColor = setab `mplus` setb
    where
        setab = fmap (. colorIntA) $ tiGetOutput1 "setab"
        setb = fmap (. colorInt) $ tiGetOutput1 "setb"

{-
withColorPair :: TermStr s => Capability (ColorPair -> s -> s)
withColorPair = withColorCmd setColorPair

setColorPair :: TermStr s => Capability (ColorPair -> s)
setColorPair = do
    setf <- setForegroundColor
    setb <- setBackgroundColor
    return (\(f,b) -> setf f <#> setb b)

type ColorPair = (Color,Color)
-}  


-- | Restores foreground/background colors to their original
-- settings.
restoreDefaultColors :: TermStr s => Capability s 
restoreDefaultColors = tiGetOutput1 "op"
