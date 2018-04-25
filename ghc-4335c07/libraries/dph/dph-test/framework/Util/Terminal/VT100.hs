
-- | Generation of VT100 terminal control codes.
module Util.Terminal.VT100 
	( Color(..),		codeOfColor
	, Mode (..),		codeOfMode
	, ModeState (..),	modeStateInit, applyMode
	
	, eraseDisplay
	, eraseLine
	, setCursorPos
	, moveCursorUp
	, moveCursorDown
	, moveCursorLeft
	, moveCursorRight
	, saveCursorPosition
	, restoreCursorPosition
	, setMode )
where

import Data.List

-- Color ----------------------------------------------------------------------------------------
-- | VT100 / ANSI colors
data Color
	= Black
	| Red
	| Green
	| Yellow
	| Blue
	| Magenta
	| Cyan
	| White
	deriving (Show, Eq, Ord)

-- | Control code offset for rendering colors.
--	Need to add 30 for foreground, 40 for background to get real code.
codeOfColor :: Color -> Int
codeOfColor color
 = case color of
	Black	-> 0
	Red	-> 1
	Green	-> 2
	Yellow	-> 3
	Blue	-> 4
	Magenta	-> 5
	Cyan	-> 6
	White	-> 7
	

-- Mode ----------------------------------------------------------------------------------------
-- | Rendering mode commands.
data Mode
	= Reset
	| Bold
	| Dim
	| Underscore
	| Blink
	| Reverse
	| Concealed
	| Foreground Color
	| Background Color
	deriving (Show, Eq, Ord)


-- | Control codes for rendering mode commands.
codeOfMode :: Mode -> Int
codeOfMode mode
 = case mode of
	Reset			-> 0
	Bold			-> 1
	Dim			-> 3
	Underscore		-> 4
	Blink			-> 5
	Reverse			-> 7
	Concealed		-> 8
	Foreground color	-> 30 + codeOfColor color
	Background color	-> 40 + codeOfColor color


-- ModeState ------------------------------------------------------------------------------------
-- Model of VT100 terminal state, to help keep track what state
--	the terminal is in after accepting a string of commands.

data ModeState
	= ModeState
	{ sBold		:: Bool
	, sDim		:: Bool
	, sUnderscore	:: Bool
	, sBlink	:: Bool
	, sReverse	:: Bool
	, sConcealed	:: Bool
	, sForeground	:: Color
	, sBackground	:: Color }
	deriving (Show, Eq)
	
modeStateInit
	= ModeState
	{ sBold		= False
	, sDim		= False
	, sUnderscore	= False
	, sBlink	= False
	, sReverse	= False
	, sConcealed	= False
	, sForeground	= White
	, sBackground	= Black }


applyMode :: ModeState -> Mode -> ModeState
applyMode state m
 = case m of
	Reset		-> modeStateInit
	Bold		-> state { sBold 	= True }
	Dim		-> state { sDim		= True }
	Underscore	-> state { sUnderscore	= True }
	Blink		-> state { sBlink	= True }
	Reverse		-> state { sReverse	= True }
	Concealed	-> state { sConcealed	= True }
	Foreground c	-> state { sForeground	= c }
	Background c	-> state { sBackground	= c }


-- Commands -------------------------------------------------------------------------------------

-- | Erase entire display.
eraseDisplay		= "\27[2J"

-- | Erase current line of display.
eraseLine		= "\27[K"

-- | Set the position of the cursor.
setCursorPos x y	= "\27[" ++ show y ++ ";" ++ show x ++ "H"

-- | Move cursor up some number of lines.
moveCursorUp n		= "\27[" ++ show n ++ "A"

-- | Move cursor down some number of lines.
moveCursorDown n	= "\27[" ++ show n ++ "B"

-- | Move cursor right / forward some number of columns.
moveCursorRight n	= "\27[" ++ show n ++ "C"

-- | Move cursor left / backward some number of columns.
moveCursorLeft n	= "\27[" ++ show n ++ "D"

-- | Save cursor position 
saveCursorPosition 	= "\27[s"

-- | Restore cursor position
restoreCursorPosition	= "\27[u"

-- | Set graphics mode
setMode modes
	= "\27[" 
	++ concat 
		(intersperse ";" 
		$ map (show . codeOfMode) modes) 
	++ "m"
	
