> module MGRlib 

	Module that acts as interface to MGR window manager. 
	Window manager functions by picking up escaped command
	from standard output.

>		(setTextRegion, textReset, clear, clearEvent, func, 
> 		 line, setCursor, setEvent, setMode, shapeWindow,
> 		 stringTo, printOver, movePrintTo,writeVert) 

> where


	Function generalises command formats
	
> command :: String -> [Int] -> String
> command str ns = ('\ESC':foldr f "" ns)
> 		where f n "" = show n ++ str
> 		      f n s  = show n ++ "," ++ s

	

	setTextRegion : define a text Region by its origin height and width

> setTextRegion :: [Int] -> String
> setTextRegion = command "t" 

	textReset : clear textRegion

> textReset :: String
> textReset = ('\ESC':"t")


	clear : clears the current text Region

> clear :: String
> clear = "\^L"


	clearEvent : clears the string corresponding to events given.

> clearEvent event = command "e" [mapEvent event]


	mapEvent : used to stop the up and down of the button registering
		2 events

> mapEvent event = if ((event == 3) || (event == 4)) then (2-event) else event


	func : sets up the drawing mode for graphics and bit_blt
		operations.
		func 4 = invertMode 
		func 0 = deleteMode
		func 15 = insertMode 

> func :: Int -> String
> func mode = command "b" [mode]

	
	line : draw a line from (x0,y0) to (x1,y1)

> line :: [Int] -> String
> line = command "l"   -- x0 y0 x1 y1


	setCursor : sets the cursor style
		setcursor 0 = Normal
		setcursor 7 = Off

> setCursor n = command "h" [n]


	setEvent : Associate a string with an event. When the event
		is triggered the string is placed on the  standard input

> setEvent event str = command ("e"++str) [mapEvent event, length str]


	setMode : set up various window modes
		setMode 1 = Standard screen colour orientation
		setMode 2 = White on black
		setMode 7 = overwrite
		etc..

> setMode mode = command "S" [mode]


	shapeWindow : Place the window at (x,y) with width w and height h

> shapeWindow :: [Int] -> String
> shapeWindow = command "W" -- x y w h

	stringTo : Places text at x y on window win. Merges
		with anything there

> stringTo :: Int -> Int -> Int -> String -> String
> stringTo win x y str = command ("."++str) [win,x,y,length str]


	printOver : Prints over existing character
	
> printOver :: Int -> Int -> String -> String
> printOver w h str = concat [func 0, stringTo 0 w h (take (length str) spaces),func 4,stringTo 0 w h str,func 15]
>		where
> 		spaces = ' ':spaces

        movePrintTo : Puts print in a text Region

> movePrintTo :: Int -> Int -> String -> String
> movePrintTo w h str = concat [func 4, stringTo 0 w h str, func 15]

        writeVert: Print a String vertically downwards.

> writeVert :: (Int,Int) -> [String] -> String
> writeVert (x,y) [] = []
> writeVert (x,y) (a:l) = printOver x y a ++ writeVert (x,(y+11)) l
