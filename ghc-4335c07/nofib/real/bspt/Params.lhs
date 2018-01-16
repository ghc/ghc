> module Params where

	Module defines the Parameters that define the interface
	of the GMS.


	Command is a data type that enumerates the possible actions
	the user may select (along with the Null command)

> data Command = Polygon | Union | Intersect | Subtract | Complement | Partition 
>                 | Render | Classify | Area | Quit | Null deriving (Eq)

	Define String as a list of Characters
	
> --partain: type String = [Char]


	Define Button as a triple consisting of the associated command
	the buttons screen depth and the text label to the button.

> type Button = (Command,Int,String)


	Defines the string used to spot end of input.

> delimiter :: String
> delimiter = "         "


	Defines a blank string that can be used to erase any button
	label

> blank :: String
> blank = "              "


	Define the height, width and the indent from the left of the screen
	of a button.

> buttonHeight :: Int
> buttonHeight = 40
> buttonWidth :: Int
> buttonWidth = 110
> buttonIndent :: Int
> buttonIndent = 5


	Define the text position within a button

> textIn :: Int
> textIn = buttonIndent+4
> textDown :: Int
> textDown =  30

	Define the render region of the screen. 

> renderRegion :: [Int]
> renderRegion = [renderLeft,renderTop,windowWidth,renderHeight]


	Define the origin, height and width of the render screen

> renderTop :: Int
> renderTop = 0
> renderLeft :: Int
> renderLeft = 120
> renderHeight :: Int
> renderHeight = 510
> renderWidth :: Int
> renderWidth = 780


	Define the window region

> windowRegion :: [Int]
> windowRegion = [0,0,windowWidth,windowHeight]

	Define the text region

> textRegion :: [Int]
> textRegion = [0,(renderHeight+5),(renderLeft-5),50]

	Define the 'notext' region.

> noTextRegion :: [Int]
> noTextRegion = treeRegion

	Define the tree-form region of the screen

> treeRegion :: [Int]
> treeRegion = [renderLeft,(renderHeight+5),
>                                 (windowWidth-renderLeft),(windowHeight-renderHeight)]

	Define the mouse region of the screen

> mouseRegion :: [Int]
> mouseRegion = [0,0,windowWidth,windowHeight]


	Define the window height and width and its origin

> windowLeft :: Int
> windowLeft = 100
> windowTop :: Int
> windowTop = 50
> windowWidth :: Int
> windowWidth = 900
> windowHeight :: Int
> windowHeight = 700

	Define the mouse box by its origin and height and width

> mouseBox :: [Int]
> mouseBox = [mouseLeft,mouseTop,mouseWidth,mouseHeight]
> mouseLeft :: Int
> mouseLeft = buttonIndent
> mouseTop :: Int
> mouseTop = renderHeight+55
> mouseWidth :: Int
> mouseWidth = (renderLeft-10)
> mouseHeight :: Int
> mouseHeight = mbuttonHeight+18

	Define each of the buttons on the mouse
	
> button1Box :: [Int]
> button1Box = [(buttonIndent+5),(renderHeight+60),mbuttonWidth,mbuttonHeight]
> button2Box :: [Int]
> button2Box = [(buttonIndent+40),(renderHeight+60),mbuttonWidth,mbuttonHeight]
> button3Box :: [Int]
> button3Box = [(buttonIndent+75),(renderHeight+60),mbuttonWidth,mbuttonHeight]

	Define the positions of each text column in each mouse button

> button1TextOrigin,button2TextOrigin,button3TextOrigin :: (Int,Int)
> button1TextOrigin = ((buttonIndent+8),(renderHeight+75))
> button2TextOrigin = ((buttonIndent+43), (renderHeight+75))
> button3TextOrigin = ((buttonIndent+78), (renderHeight+75))

	Define the mouse button height and width
	
> mbuttonWidth :: Int
> mbuttonWidth = 30
> mbuttonHeight :: Int
> mbuttonHeight = 104


	define the origin of the mouse labelling caption

> mouseCaptionDown,mouseCaptionAcross :: Int
> mouseCaptionDown = (renderHeight+74+mbuttonHeight)
> mouseCaptionAcross = buttonIndent+38

	Define a list of all buttons

> buttons :: [Button]
> buttons = [primitiveButton,unionButton,intersectButton,subtractButton,
> 		complementButton, partitionButton, renderButton, 
> 				classifyButton, areaButton, quitButton]

	Define each button.

> primitiveButton :: Button
> primitiveButton = (Polygon,10,	"   Polygon    ")
> unionButton :: Button
> unionButton = (Union,60,		"    Union     ")
> intersectButton :: Button
> intersectButton = (Intersect,110,	"  Intersect   ")
> subtractButton :: Button
> subtractButton = (Subtract,160,	"   Subtract   ")
> complementButton :: Button
> complementButton = (Complement,210,	"  Complement  ")
> partitionButton :: Button
> partitionButton = (Partition,260,	"Partitionings ")
> renderButton :: Button
> renderButton = (Render,310,		"    Render    ")
> classifyButton :: Button
> classifyButton = (Classify,360,	"Classify Point")
> areaButton :: Button
> areaButton = (Area,410,		"     Area     ")
> quitButton :: Button
> quitButton = (Quit,460,		"     Quit     ")

	Define button region.

> button :: Int -> [Int]
> button d = [buttonIndent,d,buttonWidth,buttonHeight]	

	Correction parameter to make mouse pointer possible to use
	with accuracy.
	
> mouseDispx, mouseDispy :: Int
> mouseDispx = 5
> mouseDispy = 4

	Define a gap.

> gap = ' '
