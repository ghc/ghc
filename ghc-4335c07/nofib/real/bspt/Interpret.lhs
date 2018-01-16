> module Interpret 

	Module that produces the interpretation of the users input
	producing a list of human readable operation specifications

>		(Command, Operations, Operation, interpret)

> where



> import Params (Button(..),Command(..),delimiter,buttonIndent,buttonWidth,buttons,
>		 buttonHeight)
> import EuclidGMS (inScreen,Point(..),mkPoint)
> import GeomNum
> import Stdlib (between,splitAt_YORK)

	An operation is a Command and a list of points to be assiciated with that
	command

> type Operation = (Command,Points)

	Points is a list of point

> type Points = [Point]

	Operations is a list of operation

> type Operations = [Operation]


	interpret: Takes a list of strings, converts the head string
		into the appropriate command by first evaluating the point
		represented and interpreting that point w.r.t. the buttons.
		Returns the operation, the button and a list of points,
		and the rest of the remaining strings.

> interpret :: [String] -> Operations
> interpret (head:residue) = (operation:interpret residue' )
>				where
>				(operation,residue') = toOperation command residue
>				command = (toCommand.mkPoint) head

	toOperation: Produces an operation from a command and a string
		this involves interpreting a returning any list of points 
		associated the command.

> toOperation :: Command -> [String] -> (Operation,[String])
> toOperation Polygon str = ((Polygon,mkPoints points),out)
>				where 
>				(points,out) = splitAt_YORK delimiter str
> toOperation Union str = ((Union,mkPoints points),out)
>				where 
>				(points,out) = splitAt_YORK delimiter str
> toOperation Intersect str = ((Intersect,mkPoints points),out)
>				where 
>				(points,out) = splitAt_YORK delimiter str
> toOperation Subtract str = ((Subtract,mkPoints points),out)
>				where 
>				(points,out) = splitAt_YORK delimiter str
> toOperation Classify str = ((Classify,mkPoints points),out)
>				where 
>				(points,out) = splitAt_YORK delimiter str
> toOperation Complement str = ((Complement,[]),str)
> toOperation Partition str = ((Partition,[]),str)
> toOperation Render str = ((Render,[]),str)
> toOperation Area str = ((Area,[]),str)
> toOperation Quit str = ((Quit,[]),str)
> toOperation Null str = ((Null,[]),str)

	mkPoints : produces a list of points from a list of string
		representations of those points, filters those not on
		the screen away.

> mkPoints :: [String] -> Points
> mkPoints = filter inScreen.map mkPoint


	toCommand: Return the button associated with the point given.
		Checks first to see that xCo-ordinate lies
		in button pad region. If so it searches through the
		buttons, otherwise returns the Null button.

> toCommand :: Point -> Command 
> toCommand (Pt x y) | x<fromIntegral buttonIndent || 
>				x>fromIntegral (buttonIndent+buttonWidth)
>					 = Null
>		   | otherwise = command
>				where (command,_,_) = search (y::Numb) buttons


	search: Searches for a button at the height given by y. If no
		button found then Null button returned. Else button
		found is returned.

> search :: Numb -> [Button] -> Button
> search y [] = (Null,0,"")
> search y ((command,d,str):butts) | between (fromIntegral d)
>                               (fromIntegral buttonHeight) y = (command,d,str)
> search y ((command,d,str):butts) = search y butts
