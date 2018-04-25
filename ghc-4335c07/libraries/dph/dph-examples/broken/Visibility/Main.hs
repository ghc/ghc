
-- | Visibility on the 2D plane.
--   Uses an instance of Warnocks algorithm.
--   TODO: animate the line segments, make them spin and move around so we can see
--         that it's a dynamic visiblity algorithm -- not pre-computed.
--         Draw lines in random shades of color depending on the index.
--         Make a key to swap between rectangular and polar projections.
--         Allow viewpoint to be set with the mouse.

import Interface
import Draw
import State
import World
import Graphics.Gloss.Game

main :: IO ()
main
 = do	world		<- initialWorld
	let state	=  initialState world
	
	gameInWindow
		"Visibility"
		(1000, 1000)
		(10,  10)
		black
		100
		state
		drawState
		handleInput
		stepState
		
		
		