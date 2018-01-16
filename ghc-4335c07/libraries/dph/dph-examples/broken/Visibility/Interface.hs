
module Interface
	( handleInput
	, stepState)
where
import State
import qualified Graphics.Gloss.Game	as G


-- | Handle an input event.
handleInput :: G.Event -> State -> State

-- Move the view position
handleInput (G.EventKey key keyState _ (x, y)) state
	| G.MouseButton G.LeftButton	<- key
	, G.Down			<- keyState
	= state	{ stateModeInterface	= ModeInterfaceMove 
		, stateViewPos
			= ( fromRational $ toRational x
			  , fromRational $ toRational y) }

	| G.MouseButton G.LeftButton	<- key
	, G.Up				<- keyState
	= state	{ stateModeInterface	= ModeInterfaceIdle }

handleInput (G.EventMotion (x, y)) state
	| stateModeInterface state == ModeInterfaceMove
	= state { stateViewPos
			= ( fromRational $ toRational x
			  , fromRational $ toRational y) }

-- Set the display mode
handleInput (G.EventKey key keyState _ _) state
	| G.Char 'w'			<- key
	, G.Down			<- keyState
	= state	{ stateModeDisplay	= ModeDisplayWorld }

handleInput (G.EventKey key keyState _ _) state
	| G.Char 'n'			<- key
	, G.Down			<- keyState
	= state	{ stateModeDisplay	= ModeDisplayNormalised }

handleInput (G.EventKey key keyState _ _) state
	| G.Char 'p'			<- key
	, G.Down			<- keyState
	= state	{ stateModeDisplay	= ModeDisplayPolar }
	

handleInput _ state
	= state


stepState :: Float -> State -> State
stepState _ state = state
