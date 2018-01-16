
-- | Game state
module State where
import World
import Points2D.Types


-- | The game state.
data State
	= State
	{ stateWorld		:: World Rect
	, stateModeInterface	:: ModeInterface
	, stateModeDisplay	:: ModeDisplay
	, stateViewPos		:: Point }


-- | What mode the interface interaction is in.
data ModeInterface
	-- | We're not doing anything inparticular.
	= ModeInterfaceIdle

	-- | We're moving the view position.
	| ModeInterfaceMove
	deriving (Show, Eq)


-- | What mode the display is in.
data ModeDisplay
	-- | Show the world in rectangular coordinates.
	= ModeDisplayWorld

	-- | Show the world normalised so the view position is at the origin.
	| ModeDisplayNormalised

	-- | Show the world in polar coordinates.
	| ModeDisplayPolar
	deriving (Show, Eq)


-- | Initial game state.
initialState :: World Rect -> State
initialState world
	= State
	{ stateWorld		= world
	, stateModeInterface	= ModeInterfaceIdle
	, stateModeDisplay	= ModeDisplayWorld
	, stateViewPos		= (0, 0) }
		

	
	
