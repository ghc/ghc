\begin{code}
module Prog(prog) where


-- Program module - defines the program expression
 
import Interface (modeller)
import Init (initialiseMouse,initialiseScreen)
import Interpret (Command,interpret, Operation(..), Operations(..))
import Input (operationsBatch)
import BSPT (BSPT,Point,buildBSPT)
import Euclid (Face, Faces(..))



-- Main program -----------------------------

--	prog - initialises the Geometric Modeller System 
--		with an initial (null) object and with 
--		interpretation (lazy) of users requirements.
--	
--		Operations can be predefined in the file 
--		Input.hs, with the modeller being supplied
--		batchOperations (as opposed to operations)
--		as the second argument.
 
prog ::  String -> String
prog rawinp =
  initialise ++ modeller (buildBSPT []) operations
 where 	
  initialise = initialiseMouse ++ initialiseScreen
  lined@(head:rest) = lines rawinp
  operations =
    if (head=="batch") then operationsBatch else interpret lined
\end{code}
