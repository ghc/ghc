-- LML original: Sandra Foubister, 1990
-- Haskell translation: Colin Runciman, May 1991

module Main(main) where

import Mgrfuns
import Progfuns
import Auxprogfuns
import Layout
import Tilefuns

main = do
    fromMgr <- getContents
    let
	  toMgr = setmode 7 ++
		  shapewindow [0,0,1150,900] ++
		  setup ++
		  potatotile ([],1,initalist) (lines fromMgr) ++
		  shapewindow [0,0,500,500] ++
		  font 8 ++
		  textreset ++
		  clear ++
		  func 15 
    putStr toMgr



