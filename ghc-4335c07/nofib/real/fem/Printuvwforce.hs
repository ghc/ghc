-- Glasow Haskell 0.403 : FINITE ELEMENT PROGRAM V2
-- **********************************************************************
-- *                                                                    *
-- * FILE NAME : elemforce.hs           DATE : 13-3-1991                *
-- *                                                                    *
-- * CONTENTS : Computes element internal forces(only 2D bar element).  *
-- *                                                                    *
-- **********************************************************************

module Printuvwforce( uvwresult, forceresult) where

import Basics
import Vector
import DB_interface
import Displacement
import Elemforce


uvwresult   :: (Array Int Int, Array Int Float) -> 
	       		Vec Float -> [Char]

forceresult :: (Array Int Int, Array Int Float) -> 
			Vec Float -> [Char]

uvwresult s uvw =
	"\n\nDISPLACEMENT OF THE STRUCTURE \n\n" ++
	"     Node     X     Y     BC         U             V" ++
	"                  Theta\n" ++
	(concat (map a_node_s [1..(nnode s)]))
	where
	a_node_s = a_node s uvw

a_node s uvw node =
	(showrj 8 node) ++ (showrj 7 x) ++ (showrj 6 y) ++
	(showrj 7 bc) ++
	(showrj 15 u)   ++ (showrj 15 v)   ++ (showrj 15 theta) ++  "\n"
	where
	(x,y) = getnxy s node
	bc    = getnbc s node
	(u,v,theta) = getnuvw s node uvw

forceresult s frc =
	"\n\n\nINTERNAL FORCES OF ELEMENT " ++ "\n\n" ++
	"    Element    NodeL   NodeR     TENSION\n" ++
	concat  (map a_element_s [1..(nelem s)])
	where
	a_element_s = a_element s frc

a_element s frc element =
	(showrj 8 element) ++ (showrj 10 nodel) ++ (showrj 8 noder) ++
	(showrj 15 f) ++ "\n"
	where
	(nodel,noder) = getenlr s element
	f 	      = getefrc s element frc
