-- Glasow Haskell 0.403 : FINITE ELEMENT PROGRAM V2
-- **********************************************************************
-- *                                                                    *
-- * FILE NAME : elemforce.hs           DATE : 13-3-1991                *
-- *                                                                    *
-- * CONTENTS : Computes element internal forces(only 2D bar element).  *
-- *                                                                    *
-- **********************************************************************

module Elemforce ( forces, getefrc ) where

import Basics
import Vector
import DB_interface
import Displacement

forces :: (Array Int Int, Array Int Float) -> 
		Vec Float -> Vec Float
	-- The vector which stores the internal forces of all elements

getefrc :: (Array Int Int, Array Int Float) -> Int -> 
		Vec Float -> Float
	-- Return the internal force of given element

forces db uvw =
	makevec (nelem db) force_2d_bar_s 
	where
	force_2d_bar_s = force_2d_bar db uvw

getefrc db element frc =
	vecsub frc element

force_2d_bar  db  uvw element =
	(ea / length) * ( (ur-ul)*c + (vr-vl)*s )
	where
	(nodel,noder) = getenlr db  element
	(ea,ei)       = getmpro db  ( getemat db  element)
	(xl,yl)       = getnxy db  nodel
	(xr,yr)       = getnxy db  noder
	c             = det_x / length
	s             = det_y / length
        det_x         = xr - xl
	det_y         = yr - yl
	length        = sqrt (det_x * det_x + det_y * det_y)
	(ul,vl,thetal) = getnuvw db  nodel uvw
	(ur,vr,thetar) = getnuvw db  noder uvw


