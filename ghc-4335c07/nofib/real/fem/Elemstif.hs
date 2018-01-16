-- Glasow Haskell 0.403 : FINITE ELEMENT PROGRAM V2
-- **********************************************************************
-- *                                                                    *
-- * FILE NAME : elemstif.hs            DATE : 13-3-1991                *
-- *                                                                    *
-- * CONTENTS : Computes element stiffness matrix.                      *
-- *                                                                    *
-- **********************************************************************

-- Two dimensional Beam Element:
--
--     Left Node nl (xl,yl)
--     Right Node nr (xr,yr)
--     Material Property Parameter (ea,ei)
--
--
--     Ke = [ T * K11 * T~  T * K12 * T~ ]
--          [ T * K21 * T~  T * K22 * T~ ]
--
--          [ cos a   -sin a    0 ]
--     T  = [ sin a    cos a    0 ]
--          [   0        0      1 ]
--
--          [ ea/l       0       0       ]
--     K11= [  0     12ei/l"3  6ei/l"2   ]
--          [  0      6ei/l"2   4ei/l    ]
--
--          [ -ea/l       0       0      ]
--     K12= [  0    -12ei/l"3  6ei/l"2   ]
--          [  0      -6ei/l"2  2ei/l    ]
--
--     K21= K12~
--
--          [ ea/l       0       0       ]
--     K22= [  0     12ei/l"3  -6ei/l"2  ]
--          [  0      -6ei/l"2   4ei/l   ]
--
--     where
--          l = length of the beam element
--          sin a = (yr-yl) / l
--          cos a = (xr-xl) / l

module Elemstif( beam2d ) where         

import Basics
import Vector
import Matrix
import DB_interface

beam2d :: (Array Int Int, Array Int Float) -> Int -> Mat Float

beam2d str element =
        makemat (6,6) 

              ( \ (i,j) -> 
                if      ( i==1 && j==1 ) || ( i==4 && j==4 ) then b1
                else if ( i==2 && j==1 ) || ( i==5 && j==4 ) ||
                        ( i==1 && j==2 ) || ( i==4 && j==5 ) then b2
                else if ( i==2 && j==2 ) || ( i==5 && j==5 ) then b3
                else if ( i==1 && j==4 ) || ( i==4 && j==1 ) then -b1
                else if ( i==2 && j==4 ) || ( i==5 && j==1 ) ||
                        ( i==1 && j==5 ) || ( i==4 && j==2 ) then -b2
                else if ( i==2 && j==5 ) || ( i==5 && j==2 ) then -b3
                else 0.0  )
	where
        a1      = ea / l
        b1      = a1 * c * c
        b2      = a1 * c * s
        b3      = a1 * s * s
        dx      = xr - xl
        dy      = yr - yl
        l       = sqrt (dx*dx + dy*dy)
        c       = dx / l
        s       = dy / l
        (ea,ei) = getmpro str (getemat str element)
        (nl,nr) = getenlr str element
        (xl,yl) = getnxy str nl
        (xr,yr) = getnxy str nr
 
