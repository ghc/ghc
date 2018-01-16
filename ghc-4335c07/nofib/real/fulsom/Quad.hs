{-
 -  Fulsom (The Solid Modeller, written in Haskell)
 -
 -  Copyright 1990,1991,1992,1993 Duncan Sinclair
 -
 - Permissiom to use, copy, modify, and distribute this software for any 
 - purpose and without fee is hereby granted, provided that the above
 - copyright notice and this permission notice appear in all copies, and
 - that my name not be used in advertising or publicity pertaining to this
 - software without specific, written prior permission.  I makes no
 - representations about the suitability of this software for any purpose.
 - It is provided ``as is'' without express or implied warranty.
 - 
 - Duncan Sinclair 1993.
 - 
 - Oct-tree to Quad-tree processing.
 -
 -}

module Quad(quadoct) where

import Oct
import Csg
import Types
import Interval

quadoct o = qo o Q_Empty

qo (o        ) q@(Q_Full t) = q
qo (O_Empty  ) (q         ) = q
qo (O_Sub s l) (Q_Empty   ) = Q_Sub s z
                   where
                     (l0:ll1) = l   ; (l1:ll2) = ll1
                     (l2:ll3) = ll2 ; (l3:ll4) = ll3
                     (l4:ll5) = ll4 ; (l5:ll6) = ll5
                     (l6:ll7) = ll6 ; (l7:ll8) = ll7
		     z = [ qo (l1) (qo (l0) Q_Empty) ,
                           qo (l3) (qo (l2) Q_Empty) ,
                           qo (l5) (qo (l4) Q_Empty) ,
                           qo (l7) (qo (l6) Q_Empty) ]
qo (O_Sub s l) (Q_Sub t k) = Q_Sub t z
                   where
                     (l0:ll1) = l   ; (l1:ll2) = ll1
                     (l2:ll3) = ll2 ; (l3:ll4) = ll3
                     (l4:ll5) = ll4 ; (l5:ll6) = ll5
                     (l6:ll7) = ll6 ; (l7:ll8) = ll7
                     (k0:kk1) = k   ; (k1:kk2) = kk1
                     (k2:kk3) = kk2 ; (k3:kk4) = kk3
		     z = [ qo (l1) (qo (l0) (k0)) ,
                           qo (l3) (qo (l2) (k1)) ,
                           qo (l5) (qo (l4) (k2)) ,
                           qo (l7) (qo (l6) (k3)) ]
qo o@(O_Full s) (Q_Sub t k) = Q_Sub t z
                   where
                     (k0:kk1) = k   ; (k1:kk2) = kk1
                     (k2:kk3) = kk2 ; (k3:kk4) = kk3
		     z = [ qo o (k0) , qo o (k1) ,
                           qo o (k2) , qo o (k3) ]
qo (O_Full s ) (q         ) = Q_Full s

