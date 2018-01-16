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
 - All major types declared here.
 -
 -}

module Types (
  FType,BI,Prim(..),Ops(..),Csg(..),CsgOut,Calc,
  Color(..),R3,R1,Row,Arr,Oct(..),tuple,Quad(..),Vector,In
 )
where

import Interval

---------------------------------------------------------
-- Csg
----------------------------------------------------------

type FType = Double

type BI = Interval FType

data Prim = Sphere FType FType FType FType
          | Cube   FType FType FType FType
          | Plane  FType FType FType FType
          | X | Y | Z
	  deriving Show{-was:Text-}

data Ops  = RotX FType
          | RotY FType
          | RotZ FType
          | Scale FType FType FType
          | Trans FType FType FType
	  deriving Show{-was:Text-}

data Csg  = Object Prim
          | Geom Csg Ops
          | Func Calc
          | Matrix Csg Arr
          | Colour Color Csg
          | Union Csg Csg
          | Inter Csg Csg
          | Sub   Csg Csg
          | Comp  Csg
--	  deriving Show{-was:Text-}


-- type CsgOut = (R1 BI,Csg,Color,Bool)
type CsgOut = (BI,Csg,Color,Bool)

type Calc = Color -> (R3 BI) -> CsgOut

----------------------------------------------------------
-- [KC]olor
----------------------------------------------------------

data Color = RGB FType FType FType
	  deriving Show{-was:Text-}

----------------------------------------------------------
-- Matrix
----------------------------------------------------------

type Row = (FType,FType,FType,FType)
type Arr = (Row,Row,Row)

-- type (Fractional a) => R3 a = (a,a,a)
-- type (Fractional a) => R1 a = a

type R3 a = (a,a,a)
-- type R1 a = a
type R1 a = (a,a)

----------------------------------------------------------
-- Oct
----------------------------------------------------------

data Oct = O_Full Color | O_Empty | O_Sub Color [Oct]
	  deriving Show{-was:Text-}

----------------------------------------------------------
-- Quad
----------------------------------------------------------

data Quad = Q_Empty | Q_Full Color
          | Q_Sub Color [Quad]
          | Q_NewXY FType FType FType
	  deriving Show{-was:Text-}

----------------------------------------------------------
-- Vector
----------------------------------------------------------

type Vector = (FType,FType,FType)

type In = Interval FType

----------------------------------------------------------
-- Copyright
----------------------------------------------------------

copyright () = "Copyright 1990,1991,1992,1993 Duncan Sinclair."
e_mail    () = "sinclair@dcs.gla.ac.uk"

tuple = (copyright,e_mail)

----------------------------------------------------------
