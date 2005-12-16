-- Copyright (c) 2000 Galois Connections, Inc.
-- All rights reserved.  This software is distributed as
-- free software under the license in the file "LICENSE",
-- which is included in the distribution.

module Data where

import Array

import CSG      
import Geometry
import Illumination
import Primitives
import Surface

import Debug.Trace

-- Now the parsed (expresssion) language

type Name = String

type Code = [GMLToken]

data GMLToken
    -- All these can occur in parsed code
	= TOp     GMLOp
	| TId     Name
	| TBind   Name
	| TBool   Bool
	| TInt    Int
	| TReal   Double
	| TString String
	| TBody   Code
	| TArray  Code
	| TApply
	| TIf
	 -- These can occur in optimized/transformed code
	 -- NONE (yet!)


instance Show GMLToken where
   showsPrec p (TOp op)     = shows op
   showsPrec p (TId id)     = showString id
   showsPrec p (TBind id)   = showString ('/' : id)
   showsPrec p (TBool bool) = shows bool
   showsPrec p (TInt i)     = shows i
   showsPrec p (TReal d)    = shows d
   showsPrec p (TString s)  = shows s
   showsPrec p (TBody code) = shows code
   showsPrec p (TArray code) = showString "[ " 
                            . foldr (\ a b -> a . showChar ' ' . b) id (map shows code) 
                            . showString "]"
   showsPrec p (TApply)     = showString "apply" 
   showsPrec p (TIf)        = showString "if" 

   showList  code = showString "{ " 
                  . foldr (\ a b -> a . showChar ' ' . b) id (map shows code) 
                  . showString "}"


-- Now the value language, used inside the interpreter

type Stack = [GMLValue]

data GMLValue
	= VBool    !Bool
	| VInt     !Int
	| VReal    !Double
	| VString  String
	| VClosure Env Code
	| VArray   (Array Int GMLValue)		-- FIXME: Haskell array
        -- uses the interpreter version of point
	| VPoint   { xPoint :: !Double
                   , yPoint :: !Double 
                   , zPoint :: !Double 
                   } 
        -- these are abstract to the interpreter
	| VObject  Object
	| VLight   Light 
	-- This is an abstract object, used by the abstract interpreter
	| VAbsObj  AbsObj


-- There are only *3* basic abstract values,
-- and the combinators also.

data AbsObj 
    = AbsFACE 
    | AbsU 
    | AbsV
      deriving (Show)

instance Show GMLValue where
   showsPrec p value = showString (showStkEle value)

showStkEle :: GMLValue -> String
showStkEle (VBool b)      = show b ++ " :: Bool"
showStkEle (VInt i)       = show i ++ " :: Int"
showStkEle (VReal r)      = show r ++ " :: Real"
showStkEle (VString s)    = show s ++ " :: String"
showStkEle (VClosure {})  = "<closure> :: Closure"
showStkEle (VArray arr)   
     = "<array (" ++  show (succ (snd (bounds arr))) ++ " elements)> :: Array"
showStkEle (VPoint x y z) = "(" ++ show x 
                         ++ "," ++ show y
                         ++ "," ++ show z
                         ++ ") :: Point"
showStkEle (VObject {})   = "<Object> :: Object"
showStkEle (VLight {})    = "<Light> :: Object"
showStkEle (VAbsObj vobs) = "{{ " ++ show vobs ++ "}} :: AbsObj"

-- An abstract environment

newtype Env = Env [(Name, GMLValue)] deriving Show

emptyEnv :: Env
emptyEnv = Env []

extendEnv :: Env -> Name -> GMLValue -> Env
extendEnv (Env e) n v = Env ((n, v):e)

lookupEnv :: Env -> Name -> Maybe GMLValue
lookupEnv (Env e) n = lookup n e

-- All primitive operators
-- 
-- There is no Op_apply, Op_false, Op_true and Op_if
-- (because they appear explcitly in the rules).

data GMLOp
   = Op_acos
   | Op_addi
   | Op_addf
   | Op_asin
   | Op_clampf
   | Op_cone
   | Op_cos
   | Op_cube
   | Op_cylinder
   | Op_difference
   | Op_divi
   | Op_divf
   | Op_eqi
   | Op_eqf
   | Op_floor
   | Op_frac
   | Op_get
   | Op_getx
   | Op_gety
   | Op_getz
   | Op_intersect
   | Op_length
   | Op_lessi
   | Op_lessf
   | Op_light
   | Op_modi
   | Op_muli
   | Op_mulf
   | Op_negi
   | Op_negf
   | Op_plane
   | Op_point
   | Op_pointlight
   | Op_real
   | Op_render
   | Op_rotatex
   | Op_rotatey
   | Op_rotatez
   | Op_scale
   | Op_sin
   | Op_sphere
   | Op_spotlight
   | Op_sqrt
   | Op_subi
   | Op_subf
   | Op_trace       -- non standard, for debugging GML programs
   | Op_translate
   | Op_union
   | Op_uscale
    deriving (Eq,Ord,Ix,Bounded)

instance Show GMLOp where
   showsPrec _ op = showString (opNameTable ! op)


------------------------------------------------------------------------------

-- And how we use the op codes (there names, there interface)

-- These keywords include, "apply", "if", "true" and "false",
-- they are not parsed as operators, but are
-- captured by the parser as a special case.

keyWords :: [String]
keyWords = [ kwd | (kwd,_,_) <- opcodes ]

-- Lookup has to look from the start (or else...)
opTable :: [(Name,GMLToken)]
opTable = [ (kwd,op) | (kwd,op,_) <- opcodes ]

opNameTable :: Array GMLOp Name
opNameTable = array (minBound,maxBound) 
	          [ (op,name) | (name,TOp op,_) <- opcodes ]

undef = error "undefined function"
image = error "undefined function: talk to image group"

-- typically, its best to have *one* opcode table,
-- so that mis-alignments do not happen.

opcodes :: [(String,GMLToken,PrimOp)]
opcodes =
 [ ("apply",	  TApply,	 	error "incorrect use of apply")
 , ("if",	  TIf, 			error "incorrect use of if")
 , ("false",	  TBool False, 		error "incorrect use of false")
 , ("true",	  TBool True, 		error "incorrect use of true")
 ] ++ map (\ (a,b,c) -> (a,TOp b,c))
   -- These are just invocation, any coersions need to occur between here
   -- and before arriving at the application code (like deg -> rad).
 [ ("acos",	  Op_acos, 	 Real_Real (rad2deg . acos))
 , ("addi",	  Op_addi, 	 Int_Int_Int (+))
 , ("addf",	  Op_addf, 	 Real_Real_Real (+))
 , ("asin",	  Op_asin, 	 Real_Real (rad2deg . asin))
 , ("clampf",	  Op_clampf, 	 Real_Real clampf)
 , ("cone",	  Op_cone, 	 Surface_Obj cone)
 , ("cos",	  Op_cos, 	 Real_Real (cos . deg2rad))
 , ("cube",	  Op_cube, 	 Surface_Obj cube)
 , ("cylinder",	  Op_cylinder, 	 Surface_Obj cylinder)
 , ("difference", Op_difference, Obj_Obj_Obj difference)
 , ("divi",	  Op_divi, 	 Int_Int_Int (ourQuot))
 , ("divf",	  Op_divf, 	 Real_Real_Real (/))
 , ("eqi",	  Op_eqi, 	 Int_Int_Bool (==))
 , ("eqf",	  Op_eqf, 	 Real_Real_Bool (==))
 , ("floor",	  Op_floor, 	 Real_Int floor)
 , ("frac",	  Op_frac, 	 Real_Real (snd . properFraction))
 , ("get",	  Op_get, 	 Arr_Int_Value ixGet)
 , ("getx",	  Op_getx, 	 Point_Real (\ x y z -> x))
 , ("gety",	  Op_gety, 	 Point_Real (\ x y z -> y))
 , ("getz",	  Op_getz, 	 Point_Real (\ x y z -> z))
 , ("intersect",  Op_intersect,  Obj_Obj_Obj intersect)
 , ("length",	  Op_length, 	 Arr_Int (succ . snd . bounds))
 , ("lessi",	  Op_lessi, 	 Int_Int_Bool (<))
 , ("lessf",	  Op_lessf, 	 Real_Real_Bool (<))
 , ("light",	  Op_light, 	 Point_Color_Light light)
 , ("modi",	  Op_modi, 	 Int_Int_Int (ourRem))
 , ("muli",	  Op_muli, 	 Int_Int_Int (*))
 , ("mulf",	  Op_mulf, 	 Real_Real_Real (*))
 , ("negi",	  Op_negi, 	 Int_Int negate)
 , ("negf",	  Op_negf, 	 Real_Real negate)
 , ("plane",	  Op_plane, 	 Surface_Obj plane)
 , ("point",	  Op_point, 	 Real_Real_Real_Point VPoint)
 , ("pointlight", Op_pointlight, Point_Color_Light pointlight)
 , ("real",	  Op_real, 	 Int_Real fromIntegral)
 , ("render",	  Op_render, 	 Render $ render eye)
 , ("rotatex",	  Op_rotatex, 	 Obj_Real_Obj (\ o d -> rotateX (deg2rad d) o))
 , ("rotatey",	  Op_rotatey, 	 Obj_Real_Obj (\ o d -> rotateY (deg2rad d) o)) 
 , ("rotatez",	  Op_rotatez, 	 Obj_Real_Obj (\ o d -> rotateZ (deg2rad d) o))
 , ("scale",	  Op_scale, 	 Obj_Real_Real_Real_Obj (\ o x y z -> scale (x,y,z) o))
 , ("sin",	  Op_sin, 	 Real_Real (sin . deg2rad))
 , ("sphere",	  Op_sphere, 	 Surface_Obj sphere') -- see comment at end of file
 , ("spotlight",  Op_spotlight,  Point_Point_Color_Real_Real_Light mySpotlight)
 , ("sqrt",	  Op_sqrt, 	 Real_Real ourSqrt)
 , ("subi",	  Op_subi, 	 Int_Int_Int (-))
 , ("subf",	  Op_subf, 	 Real_Real_Real (-))
 , ("trace",      Op_trace,      Value_String_Value mytrace)
 , ("translate",  Op_translate,  Obj_Real_Real_Real_Obj (\ o x y z -> translate (x,y,z) o))
 , ("union",	  Op_union, 	 Obj_Obj_Obj union)
 , ("uscale",	  Op_uscale, 	 Obj_Real_Obj (\ o r -> uscale r o))
 ]

-- This enumerate all possible ways of calling the fixed primitives

-- The datatype captures the type at the *interp* level,
-- the type of the functional is mirrored on this (using Haskell types).

data PrimOp

    -- 1 argument 
    = Int_Int         (Int -> Int)
    | Real_Real       (Double -> Double)
    | Point_Real      (Double -> Double -> Double -> Double)
    | Surface_Obj     (SurfaceFn Color Double -> Object)
    | Real_Int        (Double -> Int)
    | Int_Real        (Int -> Double)
    | Arr_Int         (Array Int GMLValue -> Int)

    -- 2 arguments 
    | Int_Int_Int     (Int -> Int -> Int)
    | Int_Int_Bool    (Int -> Int -> Bool)
    | Real_Real_Real  (Double -> Double -> Double)
    | Real_Real_Bool  (Double -> Double -> Bool)
    | Arr_Int_Value   (Array Int GMLValue -> Int -> GMLValue)

    -- Many arguments, typically image mangling

    | Obj_Obj_Obj            (Object -> Object -> Object)
    | Point_Color_Light      (Coords -> Color -> Light)
    | Real_Real_Real_Point   (Double -> Double -> Double -> GMLValue)
    | Obj_Real_Obj           (Object -> Double -> Object)
    | Obj_Real_Real_Real_Obj (Object -> Double -> Double -> Double -> Object)
    | Value_String_Value     (GMLValue -> String -> GMLValue)

    | Point_Point_Color_Real_Real_Light 
                             (Coords -> Coords -> Color -> Radian -> Radian -> Light)
    -- And finally render
    | Render                 (Color -> [Light] -> Object -> Int -> Double -> Int -> Int -> String -> IO ())

data Type 
    = TyBool 
    | TyInt 
    | TyReal 
    | TyString 
    | TyCode 
    | TyArray 
    | TyPoint 
    | TyObject 
    | TyLight
    | TyAlpha
    | TyAbsObj
      deriving (Eq,Ord,Ix,Bounded)

typeTable = 
  [ ( TyBool,   "Bool")
  , ( TyInt,    "Int")
  , ( TyReal,   "Real")
  , ( TyString, "String")
  , ( TyCode,   "Code")
  , ( TyArray,  "Array")
  , ( TyPoint,  "Point")
  , ( TyObject, "Object")
  , ( TyLight,  "Light")
  , ( TyAlpha,  "<anything>")
  , ( TyAbsObj, "<abs>")
  ]

typeNames = array (minBound,maxBound) typeTable

instance Show Type where
   showsPrec _ op = showString (typeNames ! op)

getPrimOpType :: PrimOp -> [Type]
getPrimOpType (Int_Int         _) = [TyInt]
getPrimOpType (Real_Real       _) = [TyReal]
getPrimOpType (Point_Real      _) = [TyPoint]
getPrimOpType (Surface_Obj     _) = [TyCode]
getPrimOpType (Real_Int        _) = [TyReal]
getPrimOpType (Int_Real        _) = [TyInt]
getPrimOpType (Arr_Int         _) = [TyArray]
getPrimOpType (Int_Int_Int     _) = [TyInt,TyInt]
getPrimOpType (Int_Int_Bool    _) = [TyInt,TyInt]
getPrimOpType (Real_Real_Real  _) = [TyReal,TyReal]
getPrimOpType (Real_Real_Bool  _) = [TyReal,TyReal]
getPrimOpType (Arr_Int_Value   _) = [TyArray,TyInt]
getPrimOpType (Obj_Obj_Obj            _) = [TyObject,TyObject]
getPrimOpType (Point_Color_Light      _) = [TyPoint,TyPoint]
getPrimOpType (Real_Real_Real_Point   _) = [TyReal,TyReal,TyReal]
getPrimOpType (Obj_Real_Obj           _) = [TyObject,TyReal]
getPrimOpType (Obj_Real_Real_Real_Obj _) = [TyObject,TyReal,TyReal,TyReal]
getPrimOpType (Value_String_Value     _) = [TyAlpha,TyString]
getPrimOpType (Point_Point_Color_Real_Real_Light _) 
                                         = [TyPoint,TyPoint,TyPoint,TyReal,TyReal]
getPrimOpType (Render                 _) = [TyPoint,
                                            TyLight,
                                            TyObject,
                                            TyInt,
                                            TyReal,
                                            TyReal,
                                            TyReal,
                                            TyString]


-- Some primitives with better error message

mytrace v s = trace (s ++" : "++ show v ++ "\n") v


ixGet :: Array Int GMLValue -> Int -> GMLValue
ixGet arr i
   | inRange (bounds arr) i = arr ! i
   | otherwise = error ("failed access with index value " 
                     ++ show i 
                     ++ " (should be between 0 and " 
                     ++ show (snd (bounds arr)) ++ ")")

ourQuot :: Int -> Int -> Int
ourQuot _ 0 = error "attempt to use divi to divide by 0"
ourQuot a b = a `quot` b

ourRem :: Int -> Int -> Int
ourRem _ 0 = error "attempt to use remi to divide by 0"
ourRem a b = a `rem` b

ourSqrt :: Double -> Double
ourSqrt n | n < 0     = error "attempt to use sqrt on a negative number"
          | otherwise = sqrt n


mySpotlight p1 p2 col cutoff exp = spotlight p1 p2 col (deg2rad cutoff) exp

-- The problem specification gets the mapping for spheres backwards
-- (it maps the image from right to left).
-- We've fixed that in the raytracing library so that it goes from left
-- to right, but to keep the GML front compatible with the problem
-- statement, we reverse it here.

sphere' :: SurfaceFn Color Double -> CSG (SurfaceFn Color Double)
sphere' (SFun f) = sphere (SFun (\i u v -> f i (1 - u) v))
sphere' s = sphere s
