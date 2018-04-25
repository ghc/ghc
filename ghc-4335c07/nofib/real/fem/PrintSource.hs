-- Glasgow Haskell 0.403 : FINITE ELEMENT PROGRAM V2
-- **********************************************************************
-- *                                                                    *
-- * FILE NAME : printsource.hs		DATE : 13-3-1991                *
-- *                                                                    *
-- * CONTENTS : Print source data. Return source data as [char].        *
-- *                                                                    *
-- **********************************************************************

module PrintSource(source_data) where

import Database

import DB_interface

import Basics

source_data :: (Array Int Int, Array Int Float) -> [Char]
source_data db
      = 
        control_data db ++ 
	node_data db ++ 
	material_data db ++ 
  	elements db ++ 
	plds db

control_data db
      = "\n\n\nCONTROL DATA :\n\n" ++
        "   Total number of nodes = " ++ (showlj 3 (nnode db)) ++ "\n" ++
        "   Number of elements    = " ++ (showlj 3 (nelem db)) ++ "\n" ++
        "   Number of point loads = " ++ (showlj 3 (nplds db)) ++ "\n" ++
        "   Total number of materials = "++ (showlj 3 (nmats db)) ++ "\n\n\n"

node_data db 
      = "NODE INFORMATION  :\n\n" ++
        (concat ( map a_node_s [1..(nnode db)] )) ++ "\n\n"
	where	
	a_node_s = a_node db

a_node db node
      = "  Node.no = " ++ (showlj 3 node) ++  "   x = "
        ++ (showlj 8 x) ++ "   y = " ++ (showlj 8 y)
        ++ "   bc = " ++ ( showlj 3 bc) ++ "\n"
        where
        (x,y) = getnxy db node
        bc    = getnbc db node

material_data db
      = "MATERIAL INFORMATION :\n\n"
        ++ (concat (map a_material_s [1..(nmats db)]))
	where
	a_material_s = a_material db

a_material db material =
        "  Material No.=" ++ (showlj 3 material) ++ "   EA = "
        ++ (showlj 8 ea) ++ "   EI = " ++ (showlj 8 ei) ++ "\n"
	where
        (ea,ei) = getmpro db material

elements db
      = "\nELEMENT DATA:\n\n" ++
        (concat (map (a_element_s) [1..(nelem db)])) ++ "\n\n"
	where
	a_element_s = a_element db
	
a_element db element =
	"  Element No.=" ++ (showlj 3 element) ++ "   Node.L ="
       	++ (showlj 3 nodel) ++ "   Node.R =" ++ (showlj 3 noder)
       	++ "   Material No. =" ++ (showlj 3 material) ++ "\n"
	where
        (nodel,noder) = getenlr db element
        material      = getemat db element

plds db
      = "\nPOINT LOADS DATA:\n\n" ++
        (concat (map (a_load_s) [1..(nplds db)])) ++ "\n\n"
	where
	a_load_s = a_load db

a_load db n =
	"  To_point No." ++ (showlj 3 to_point) ++ "  Px = "
       	++ (showlj 9 px) ++ "  Py = " ++ (showlj 9 py)
       	++ "  M = " ++ (showlj 9 m) ++ "\n"
	where
        (to_point,px,py,m) = getpld db n


