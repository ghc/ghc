-- Glasow Haskell 0.403 : FINITE ELEMENT PROGRAM V2
-- **********************************************************************
-- *                                                                    *
-- * FILE NAME : db_interface.hs		DATE : 13-3-1991        *
-- *                                                                    *
-- * CONTENTS : The data base interface functions.                      *
-- *                                                                    *
-- **********************************************************************

module DB_interface(nnode, nmats, getnxy, getnbc, getmpro, 
                    nelem, nplds, getenlr, getemat, getpld) where

import Data.Array
import Database

nnode, nelem, nmats, nplds :: (Array Int Int, Array Int Float) -> Int

nnode (idb, rdb)  =  idb ! 0
nelem (idb, rdb)  =  idb ! 1
nmats (idb, rdb)  =  idb ! 2      
nplds (idb, rdb)  =  idb ! 3       

getnxy :: (Array Int Int, Array Int Float) -> Int -> (Float, Float)
getnxy (idb, rdb) node 
	= ( x, y )
	  where
     		x = rdb ! index
		y = rdb ! (index + 1)
		index = (node - 1) * 2

getnbc :: (Array Int Int, Array Int Float) -> Int -> Int
getnbc (idb, rdb) node 
	= idb ! ( 3 + node )

getmpro :: (Array Int Int, Array Int Float) -> Int -> (Float, Float)
getmpro (idb, rdb) material 
	= (ea,ei)
          where
		ea = rdb ! index
		ei = rdb ! (index + 1)
		index = (nnode (idb, rdb))*2 + (material-1)*2

getenlr :: (Array Int Int, Array Int Float) -> Int -> (Int, Int)
getenlr (idb, rdb) element 
	= (nodel, noder)
	  where
		nodel = idb ! index
		noder = idb ! (index + 1)
		index = 4 + (nnode (idb, rdb)) + (element-1)*3

getemat :: (Array Int Int, Array Int Float) -> Int -> Int
getemat (idb, rdb) element 
	= idb ! (3 + (nnode (idb, rdb)) + element*3 )

getpld :: (Array Int Int, Array Int Float) -> Int -> (Int, Float, Float, Float)
getpld  (idb, rdb) j 
	= (to_node, px, py, m)
          where
		to_node = idb ! indexi
		px      = rdb ! (indexr)
		py      = rdb ! (indexr+1)
		m       = rdb ! (indexr+2)
		indexi  = 4 + nnodes + nelems * 3 + (j-1)
		indexr  = nnodes * 2 + nmatss * 2 + (j-1)*3
		nnodes  = nnode (idb, rdb)
		nelems  = nelem (idb, rdb)
		nmatss  = nmats (idb, rdb)
