-- LML original: Sandra Foubister, 1990
-- Haskell translation: Colin Runciman, May 1991

module Progfuns (tileprompt, tilequit, tiletrans, potatotile, State) where

import Mgrfuns
import Drawfuns
import Geomfuns
import Psfuns
import Interstate
import Auxprogfuns
import Layout
import Tilefuns
import Help

tileprompt :: a -> [Char]
tileprompt _  = ""

tilequit :: a -> [[Char]] -> Bool
tilequit _ (('q':_):_) = True
tilequit _ []          = True
tilequit _           _ = False

type State = ([([Int],[Int])], Int, [((Int,Int),Int)])  --CR needs abstraction! 
type Trans = State -> [[Char]] -> ([Char], State, [[Char]])

tiletrans :: Trans

tiletrans (dlist,sel,tilist) (('m':'s':'a':' ':rest):inpt) =
    	if      intsave   x y then doo tsave
     	else if intclear  x y then doo tclear
        else if intget    x y then doo tget
	else if intile4   x y then doo t4'
        else if inquit    x y then doo q
        else if inbigtile x y then doo delsq
        else if intoalter x y then doo tofiddle'
        else if intotile  x y then doo totile'
        else if intodraw  x y then doo todesign'
        else if inpicarea x y then doo sel'
        else if inhelp    x y then doo tohelp'
        else tiletrans (dlist,sel,tilist) inpt
        where
        [x,y]  = stoil rest
	doo fun = fun rest (dlist,sel,tilist) inpt

tiletrans (dlist,sel,tilist) (('m':'s':'b':' ':rest):inpt) =
	if inbigtile x y then inv' rest (dlist,sel,tilist) inpt
        else tiletrans (dlist,sel,tilist) (('m':'s':'a':' ':rest):inpt)
	where
        [x,y]  = stoil rest

tiletrans (dlist,sel,tilist) (('m':'s':'c':' ':rest):inpt) =
    if      indesign x y then doo rl
    else if indsave  x y then doo dsave
    else if indclear x y then doo dclear
    else if indget   x y then doo dget
    else tiletrans (dlist,sel,tilist) (('m':'s':'a':' ':rest):inpt)
    where
    [x,y]  = stoil rest
    doo fun = fun rest (dlist,sel,tilist) inpt

tiletrans state (('m':'s':'d':' ':rest):inpt) =
       (inithelp ++ out,state,inpt)
       where
       [x,y] = stoil rest
       cf str = clear ++ str
       out = if      intodraw  x y then cf helpdraw
	     else if intotile  x y then cf helptile
	     else if intoalter x y then cf helpalter
	     else if intsave   x y then cf helptsave
	     else if intclear  x y then cf helptclear
	     else if intget    x y then cf helptget
	     else if intile4   x y then cf helpt4
	     else if inquit    x y then cf helpquit
	     else if inbigtile x y then cf helpbt
	     else if inpicarea x y then cf helppic
	     else if indesign  x y then cf helpdesign
	     else if indsave   x y then cf helpdsave
	     else if indclear  x y then cf helpdclear
	     else if indget    x y then cf helpdget
	     else if inhelp    x y then cf inithelp
	     else cf errmes

tiletrans (dlist,sel,tilist) (('c':'s':' ':rest):inpt) =
	if indgrid nstoilrest then
	  (linecircs ++ wnstoilrest,(newele:dlist,sel,tilist),inpt) 
	else
	  ("",(dlist,sel,tilist),inpt)
        where
	nearline [x0,y0,x1,y1] = [nearx x0, neary y0, nearx x1, neary y1]
	nstoilrest = nearline (stoil rest)
	wnstoilrest = wline nstoilrest
	cssr = cs nstoilrest
        newele  = (nstoilrest,snd cssr)
	linecircs = fst cssr

         
tiletrans (dlist,sel,tilist) (('r':'o':'t':' ':rest):inpt) =
  	if lsrest == [0,0] then
  	  ("",(dlist,sel,tilist),inpt)
        else
  	  ( undo (put lsrest (orient xymax oldas wcoords)) ++
            put lsrest (orient xymax (rot oldas) wcoords)
          , (dlist,sel,newtilist)
          , inpt )
        where
  	stoilrest = stoil rest
	wcoords = map (map wscale) (map fst dlist)
  	oldas = assoc (sqid stoilrest) tilist
  	newtilist = newas (sqid stoilrest) (rot oldas) tilist
  	lsrest = btlocate stoilrest
  
tiletrans (dlist,sel,tilist) (('p':'u':'t':' ':rest):inpt) =
        if lsrest == [0,0] then
  	  ("",(dlist,sel,tilist),inpt)
        else
	  ( undo (put lsrest (orient xymax oldas wcoords)) ++
            put lsrest (orient xymax sel wcoords)
          , (dlist,sel,newtilist)
          , inpt )
        where
  	stoilrest = stoil rest
        newtilist = newas (sqid stoilrest) sel tilist 
        lsrest = btlocate stoilrest
	coords = map fst dlist
	oldas = assoc (sqid stoilrest) tilist
	wcoords = map (map wscale) coords

tiletrans state ("":inpt) = (helpend ++ todesign,state,inpt)

tiletrans state (_:inpt)= ("",state,inpt)

todesign', totile', tofiddle', tohelp' :: [Char] -> Trans

todesign' _ (dlist,sel,tilist) inpt =
	( cleara picarea ++
          picgrid ++
          cleara tilearea ++
          tpgrid ++
          showoris (map fst dlist) 1 ++
          todesign
        , (dlist,sel,tilist)
        , inpt )

totile' _  (dlist,sel,tilist) inpt =
	( concat (map (showoris coords) [1..8]) ++ totile
        , (dlist,sel,tilist)
        , inpt)
	where
        coords = map fst dlist

tofiddle' _  (dlist,sel,tilist) inpt = (tofiddle,(dlist,sel,tilist),inpt)

tohelp' _ (dlist,sel,tilist) inpt = (tohelp,(dlist,sel,tilist),inpt)

rl, dsave, dclear, dget :: [Char] -> Trans

rl rest (dlist,sel,tilist) inpt =
	(out,(newdlist,sel,tilist),inpt)
	where
        (out,newdlist) = deline dlist (stoil rest)
	
dsave _ state inpt = ("", state, inpt) --CR: dsave does nothing, for now 
-- dsave _ (dlist,sel,tilist) inpt =
-- 	(out,(dlist,sel,tilist),t)
--         where
-- 	(h:t) = inpt
-- 	out = menumark "dsave" ++
--               prompt ++
--               tofile h ++
--               totext (map fst dlist) ++
--               "TOSTDOUT" ++
--               clearit ++
--               unmenumark "dsave"

dclear rest (dlist,sel,tilist) inpt =
        ( menumark "dclear" ++ newdraw ++ unmark sel ++ unmenumark "dclear"
        , ([],1,initalist)
        , inpt ) 


dget _ state inpt = ("", state, inpt) --CR: dget does nothing, for now 
-- dget _ (dlist,sel,tilist) inpt =
-- 	( menumark "dget" ++ prompt ++ out ++ unmenumark "dget"
--         , (newd,news,newt)
--         , i )
--         where
-- 	(h:t) = inpt
--         conddraw = if dlist == [] then "" else newdraw
-- 	(out,(newd,news,newt),i) =
--           case openfile h of
-- 	    No emsg  -> ( emsg ++ "\n" ++ delay 1000 ++ clearit
--                         , (dlist,sel,tilist)
--                         , t )
-- 	    Yes file -> ( clearit ++ conddraw ++ out'
--                         , s
--                         , inp )
-- 		        where
--                         (out',s,inp) =
--                           tiletrans ([],sel,tilist) (lines file ++ t))

sel', delsq, inv' :: [Char] -> Trans

sel' rest (dlist,sel,slist) inpt =
  	(unmark sel ++ mark newsel, (dlist,newsel,slist), inpt)
	where
  	new = inbox (stoil rest)
  	newsel = if new == 0 then sel else new

delsq rest (dlist,sel,tilist) inpt =
	( undo (put lsrest (orient xymax oldas wcoords))
        , (dlist,sel,newtilist)
        , inpt )
        where
	wcoords = map (map wscale) (map fst dlist)
	stoilrest = stoil rest
	oldas = assoc (sqid stoilrest) tilist
	lsrest = btlocate stoilrest
	newtilist = newas (sqid stoilrest) 0 tilist

inv' rest (dlist,sel,tilist) inpt =
  	if lsrest == [0,0] then
  	  ("",(dlist,sel,tilist),inpt)
        else 
  	  ( undo (put lsrest (orient xymax oldas wcoords)) ++
            put lsrest (orient xymax (inv oldas) wcoords)
          , (dlist,sel,newtilist)
          , inpt ) 
        where
  	stoilrest = stoil rest
	wcoords = map (map wscale) (map fst dlist)
  	oldas = assoc (sqid stoilrest) tilist
  	newtilist = newas (sqid stoilrest) (inv oldas) tilist
  	lsrest = btlocate stoilrest
  
tclear, tsave, tget, t4' :: [Char] -> Trans

tclear _ (dlist,sel,tilist) inpt =
    ( menumark "tclear" ++
      cleara tilearea ++
      tpgrid ++
      totile ++
      unmenumark "tclear"
    , (dlist,sel,initalist)
    , inpt )

tsave _ state inpt = ("", state, inpt) --CR: tsave does nothing, for now 
-- tsave _ (dlist,sel,tilist) inpt =
--     ( menumark "tsave" ++
--       prompt ++
--       tofile h ++
--       pos8head (tops dlist) ++
--       introline ++
--       concat . (map lf) ((reverse . ineights) (map (turn . snd) tilist)) ++
--       "\nshowpage\n" ++
--       tofile (h ++ ".pat") ++
--       (tpatformat . ineights . map snd) tilist ++
--       "TOSTDOUT" ++
--       clearit ++
--       unmenumark "tsave")
--     , (dlist,sel,tilist)
--     , t )
--     where
--     (h:t) = inpt
--     tops = (map (map wwscale)) . (map fst)

tget _ state inpt = ("", state, inpt) --CR: tget does nothing, for now 
-- tget rest (dlist,sel,tilist) inpt =
-- 	(out,(dlist,sel,(snd infromfile)),i)
--         where
-- 	(h:i) = inpt
-- 	wcoords = map (map wscale) (map fst dlist)
-- 	patfile = if h == "" then h
--                   else if head h == '*' then lib ++ tail h
-- 	          else h ++ ".pat"
--         lib = "/n/johann/usr2/openday/reptile/potato/" --CR now where?
-- 	infromfile = case openfile patfile of
-- 		       No emsg -> ( emsg ++ "\n" ++ delay 1000 ++ tpgrid
--                                   , tilist )
-- 		       Yes ls8 -> ( concat (map2 put (map squas alistind) 
-- 				       (pam (orient xymax) orilist wcoords))
--                                   , zip alistind orilist )
--                                   where
--                                   orilist = concat (map stoil (lines ls8))
-- 	-- have omitted @ tgrid after cleara tilearea
-- 	out = menumark "tget" ++
--               cleara tilearea ++
--               prompt ++
--               fst infromfile ++
--               clearit ++
--               unmenumark "tget"

t4' _ (dlist,sel,tilist) inpt =
    (out,(dlist,sel,newtilist),inpt)
    where
    orilist = pam assoc [(0,0),(0,1),(1,0),(1,1)] tilist
    wcoords = map (map wscale) (map fst dlist)
    pic = t4 (pam (orient xymax) orilist wcoords)
    newtilist = zip alistind (concrep 4 (cr12 ++ cr34))
                where
                cr12 = concrep 4 [n1,n2]
                cr34 = concrep 4 [n3,n4]
                [n1,n2,n3,n4] = orilist
    out = menumark "t4" ++
          cleara tilearea ++ 
          tile tpxorig tpyorig 4 4 pic ++
          unmenumark "t4"

assoc :: (Eq a) => a -> [(a,b)] -> b 
assoc i ((j,v):ivs) = if i == j then v else assoc i ivs

q :: [Char] -> Trans
q _ state _ = ("",state,[])

{- UNUSED:
prompt :: [Char]
prompt = clearit ++
	 vistextreg ++
	 func 4 ++
	 stringto 0 50 600 "Type in filename: " ++
	 func 15

clearit :: [Char]
clearit = cleara textarea

totext :: [[Int]] -> [Char]
totext = concat . map putline

putline :: [Int] -> [Char]
putline [x0,y0,x1,y1] = "cs " ++  show x0 ++ " " ++ show y0 ++ " " ++
				  show x1 ++ " " ++ show y1 ++ "\n"
-}

-- newdraw clears and redraws the design area, and the picarea.
-- also the tile area
-- It is used by dclear and by get
newdraw :: [Char]
newdraw = cleara designarea ++
          dpgrid ++
          cleara picarea ++
          picgrid ++
          cleara tilearea ++
          tpgrid ++
          invisibletext ++
          todesign

potatotile :: State -> [[Char]] -> [Char]
potatotile = inter tileprompt tilequit tiletrans

stoil :: [Char] -> [Int]
stoil = map read . words



