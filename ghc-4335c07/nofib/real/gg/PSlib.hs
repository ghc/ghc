module PSlib where

-- This module implements provision of 
-- control of postscript

type Postscript = String

data Point = Pt Int Int  deriving (Eq,Show{-was:Text-})

initialise header = header ++  "/SMALL /Helvetica findfont 4 scalefont def\n" ++
				"/SMALLBOLD /Helvetica-Bold findfont 4 scalefont def\n" ++
				"/SMALLITALIC /Helvetica-Oblique findfont 4 scalefont def\n" ++
				"/NORM /Helvetica findfont 5 scalefont def\n" ++
				"/BOLD /Helvetica-Bold findfont 5 scalefont def\n" ++
				"/LARGE /Helvetica-Bold findfont 11 scalefont def\n" ++
				"NORM setfont\n"
                ++ setcms ++ stdProcedures ++ thinlines

setfont str = str ++ " setfont\n"

stdheader :: Postscript
stdheader = "%!PS-Adobe-2.0\n%%Created by Haskell Graph Package\n"

gslandscape = ""
landscape = translate 8 290 ++ rotate 270 ++ translate 20 10 ++ "0.9 0.9 scale\n"
portrait = ""

stdProcedures = rightshow ++ centreshow


drawObject :: [Point] -> Postscript
drawObject (pts) = newpath ++ moveto (Pt 0 0) ++ concat (map lineto pts) ++ 
			thinlines  ++ stroke

fillObject :: [Point] -> Postscript
fillObject (pts) = newpath ++ moveto (Pt 0 0) ++ concat (map lineto pts) ++ 
			closepath ++ fill ++ stroke

fillBox :: Point -> Int -> Int  -> Int -> Postscript
fillBox pt dx dy c = newpath ++ moveto pt ++ rlineto 0 dy ++ rlineto dx 0 ++ 
			rlineto 0 (-dy) ++ closepath ++ setgray c ++ fill

drawBox :: Point -> Int -> Int -> Postscript
drawBox pt dx dy = thinlines ++ newpath ++ moveto pt ++ rlineto 0 dy ++ rlineto dx 0 ++ 
			rlineto 0 (-dy) ++ closepath ++ stroke

rjustify str = "("++str++") rightshow\n"
cjustify str = "("++str++") centreshow\n"

-- basic prodedures

rightshow = "/rightshow\n{dup stringwidth pop\n0 exch sub\n0 rmoveto\nshow } def \n"
centreshow = "/centreshow\n{dup stringwidth pop\n0 exch sub\n2 div\n0 rmoveto\nshow } def \n"

-- basic functions.



fill = "fill\n"
stroke = "stroke\n"
closepath = "closepath\n"
newpath = "newpath\n"
showpage = "showpage\n\n"
gsave = "gsave\n"
grestore = "grestore\n"

text t = setgray 0 ++ "("++t++") show\n"

setgray 0 = "0 setgray\n"
setgray 10 = "1 setgray\n"
setgray n = "."++show n++" setgray\n"

moveto (Pt x y) = psCommand "moveto" [x,y] 

rmoveto x y = psCommand "rmoveto" [x,y]

lineto :: Point -> Postscript
lineto (Pt x y) = psCommand "lineto" [x,y]

rlineto x y = psCommand "rlineto" [x,y] 

setlinewidth n = psCommand "setlinewidth" [n]

thinlines = "0.2 setlinewidth\n"

rotate n = psCommand "rotate" [n]

psCommand c args = concat (map f args) ++c++"\n"
	where f x = show x++" "


translate x y = psCommand "translate" [x,y]

scale x y = psCommand "scale" [x,y]

setcms = "2.84584 2.84584 scale\n"




