module Postscript(Output, draw)where
import Numbers
import Vectors
import EdgePlate
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Postscript driver for hiddenline program
-- Last update 14 Januari 92

type Output = String

draw :: [Edge] -> Output
draw ls = iniplot ++ plot ls exiplot
	where
	iniplot = "\nerasepage gsave 100 100 translate 0 setlinewidth newpath\n"
	exiplot = "\nstroke grestore\n"

plot		   :: [Edge] -> ShowS
plotFrom :: Vector -> [Edge] -> ShowS
moveTo,lineTo :: Vector -> ShowS
flush :: ShowS

plot []		= \s->s
plot ls@(l:_)	= moveTo (s(l)) . plotFrom (s(l)) ls

plotFrom currentPoint [] = \s->s
plotFrom currentPoint (l:ls)
	| s(l) == currentPoint = lineTo (t(l)) . plotFrom (t(l)) ls
	| s(l) /= currentPoint = flush . plot (l:ls)
moveTo v = shows (x(v)) . showChar ' ' . shows (y(v)) . showString " moveto "
lineTo v = shows (x(v)) . showChar ' ' . shows (y(v)) . showString " lineto "
flush	 = showString "\nstroke newpath\n"
