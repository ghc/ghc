{---------------------------------------------------------------
Daan Leijen (c) 2001.  daan@cs.uu.nl

$Revision: 1.1 $
$Author: panne $
$Date: 2002/05/31 12:22:35 $
---------------------------------------------------------------}
module Main where

import While( prettyWhileFromFile )

main  = prettyWhileFromFile "fib.wh"
