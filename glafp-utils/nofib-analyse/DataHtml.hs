-------------------------------------------------------------------------------
-- $Id: DataHtml.hs,v 1.1 1999/11/12 11:54:17 simonmar Exp $
--
-- Copyright (c) 1999 Andy Gill
-------------------------------------------------------------------------------

module DataHtml (
	Html, HtmlName, HtmlAttr, HtmlTable,
	(+++), verbatim, {- tag, atag, -} noHtml, primHtml, 
	concatHtml, htmlStr, htmlLine,
	h1,h2,h3,h4,h5,h6, 	
	font, bold, anchor, header, body, theTitle, paragraph, italics,
	ul, tt,
	bar, meta, li,
	{- tr, int, percent -}
	color, bgcolor, href, name, title, height, width, align, valign,
	border, size, cellpadding, cellspacing,
	p, hr, copyright, spaceHtml, 
	renderHtml, 
	cellHtml, (+/+), above, (+-+), beside, aboves, besides,		
	renderTable, simpleTable, 
	) where

import qualified OptTable as OT

infixr 5 +++	-- appending Html
infixr 3 +/+	-- combining HtmlTable
infixr 4 +-+	-- combining HtmlTable

data Html
	= HtmlAppend Html Html		  -- Some Html, followed by more text
	| HtmlVerbatim Html		  -- Turn on or off smart formating
	| HtmlEmpty			  -- Nothing!
	| HtmlNestingTag HtmlName [HtmlAttr] Html
	| HtmlSimpleTag  HtmlName [HtmlAttr]
	| HtmlString String
		deriving (Show)

{-
 - A important property of Html is all strings inside the
 - structure are already in Html friendly format.
 - For example, use of &gt;,etc.
 -}

type HtmlName	= String
type HtmlAttr	= (HtmlName,Either Int String)
type HtmlTable	= OT.OptTable (Int -> Int -> Html)

------------------------------------------------------------------------------
-- Interface
------------------------------------------------------------------------------

-- primitive combinators
(+++) 		:: Html -> Html 		-> Html
verbatim	:: Html 			-> Html
tag 		:: String -> [HtmlAttr] -> Html -> Html
atag 		:: String -> [HtmlAttr] 	-> Html
noHtml  	:: 				   Html
primHtml 	:: String 			-> Html

-- useful combinators
concatHtml 		:: [Html] 			-> Html
htmlStr, htmlLine 	:: String 			-> Html

-- html constructors
h1,h2,h3,h4,h5,h6 	:: [HtmlAttr] -> Html		-> Html
font, bold, anchor, 
 header, body, 
 theTitle, paragraph,
 italics, ul, tt	:: [HtmlAttr] -> Html		-> Html
bar, meta, li		:: [HtmlAttr]			-> Html

-- html attributes
str 			:: String -> String 		-> HtmlAttr
int 			:: String -> Int 		-> HtmlAttr
percent 		:: String -> Int 		-> HtmlAttr

color, bgcolor, href,
 name, title, height,
 width, align, valign	:: String			-> HtmlAttr

border, size,
 cellpadding,
 cellspacing		:: Int				-> HtmlAttr 

-- abbriviations

p			:: Html				-> Html
hr			::				   Html
copyright		::				   Html
spaceHtml		::				   Html

-- rendering
renderHtml 		:: Html -> String

-- html tables
cellHtml		:: [HtmlAttr] -> Html 		-> HtmlTable
(+/+),above,
 (+-+),beside 		:: HtmlTable -> HtmlTable 	-> HtmlTable
aboves, besides		:: [HtmlTable] 			-> HtmlTable
renderTable 		:: [HtmlAttr] -> HtmlTable 	-> Html
simpleTable 		:: [HtmlAttr] -> [HtmlAttr] -> [[Html]] 
							-> Html

------------------------------------------------------------------------------
-- Basic, primitive combinators

-- This is intentionally lazy in the second argument.
(HtmlAppend x y) +++ z = x +++ (y +++ z)
(HtmlEmpty)      +++ z = z
x                +++ z = HtmlAppend x z

verbatim 	= HtmlVerbatim
tag 		= HtmlNestingTag
atag 		= HtmlSimpleTag
noHtml 		= HtmlEmpty

-- This is not processed for special chars. 
-- It is used to output them, though!
primHtml 	= HtmlString

------------------------------------------------------------------------------
-- Useful Combinators

concatHtml = foldr (+++) noHtml
-- Processing Strings into Html friendly things.
-- This converts a string to an Html.
htmlStr = primHtml . htmlizeStr

-- This converts a string, but keeps spaces as non-line-breakable
htmlLine = primHtml . concat . map htmlizeChar2
   where 
	htmlizeChar2 ' ' = "&nbsp;"
	htmlizeChar2 c   = htmlizeChar c

-- Local Utilites
htmlizeStr :: String -> String
htmlizeStr = concat . map htmlizeChar

htmlizeChar :: Char -> String
htmlizeChar '<' = "&gt;"
htmlizeChar '>' = "&lt;"
htmlizeChar '&' = "&amb;"
htmlizeChar '"' = "&quot;"
htmlizeChar c   = [c]

------------------------------------------------------------------------------
-- Html Constructors
h n = tag ("h" ++ show n)

-- Isn't Haskell great!
[h1,h2,h3,h4,h5,h6] = map h [1..6]

-- tags
font   		= tag "font"
bold		= tag "b"
anchor		= tag "a"
header 		= tag "header"
body   		= tag "body"
theTitle	= tag "title"
paragraph	= tag "p"
italics		= tag "i"
ul		= tag "ul"
tt		= tag "tt"

bar		= atag "hr"
meta		= atag "meta"
li		= atag "li"

------------------------------------------------------------------------------
-- Html Attributes

-- note: the string is presumed to be formated for output
--str :: String -> String -> HtmlAttr
str n s = (n,Right s)

--int :: String -> Int -> HtmlAttr
int n v = (n,Left v)

--percent :: String -> Int -> HtmlAttr
percent n v = str n (show v ++ "%")

-- attributes
color   	= str "color"
bgcolor 	= str "bgcolor"
href		= str "href"
name		= str "name"
title		= str "tile"
height  	= str "height" 
width   	= str "width"
align   	= str "align"
valign  	= str "valign"

border  	= int "border" 
size    	= int "size"
cellpadding    	= int "cellpadding"
cellspacing    	= int "cellspacing"

------------------------------------------------------------------------------
-- abbriviations
p 		= paragraph []
hr		= atag "hr" []
copyright	= primHtml "&copy;"
spaceHtml 	= primHtml "&nbsp;"

------------------------------------------------------------------------------
-- Rendering

renderHtml html = renderHtml' html (Just 0) ++ footerMessage

footerMessage 
   = "\n<!-- Generated using the Haskell HTML generator package HaskHTML -->\n"

renderHtml' (HtmlAppend html1 html2) d
	= renderHtml' html1 d ++ renderHtml' html2 d
renderHtml' (HtmlVerbatim html1) d
	= renderHtml' html1 Nothing
renderHtml' (HtmlEmpty) d = ""
renderHtml' (HtmlSimpleTag name attr) d
	= renderTag True name attr d
renderHtml' (HtmlNestingTag name attr html) d
	= renderTag True name attr d ++ renderHtml' html (incDepth d) ++
	  renderTag False name [] d
renderHtml' (HtmlString str) _ = str

incDepth :: Maybe Int -> Maybe Int
incDepth = fmap (+4)

-- This prints the tags in 
renderTag :: Bool -> HtmlName -> [HtmlAttr] -> Maybe Int -> String
renderTag x name attrs n = start ++ base_spaces ++ open ++ name ++ rest attrs ++ ">"
  where
	open = if x then "<" else "</"
	(start,base_spaces,sep) = case n of
		 	      Nothing -> ("",""," ")
			      Just n ->  ("\n",replicate n ' ',"\n")
			
	rest []            = ""
	rest [(tag,val)]   = " " ++ tag ++ "=" ++ myShow val 
	rest (hd:tl)       = " " ++ showPair hd ++ sep ++
		  foldr1 (\ x y -> x ++ sep ++ y)
			 [ base_spaces ++ replicate (1 + length name + 1) ' ' 
				++ showPair p | p <- tl ]

	showPair :: HtmlAttr -> String
	showPair (tag,val) = tag ++ replicate (tagsz - length tag) ' ' ++ 
			" = " ++ myShow val 
	myShow (Left n) = show n
	myShow (Right s) = "\"" ++ s ++ "\""

	tagsz = maximum (map (length.fst) attrs)

------------------------------------------------------------------------------
-- Html table related things

cellHtml attr html = OT.single cellFn
    where
  	cellFn x y = tag "td" (addX x (addY y attr)) html
	addX 1 rest = rest
	addX n rest = int "colspan" n : rest
	addY 1 rest = rest
	addY n rest = int "rowspan" n : rest

above 	= OT.above
(+/+) 	= above
beside	= OT.beside
(+-+)	= beside

{-
 - Note: Both aboves and besides presume a non-empty list.
 -}

aboves = foldl1 (+/+)
besides = foldl1 (+-+)

-- renderTable takes the HtmlTable, and renders it back into
-- and Html object. The attributes are added to the outside
-- table tag.

renderTable attr theTable
 	= table [row [theCell x y | (theCell,(x,y)) <- theRow ] 
			| theRow <- OT.getMatrix theTable]
   where
	row :: [Html] -> Html
	row  = tag "tr" [] . concatHtml

	table :: [Html] -> Html
	table = tag "table" attr . concatHtml

-- If you cant be bothered with the above, then you
-- can build simple tables with this.
-- Just provide the attributes for the whole table,
-- attributes for the cells (same for every cell),
-- and a list of list of cell contents,
-- and this function will build the table for you.
-- It does presume that all the lists are non-empty,
-- and there is at least one list.
--  
-- Different length lists means that the last cell
-- gets padded. If you want more power, then
-- use the system above.

simpleTable attr cellAttr
	= renderTable attr 
	. aboves
	. map (besides . map (cellHtml cellAttr))

	
------------------------------------------------------------------------------
