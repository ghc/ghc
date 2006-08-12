-----------------------------------------------------------------------------
-- 
-- Module      :  Text.Html
-- Copyright   :  (c) Andy Gill, and the Oregon Graduate Institute of 
--		  Science and Technology, 1999-2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  Andy Gill <andy@galconn.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- An Html combinator library
--
-----------------------------------------------------------------------------

module Html (
      module Html,
      ) where

import qualified BlockTable as BT

import Data.Char (isAscii, ord)
import Numeric (showHex)

infixr 2 +++  -- combining Html
infixr 7 <<   -- nesting Html
infixl 8 !    -- adding optional arguments


-- A important property of Html is that all strings inside the
-- structure are already in Html friendly format.
-- For example, use of &gt;,etc.

data HtmlElement
{-
 -    ..just..plain..normal..text... but using &copy; and &amb;, etc.
 -}
      = HtmlString String
{-
 -    <thetag {..attrs..}> ..content.. </thetag>
 -}
      | HtmlTag {                   -- tag with internal markup
              markupTag      :: String,
              markupAttrs    :: [HtmlAttr],
              markupContent  :: Html
              }

{- These are the index-value pairs.
 - The empty string is a synonym for tags with no arguments.
 - (not strictly HTML, but anyway).
 -}


data HtmlAttr = HtmlAttr String String


newtype Html = Html { getHtmlElements :: [HtmlElement] }

-- Read MARKUP as the class of things that can be validly rendered
-- inside MARKUP tag brackets. So this can be one or more Html's,
-- or a String, for example.

class HTML a where
      toHtml     :: a -> Html
      toHtmlFromList :: [a] -> Html

      toHtmlFromList xs = Html (concat [ x | (Html x) <- map toHtml xs])

instance HTML Html where
      toHtml a    = a

instance HTML Char where
      toHtml       a = toHtml [a]
      toHtmlFromList []  = Html []
      toHtmlFromList str = Html [HtmlString (stringToHtmlString str)]

instance (HTML a) => HTML [a] where
    toHtml xs = toHtmlFromList xs

class ADDATTRS a where
      (!) :: a -> [HtmlAttr] -> a

instance (ADDATTRS b) => ADDATTRS (a -> b) where
      (!) fn attr = \ arg -> fn arg ! attr

instance ADDATTRS Html where
      (!) (Html htmls) attr = Html (map addAttrs htmls)
        where
              addAttrs html =
                  case html of
                       HtmlTag { markupAttrs   = markupAttrs0
                               , markupTag     = markupTag0
                               , markupContent = markupContent0 } ->
                               HtmlTag { markupAttrs   = markupAttrs0 ++ attr
                                       , markupTag     = markupTag0
                                       , markupContent = markupContent0 }
                       _                                         -> html


(<<)            :: (HTML a) => (Html -> b) -> a        -> b
fn << arg = fn (toHtml arg)


concatHtml :: (HTML a) => [a] -> Html
concatHtml as = Html (concat (map (getHtmlElements.toHtml) as))

(+++) :: (HTML a,HTML b) => a -> b -> Html
a +++ b = Html (getHtmlElements (toHtml a) ++ getHtmlElements (toHtml b))

noHtml :: Html
noHtml = Html []


isNoHtml :: Html -> Bool
isNoHtml (Html xs) = null xs


tag  :: String -> Html -> Html
tag str htmls =
    Html [ HtmlTag { markupTag = str,
                     markupAttrs = [],
                     markupContent = htmls }
         ]

itag :: String -> Html
itag str = tag str noHtml

emptyAttr :: String -> HtmlAttr
emptyAttr s = HtmlAttr s ""

intAttr :: String -> Int -> HtmlAttr
intAttr s i = HtmlAttr s (show i)

strAttr :: String -> String -> HtmlAttr
strAttr s t = HtmlAttr s t


{-
foldHtml :: (String -> [HtmlAttr] -> [a] -> a) 
      -> (String -> a)
      -> Html
      -> a
foldHtml f g (HtmlTag str attr fmls) 
      = f str attr (map (foldHtml f g) fmls) 
foldHtml f g (HtmlString  str)           
      = g str

-}
-- Processing Strings into Html friendly things.
-- This converts a String to a Html String.
stringToHtmlString :: String -> String
stringToHtmlString = concatMap fixChar
    where
      fixChar '<' = "&lt;"
      fixChar '>' = "&gt;"
      fixChar '&' = "&amp;"
      fixChar '"' = "&quot;"
      fixChar c
	| isAscii c = [c]
	| otherwise = "&#x" ++ showHex (ord c) ";"

-- ---------------------------------------------------------------------------
-- Classes

instance Show Html where
      showsPrec _ html = showString (prettyHtml html)
      showList htmls   = showString (concat (map show htmls))

instance Show HtmlAttr where
      showsPrec _ (HtmlAttr str val) = 
              showString str .
              showString "=" .
              shows val


-- ---------------------------------------------------------------------------
-- Data types

type URL = String

-- ---------------------------------------------------------------------------
-- Basic primitives

-- This is not processed for special chars. 
-- use stringToHtml or lineToHtml instead, for user strings, 
-- because they  understand special chars, like '<'.

primHtml      :: String                                -> Html
primHtml x    = Html [HtmlString x]

-- ---------------------------------------------------------------------------
-- Basic Combinators

stringToHtml          :: String                       -> Html
stringToHtml = primHtml . stringToHtmlString 

-- This converts a string, but keeps spaces as non-line-breakable

lineToHtml            :: String                       -> Html
lineToHtml = primHtml . concatMap htmlizeChar2 . stringToHtmlString 
   where 
      htmlizeChar2 ' ' = "&nbsp;"
      htmlizeChar2 c   = [c]

-- ---------------------------------------------------------------------------
-- Html Constructors

-- (automatically generated)

address             :: Html -> Html
anchor              :: Html -> Html
applet              :: Html -> Html
area                ::         Html
basefont            ::         Html
big                 :: Html -> Html
blockquote          :: Html -> Html
body                :: Html -> Html
bold                :: Html -> Html
br                  ::         Html
button		    :: Html -> Html
caption             :: Html -> Html
center              :: Html -> Html
cite                :: Html -> Html
ddef                :: Html -> Html
define              :: Html -> Html
dlist               :: Html -> Html
dterm               :: Html -> Html
emphasize           :: Html -> Html
fieldset            :: Html -> Html
font                :: Html -> Html
form                :: Html -> Html
frame               :: Html -> Html
frameset            :: Html -> Html
h1                  :: Html -> Html
h2                  :: Html -> Html
h3                  :: Html -> Html
h4                  :: Html -> Html
h5                  :: Html -> Html
h6                  :: Html -> Html
header              :: Html -> Html
hr                  ::         Html
image               ::         Html
input               ::         Html
italics             :: Html -> Html
keyboard            :: Html -> Html
legend              :: Html -> Html
li                  :: Html -> Html
meta                ::         Html
noframes            :: Html -> Html
olist               :: Html -> Html
option              :: Html -> Html
paragraph           :: Html -> Html
param               ::         Html
pre                 :: Html -> Html
sample              :: Html -> Html
script		    :: Html -> Html
select              :: Html -> Html
small               :: Html -> Html
strong              :: Html -> Html
style               :: Html -> Html
sub                 :: Html -> Html
sup                 :: Html -> Html
table               :: Html -> Html
thetd               :: Html -> Html
textarea            :: Html -> Html
th                  :: Html -> Html
thebase             ::         Html
thecode             :: Html -> Html
thediv              :: Html -> Html
thehtml             :: Html -> Html
thelink             ::         Html
themap              :: Html -> Html
thespan             :: Html -> Html
thetitle            :: Html -> Html
tr                  :: Html -> Html
tt                  :: Html -> Html
ulist               :: Html -> Html
underline           :: Html -> Html
variable            :: Html -> Html

address             =  tag "ADDRESS"
anchor              =  tag "A"
applet              =  tag "APPLET"
area                = itag "AREA"
basefont            = itag "BASEFONT"
big                 =  tag "BIG"
blockquote          =  tag "BLOCKQUOTE"
body                =  tag "BODY"
bold                =  tag "B"
br                  = itag "BR"
button		    =  tag "BUTTON"
caption             =  tag "CAPTION"
center              =  tag "CENTER"
cite                =  tag "CITE"
ddef                =  tag "DD"
define              =  tag "DFN"
dlist               =  tag "DL"
dterm               =  tag "DT"
emphasize           =  tag "EM"
fieldset            =  tag "FIELDSET"
font                =  tag "FONT"
form                =  tag "FORM"
frame               =  tag "FRAME"
frameset            =  tag "FRAMESET"
h1                  =  tag "H1"
h2                  =  tag "H2"
h3                  =  tag "H3"
h4                  =  tag "H4"
h5                  =  tag "H5"
h6                  =  tag "H6"
header              =  tag "HEAD"
hr                  = itag "HR"
image               = itag "IMG"
input               = itag "INPUT"
italics             =  tag "I"
keyboard            =  tag "KBD"
legend              =  tag "LEGEND"
li                  =  tag "LI"
meta                = itag "META"
noframes            =  tag "NOFRAMES"
olist               =  tag "OL"
option              =  tag "OPTION"
paragraph           =  tag "P"
param               = itag "PARAM"
pre                 =  tag "PRE"
sample              =  tag "SAMP"
script		    =  tag "SCRIPT"
select              =  tag "SELECT"
small               =  tag "SMALL"
strong              =  tag "STRONG"
style               =  tag "STYLE"
sub                 =  tag "SUB"
sup                 =  tag "SUP"
table               =  tag "TABLE"
thetd               =  tag "TD"
textarea            =  tag "TEXTAREA"
th                  =  tag "TH"
thebase             = itag "BASE"
thecode             =  tag "CODE"
thediv              =  tag "DIV"
thehtml             =  tag "HTML"
thelink             = itag "LINK"
themap              =  tag "MAP"
thespan             =  tag "SPAN"
thetitle            =  tag "TITLE"
tr                  =  tag "TR"
tt                  =  tag "TT"
ulist               =  tag "UL"
underline           =  tag "U"
variable            =  tag "VAR"

-- ---------------------------------------------------------------------------
-- Html Attributes

-- (automatically generated)

action              :: String -> HtmlAttr
align               :: String -> HtmlAttr
alink               :: String -> HtmlAttr
alt                 :: String -> HtmlAttr
altcode             :: String -> HtmlAttr
archive             :: String -> HtmlAttr
background          :: String -> HtmlAttr
base                :: String -> HtmlAttr
bgcolor             :: String -> HtmlAttr
border              :: Int    -> HtmlAttr
bordercolor         :: String -> HtmlAttr
cellpadding         :: Int    -> HtmlAttr
cellspacing         :: Int    -> HtmlAttr
checked             ::           HtmlAttr
clear               :: String -> HtmlAttr
code                :: String -> HtmlAttr
codebase            :: String -> HtmlAttr
color               :: String -> HtmlAttr
cols                :: String -> HtmlAttr
colspan             :: Int    -> HtmlAttr
compact             ::           HtmlAttr
content             :: String -> HtmlAttr
coords              :: String -> HtmlAttr
enctype             :: String -> HtmlAttr
face                :: String -> HtmlAttr
frameborder         :: Int    -> HtmlAttr
height              :: Int    -> HtmlAttr
href                :: String -> HtmlAttr
hspace              :: Int    -> HtmlAttr
httpequiv           :: String -> HtmlAttr
identifier          :: String -> HtmlAttr
ismap               ::           HtmlAttr
lang                :: String -> HtmlAttr
link                :: String -> HtmlAttr
marginheight        :: Int    -> HtmlAttr
marginwidth         :: Int    -> HtmlAttr
maxlength           :: Int    -> HtmlAttr
method              :: String -> HtmlAttr
multiple            ::           HtmlAttr
name                :: String -> HtmlAttr
nohref              ::           HtmlAttr
noresize            ::           HtmlAttr
noshade             ::           HtmlAttr
nowrap              ::           HtmlAttr
onclick		    :: String -> HtmlAttr
rel                 :: String -> HtmlAttr
rev                 :: String -> HtmlAttr
rows                :: String -> HtmlAttr
rowspan             :: Int    -> HtmlAttr
rules               :: String -> HtmlAttr
scrolling           :: String -> HtmlAttr
selected            ::           HtmlAttr
shape               :: String -> HtmlAttr
size                :: String -> HtmlAttr
src                 :: String -> HtmlAttr
start               :: Int    -> HtmlAttr
target              :: String -> HtmlAttr
text                :: String -> HtmlAttr
theclass            :: String -> HtmlAttr
thestyle            :: String -> HtmlAttr
thetype             :: String -> HtmlAttr
title               :: String -> HtmlAttr
usemap              :: String -> HtmlAttr
valign              :: String -> HtmlAttr
value               :: String -> HtmlAttr
version             :: String -> HtmlAttr
vlink               :: String -> HtmlAttr
vspace              :: Int    -> HtmlAttr
width               :: String -> HtmlAttr

action              =   strAttr "ACTION"
align               =   strAttr "ALIGN"
alink               =   strAttr "ALINK"
alt                 =   strAttr "ALT"
altcode             =   strAttr "ALTCODE"
archive             =   strAttr "ARCHIVE"
background          =   strAttr "BACKGROUND"
base                =   strAttr "BASE"
bgcolor             =   strAttr "BGCOLOR"
border              =   intAttr "BORDER"
bordercolor         =   strAttr "BORDERCOLOR"
cellpadding         =   intAttr "CELLPADDING"
cellspacing         =   intAttr "CELLSPACING"
checked             = emptyAttr "CHECKED"
clear               =   strAttr "CLEAR"
code                =   strAttr "CODE"
codebase            =   strAttr "CODEBASE"
color               =   strAttr "COLOR"
cols                =   strAttr "COLS"
colspan             =   intAttr "COLSPAN"
compact             = emptyAttr "COMPACT"
content             =   strAttr "CONTENT"
coords              =   strAttr "COORDS"
enctype             =   strAttr "ENCTYPE"
face                =   strAttr "FACE"
frameborder         =   intAttr "FRAMEBORDER"
height              =   intAttr "HEIGHT"
href                =   strAttr "HREF"
hspace              =   intAttr "HSPACE"
httpequiv           =   strAttr "HTTP-EQUIV"
identifier          =   strAttr "ID"
ismap               = emptyAttr "ISMAP"
lang                =   strAttr "LANG"
link                =   strAttr "LINK"
marginheight        =   intAttr "MARGINHEIGHT"
marginwidth         =   intAttr "MARGINWIDTH"
maxlength           =   intAttr "MAXLENGTH"
method              =   strAttr "METHOD"
multiple            = emptyAttr "MULTIPLE"
name                =   strAttr "NAME"
nohref              = emptyAttr "NOHREF"
noresize            = emptyAttr "NORESIZE"
noshade             = emptyAttr "NOSHADE"
nowrap              = emptyAttr "NOWRAP"
onclick             =   strAttr "ONCLICK"
rel                 =   strAttr "REL"
rev                 =   strAttr "REV"
rows                =   strAttr "ROWS"
rowspan             =   intAttr "ROWSPAN"
rules               =   strAttr "RULES"
scrolling           =   strAttr "SCROLLING"
selected            = emptyAttr "SELECTED"
shape               =   strAttr "SHAPE"
size                =   strAttr "SIZE"
src                 =   strAttr "SRC"
start               =   intAttr "START"
target              =   strAttr "TARGET"
text                =   strAttr "TEXT"
theclass            =   strAttr "CLASS"
thestyle            =   strAttr "STYLE"
thetype             =   strAttr "TYPE"
title               =   strAttr "TITLE"
usemap              =   strAttr "USEMAP"
valign              =   strAttr "VALIGN"
value               =   strAttr "VALUE"
version             =   strAttr "VERSION"
vlink               =   strAttr "VLINK"
vspace              =   intAttr "VSPACE"
width               =   strAttr "WIDTH"

-- ---------------------------------------------------------------------------
-- Html Constructors

-- (automatically generated)

validHtmlTags :: [String]
validHtmlTags = [
      "ADDRESS",
      "A",
      "APPLET",
      "BIG",
      "BLOCKQUOTE",
      "BODY",
      "B",
      "CAPTION",
      "CENTER",
      "CITE",
      "DD",
      "DFN",
      "DL",
      "DT",
      "EM",
      "FIELDSET",
      "FONT",
      "FORM",
      "FRAME",
      "FRAMESET",
      "H1",
      "H2",
      "H3",
      "H4",
      "H5",
      "H6",
      "HEAD",
      "I",
      "KBD",
      "LEGEND",
      "LI",
      "NOFRAMES",
      "OL",
      "OPTION",
      "P",
      "PRE",
      "SAMP",
      "SELECT",
      "SMALL",
      "STRONG",
      "STYLE",
      "SUB",
      "SUP",
      "TABLE",
      "TD",
      "TEXTAREA",
      "TH",
      "CODE",
      "DIV",
      "HTML",
      "LINK",
      "MAP",
      "TITLE",
      "TR",
      "TT",
      "UL",
      "U",
      "VAR"]

validHtmlITags :: [String]
validHtmlITags = [
      "AREA",
      "BASEFONT",
      "BR",
      "HR",
      "IMG",
      "INPUT",
      "LINK",
      "META",
      "PARAM",
      "BASE"]

validHtmlAttrs :: [String]
validHtmlAttrs = [
      "ACTION",
      "ALIGN",
      "ALINK",
      "ALT",
      "ALTCODE",
      "ARCHIVE",
      "BACKGROUND",
      "BASE",
      "BGCOLOR",
      "BORDER",
      "BORDERCOLOR",
      "CELLPADDING",
      "CELLSPACING",
      "CHECKED",
      "CLEAR",
      "CODE",
      "CODEBASE",
      "COLOR",
      "COLS",
      "COLSPAN",
      "COMPACT",
      "CONTENT",
      "COORDS",
      "ENCTYPE",
      "FACE",
      "FRAMEBORDER",
      "HEIGHT",
      "HREF",
      "HSPACE",
      "HTTP-EQUIV",
      "ID",
      "ISMAP",
      "LANG",
      "LINK",
      "MARGINHEIGHT",
      "MARGINWIDTH",
      "MAXLENGTH",
      "METHOD",
      "MULTIPLE",
      "NAME",
      "NOHREF",
      "NORESIZE",
      "NOSHADE",
      "NOWRAP",
      "REL",
      "REV",
      "ROWS",
      "ROWSPAN",
      "RULES",
      "SCROLLING",
      "SELECTED",
      "SHAPE",
      "SIZE",
      "SRC",
      "START",
      "TARGET",
      "TEXT",
      "CLASS",
      "STYLE",
      "TYPE",
      "TITLE",
      "USEMAP",
      "VALIGN",
      "VALUE",
      "VERSION",
      "VLINK",
      "VSPACE",
      "WIDTH"]

-- ---------------------------------------------------------------------------
-- Html colors

aqua          :: String
black         :: String
blue          :: String
fuchsia       :: String
gray          :: String
green         :: String
lime          :: String
maroon        :: String
navy          :: String
olive         :: String
purple        :: String
red           :: String
silver        :: String
teal          :: String
yellow        :: String
white         :: String

aqua          = "aqua"
black         = "black"
blue          = "blue"
fuchsia       = "fuchsia"
gray          = "gray"
green         = "green"
lime          = "lime"
maroon        = "maroon"
navy          = "navy"
olive         = "olive"
purple        = "purple"
red           = "red"
silver        = "silver"
teal          = "teal"
yellow        = "yellow"
white         = "white"

-- ---------------------------------------------------------------------------
-- Basic Combinators

linesToHtml :: [String]       -> Html

linesToHtml []     = noHtml
linesToHtml (x:[]) = lineToHtml x
linesToHtml (x:xs) = lineToHtml x +++ br +++ linesToHtml xs


-- ---------------------------------------------------------------------------
-- Html abbriviations

primHtmlChar  :: String -> Html
copyright     :: Html
spaceHtml     :: Html
bullet        :: Html
p             :: Html -> Html

primHtmlChar  = \ x -> primHtml ("&" ++ x ++ ";")
copyright     = primHtmlChar "copy"
spaceHtml     = primHtmlChar "nbsp"
bullet        = primHtmlChar "#149"

p             = paragraph

-- ---------------------------------------------------------------------------
-- Html tables

cell :: Html -> HtmlTable
cell h = let
              cellFn x y = h ! (add x colspan $ add y rowspan $ [])
              add 1 _  rest = rest
              add n fn rest = fn n : rest
              r = BT.single cellFn
         in 
              mkHtmlTable r

-- We internally represent the Cell inside a Table with an
-- object of the type
-- \pre{
-- 	   Int -> Int -> Html
-- } 	
-- When we render it later, we find out how many columns
-- or rows this cell will span over, and can
-- include the correct colspan/rowspan command.

newtype HtmlTable 
      = HtmlTable (BT.BlockTable (Int -> Int -> Html))

td :: Html -> HtmlTable
td = cell . thetd

tda :: [HtmlAttr] -> Html -> HtmlTable
tda as = cell . (thetd ! as)

above, beside :: HtmlTable -> HtmlTable -> HtmlTable
above  a b = combine BT.above a b
beside a b = combine BT.beside a b

infixr 3 </>  -- combining table cells 
infixr 4 <->  -- combining table cells
(</>), (<->) :: HtmlTable -> HtmlTable -> HtmlTable
(</>) = above
(<->) = beside

emptyTable :: HtmlTable
emptyTable = HtmlTable BT.empty

aboves, besides :: [HtmlTable] -> HtmlTable
aboves  = foldr above  emptyTable
besides = foldr beside emptyTable

mkHtmlTable :: BT.BlockTable (Int -> Int -> Html) -> HtmlTable
mkHtmlTable r = HtmlTable r

combine :: (BT.BlockTable (Int -> Int -> Html)
	    -> BT.BlockTable (Int -> Int -> Html)
	    -> BT.BlockTable (Int -> Int -> Html))
	-> HtmlTable -> HtmlTable -> HtmlTable
combine fn (HtmlTable a) (HtmlTable b) = mkHtmlTable (a `fn` b)

-- renderTable takes the HtmlTable, and renders it back into
-- and Html object.

renderTable :: BT.BlockTable (Int -> Int -> Html) -> Html
renderTable theTable
      = concatHtml
          [tr << [theCell x y | (theCell,(x,y)) <- theRow ]
                      | theRow <- BT.getMatrix theTable]

instance HTML HtmlTable where
      toHtml (HtmlTable tab) = renderTable tab

instance Show HtmlTable where
      showsPrec _ (HtmlTable tab) = shows (renderTable tab)


-- If you can't be bothered with the above, then you
-- can build simple tables with simpleTable.
-- Just provide the attributes for the whole table,
-- attributes for the cells (same for every cell),
-- and a list of lists of cell contents,
-- and this function will build the table for you.
-- It does presume that all the lists are non-empty,
-- and there is at least one list.
--  
-- Different length lists means that the last cell
-- gets padded. If you want more power, then
-- use the system above, or build tables explicitly.

simpleTable :: HTML a => [HtmlAttr] -> [HtmlAttr] -> [[a]] -> Html
simpleTable attr cellAttr lst
      = table ! attr 
          <<  (aboves 
              . map (besides . map (cell . (thetd ! cellAttr) . toHtml))
              ) lst


-- ---------------------------------------------------------------------------
-- Tree Displaying Combinators
 
-- The basic idea is you render your structure in the form
-- of this tree, and then use treeHtml to turn it into a Html
-- object with the structure explicit.

data HtmlTree
      = HtmlLeaf Html
      | HtmlNode Html [HtmlTree] Html

treeHtml :: [String] -> HtmlTree -> Html
treeHtml colors h = table ! [
                    border 0,
                    cellpadding 0,
                    cellspacing 2] << treeHtml' colors h
     where
      manycolors = scanr (:) []

      treeHtmls :: [[String]] -> [HtmlTree] -> HtmlTable
      treeHtmls c ts = aboves (zipWith treeHtml' c ts)

      treeHtml' :: [String] -> HtmlTree -> HtmlTable
      treeHtml' (_:_) (HtmlLeaf leaf) = cell
                                         (thetd ! [width "100%"] 
                                            << bold  
                                               << leaf)
      treeHtml' (c:cs@(c2:_)) (HtmlNode hopen ts hclose) =
          if null ts && isNoHtml hclose
          then
              hd 
          else if null ts
          then
              hd </> bar `beside` (cell . (thetd ! [bgcolor c2]) << spaceHtml)
                 </> tl
          else
              hd </> (bar `beside` treeHtmls morecolors ts)
                 </> tl
        where
              -- This stops a column of colors being the same
              -- color as the immeduately outside nesting bar.
              morecolors = filter ((/= c).head) (manycolors cs)
              bar = cell (thetd ! [bgcolor c,width "10"] << spaceHtml)
              hd = cell (thetd ! [bgcolor c] << hopen)
              tl = cell (thetd ! [bgcolor c] << hclose)
      treeHtml' _ _ = error "The imposible happens"

instance HTML HtmlTree where
      toHtml x = treeHtml treeColors x

-- type "length treeColors" to see how many colors are here.
treeColors :: [String]
treeColors = ["#88ccff","#ffffaa","#ffaaff","#ccffff"] ++ treeColors


-- ---------------------------------------------------------------------------
-- Html Debugging Combinators
 
-- This uses the above tree rendering function, and displays the
-- Html as a tree structure, allowing debugging of what is
-- actually getting produced.

debugHtml :: (HTML a) => a -> Html
debugHtml obj = table ! [border 0] << (
                  cell (th ! [bgcolor "#008888"] 
                     	<< underline
                       	   << "Debugging Output")
               </>  td << (toHtml (debug' (toHtml obj)))
              )
  where

      debug' :: Html -> [HtmlTree]
      debug' (Html markups) = map debug markups

      debug :: HtmlElement -> HtmlTree
      debug (HtmlString str) = HtmlLeaf (spaceHtml +++
                                              linesToHtml (lines str))
      debug (HtmlTag {
              markupTag = markupTag0,
              markupContent = markupContent0,
              markupAttrs  = markupAttrs0
              }) =
              case markupContent0 of
                Html [] -> HtmlNode hd [] noHtml
                Html xs -> HtmlNode hd (map debug xs) tl
        where
              args = if null markupAttrs0
                     then ""
                     else "  " ++ unwords (map show markupAttrs0) 
              hd = font ! [size "1"] << ("<" ++ markupTag0 ++ args ++ ">")
              tl = font ! [size "1"] << ("</" ++ markupTag0 ++ ">")

-- ---------------------------------------------------------------------------
-- Hotlink datatype

data HotLink = HotLink {
      hotLinkURL        :: URL,
      hotLinkContents   :: [Html],
      hotLinkAttributes :: [HtmlAttr]
      } deriving Show

instance HTML HotLink where
      toHtml hl = anchor ! (href (hotLinkURL hl) : hotLinkAttributes hl)
                      << hotLinkContents hl

hotlink :: URL -> [Html] -> HotLink
hotlink url h = HotLink {
      hotLinkURL = url,
      hotLinkContents = h,
      hotLinkAttributes = [] }


-- ---------------------------------------------------------------------------
-- More Combinators

-- (Abridged from Erik Meijer's Original Html library)

ordList   :: (HTML a) => [a] -> Html
ordList items = olist << map (li <<) items

unordList :: (HTML a) => [a] -> Html
unordList items = ulist << map (li <<) items

defList   :: (HTML a,HTML b) => [(a,b)] -> Html
defList items
 = dlist << [ [ dterm << bold << dt, ddef << dd ] | (dt,dd) <- items ]


widget :: String -> String -> [HtmlAttr] -> Html
widget w n markupAttrs0 = input ! ([thetype w,name n] ++ markupAttrs0)

checkbox :: String -> String -> Html
hidden   :: String -> String -> Html
radio    :: String -> String -> Html
reset    :: String -> String -> Html
submit   :: String -> String -> Html
password :: String           -> Html
textfield :: String          -> Html
afile    :: String           -> Html
clickmap :: String           -> Html

checkbox n v = widget "CHECKBOX" n [value v]
hidden   n v = widget "HIDDEN"   n [value v]
radio    n v = widget "RADIO"    n [value v]
reset    n v = widget "RESET"    n [value v]
submit   n v = widget "SUBMIT"   n [value v]
password n   = widget "PASSWORD" n []
textfield n  = widget "TEXT"     n []
afile    n   = widget "FILE"     n []
clickmap n   = widget "IMAGE"    n []

menu :: String -> [Html] -> Html
menu n choices
   = select ! [name n] << [ option << p << choice | choice <- choices ]

gui :: String -> Html -> Html
gui act = form ! [action act,method "POST"]

-- ---------------------------------------------------------------------------
-- Html Rendering
 
-- Uses the append trick to optimize appending.
-- The output is quite messy, because space matters in
-- HTML, so we must not generate needless spaces.

renderHtml :: (HTML html) => html -> String
renderHtml theHtml =
      renderMessage ++ 
         foldr (.) id (map unprettyHtml
                           (getHtmlElements (tag "HTML" << theHtml))) "\n"

renderMessage :: String
renderMessage =
      "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">\n" ++
      "<!--Rendered using the Haskell Html Library v0.2-->\n"

unprettyHtml :: HtmlElement -> ShowS
unprettyHtml (HtmlString str) = (++) str
unprettyHtml (HtmlTag
              { markupTag = name0,
                markupContent = html,
                markupAttrs = markupAttrs0 })
      = if isNoHtml html && elem name0 validHtmlITags
        then renderTag True name0 markupAttrs0 0
        else (renderTag True name0 markupAttrs0 0
             . foldr (.) id (map unprettyHtml (getHtmlElements html))
             . renderTag False name0 [] 0)

-- Local Utilities
prettyHtml :: (HTML html) => html -> String
prettyHtml theHtml = 
        unlines
      $ concat
      $ map prettyHtml'
      $ getHtmlElements
      $ toHtml theHtml

prettyHtml' :: HtmlElement -> [String]
prettyHtml' (HtmlString str) = [str]
prettyHtml' (HtmlTag
              { markupTag = name0,
                markupContent = html,
                markupAttrs = markupAttrs0 })
      = if isNoHtml html && elem name0 validHtmlITags
        then 
         [rmNL (renderTag True name0 markupAttrs0 0 "")]
        else
         [rmNL (renderTag True name0 markupAttrs0 0 "")] ++ 
          shift (concat (map prettyHtml' (getHtmlElements html))) ++
         [rmNL (renderTag False name0 [] 0 "")]
  where
      shift = map (\x -> "   " ++ x)

rmNL :: [Char] -> [Char]
rmNL = filter (/= '\n')

-- This prints the Tags The lack of spaces in intentunal, because Html is
-- actually space dependant.

renderTag :: Bool -> String -> [HtmlAttr] -> Int -> ShowS
renderTag x name0 markupAttrs0 n r
      = open ++ name0 ++ rest markupAttrs0 ++ ">" ++ r
  where
      open = if x then "<" else "</"
      
      nl = "\n" ++ replicate (n `div` 8) '\t' 
                ++ replicate (n `mod` 8) ' '

      rest []   = nl
      rest attr = " " ++ unwords (map showPair attr) ++ nl

      showPair :: HtmlAttr -> String
      showPair (HtmlAttr tag0 val)
              = tag0 ++ "=\"" ++ val  ++ "\""

