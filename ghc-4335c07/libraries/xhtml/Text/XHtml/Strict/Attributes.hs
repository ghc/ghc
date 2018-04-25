{-# OPTIONS_HADDOCK hide #-}

module Text.XHtml.Strict.Attributes where

import Text.XHtml.Internals

-- * Attributes in XHTML Strict

action              :: String -> HtmlAttr
align               :: String -> HtmlAttr

alt                 :: String -> HtmlAttr
altcode             :: String -> HtmlAttr
archive             :: String -> HtmlAttr
base                :: String -> HtmlAttr
border              :: Int    -> HtmlAttr
bordercolor         :: String -> HtmlAttr
cellpadding         :: Int    -> HtmlAttr
cellspacing         :: Int    -> HtmlAttr
checked             ::           HtmlAttr
codebase            :: String -> HtmlAttr
cols                :: String -> HtmlAttr
colspan             :: Int    -> HtmlAttr
content             :: String -> HtmlAttr
coords              :: String -> HtmlAttr
disabled            ::           HtmlAttr
enctype             :: String -> HtmlAttr
height              :: String -> HtmlAttr
href                :: String -> HtmlAttr
hreflang            :: String -> HtmlAttr
httpequiv           :: String -> HtmlAttr
identifier          :: String -> HtmlAttr
ismap               ::           HtmlAttr
lang                :: String -> HtmlAttr
maxlength           :: Int    -> HtmlAttr
method              :: String -> HtmlAttr
multiple            ::           HtmlAttr
name                :: String -> HtmlAttr
nohref              ::           HtmlAttr
rel                 :: String -> HtmlAttr
rev                 :: String -> HtmlAttr
rows                :: String -> HtmlAttr
rowspan             :: Int    -> HtmlAttr
rules               :: String -> HtmlAttr
selected            ::           HtmlAttr
shape               :: String -> HtmlAttr
size                :: String -> HtmlAttr
src                 :: String -> HtmlAttr
theclass            :: String -> HtmlAttr
thefor              :: String -> HtmlAttr
thestyle            :: String -> HtmlAttr
thetype             :: String -> HtmlAttr
title               :: String -> HtmlAttr
usemap              :: String -> HtmlAttr
valign              :: String -> HtmlAttr
value               :: String -> HtmlAttr
width               :: String -> HtmlAttr

action              =   strAttr "action"
align               =   strAttr "align"
alt                 =   strAttr "alt"
altcode             =   strAttr "altcode"
archive             =   strAttr "archive"
base                =   strAttr "base"
border              =   intAttr "border"
bordercolor         =   strAttr "bordercolor"
cellpadding         =   intAttr "cellpadding"
cellspacing         =   intAttr "cellspacing"
checked             = emptyAttr "checked"
codebase            =   strAttr "codebase"
cols                =   strAttr "cols"
colspan             =   intAttr "colspan"
content             =   strAttr "content"
coords              =   strAttr "coords"
disabled            = emptyAttr "disabled"
enctype             =   strAttr "enctype"
height              =   strAttr "height"
href                =   strAttr "href"
hreflang            =   strAttr "hreflang"
httpequiv           =   strAttr "http-equiv"
identifier          =   strAttr "id"
ismap               = emptyAttr "ismap"
lang                =   strAttr "lang"
maxlength           =   intAttr "maxlength"
method              =   strAttr "method"
multiple            = emptyAttr "multiple"
name                =   strAttr "name"
nohref              = emptyAttr "nohref"
rel                 =   strAttr "rel"
rev                 =   strAttr "rev"
rows                =   strAttr "rows"
rowspan             =   intAttr "rowspan"
rules               =   strAttr "rules"
selected            = emptyAttr "selected"
shape               =   strAttr "shape"
size                =   strAttr "size"
src                 =   strAttr "src"
theclass            =   strAttr "class"
thefor              =   strAttr "for"
thestyle            =   strAttr "style"
thetype             =   strAttr "type"
title               =   strAttr "title"
usemap              =   strAttr "usemap"
valign              =   strAttr "valign"
value               =   strAttr "value"
width               =   strAttr "width"


