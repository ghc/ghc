{-# OPTIONS_HADDOCK hide #-}

module Text.XHtml.Strict.Elements where

import Text.XHtml.Internals

-- * Elements in XHTML Strict

abbr                :: Html -> Html
acronym             :: Html -> Html
address             :: Html -> Html
anchor              :: Html -> Html
area                ::         Html
bdo                 :: Html -> Html
big                 :: Html -> Html
blockquote          :: Html -> Html
body                :: Html -> Html
bold                :: Html -> Html
br                  ::         Html
button              :: Html -> Html
caption             :: Html -> Html
cite                :: Html -> Html
col                 :: Html -> Html
colgroup            :: Html -> Html
del                 :: Html -> Html
ddef                :: Html -> Html
define              :: Html -> Html
dlist               :: Html -> Html
dterm               :: Html -> Html
emphasize           :: Html -> Html
fieldset            :: Html -> Html
form                :: Html -> Html
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
ins                 :: Html -> Html
italics             :: Html -> Html
keyboard            :: Html -> Html
label               :: Html -> Html
legend              :: Html -> Html
li                  :: Html -> Html
meta                ::         Html
noscript            :: Html -> Html
object              :: Html -> Html
olist               :: Html -> Html
optgroup            :: Html -> Html
option              :: Html -> Html
paragraph           :: Html -> Html
param               ::         Html
pre                 :: Html -> Html
quote               :: Html -> Html
sample              :: Html -> Html
script              :: Html -> Html
select              :: Html -> Html
small               :: Html -> Html
strong              :: Html -> Html
style               :: Html -> Html
sub                 :: Html -> Html
sup                 :: Html -> Html
table               :: Html -> Html
tbody               :: Html -> Html
td                  :: Html -> Html
textarea            :: Html -> Html
tfoot               :: Html -> Html
th                  :: Html -> Html
thead               :: Html -> Html
thebase             ::         Html
thecode             :: Html -> Html
thediv              :: Html -> Html
thehtml             :: Html -> Html
thelink             :: Html -> Html
themap              :: Html -> Html
thespan             :: Html -> Html
thetitle            :: Html -> Html
tr                  :: Html -> Html
tt                  :: Html -> Html
ulist               :: Html -> Html
variable            :: Html -> Html

abbr                =  tag "abbr"
acronym             =  tag "acronym"
address             =  tag "address"
anchor              =  tag "a"
area                = itag "area"
bdo                 =  tag "bdo"
big                 =  tag "big"
blockquote          =  tag "blockquote"
body                =  tag "body"
bold                =  tag "b"
button              =  tag "button"
br                  = itag "br"
caption             =  tag "caption"
cite                =  tag "cite"
col                 =  tag "col"
colgroup            =  tag "colgroup"
ddef                =  tag "dd"
define              =  tag "dfn"
del                 =  tag "del"
dlist               =  tag "dl"
dterm               =  tag "dt"
emphasize           =  tag "em"
fieldset            =  tag "fieldset"
form                =  tag "form"
h1                  =  tag "h1"
h2                  =  tag "h2"
h3                  =  tag "h3"
h4                  =  tag "h4"
h5                  =  tag "h5"
h6                  =  tag "h6"
header              =  tag "head"
hr                  = itag "hr"
image               = itag "img"
input               = itag "input"
ins                 =  tag "ins"
italics             =  tag "i"
keyboard            =  tag "kbd"
label               =  tag "label"
legend              =  tag "legend"
li                  =  tag "li"
meta                = itag "meta"
noscript            =  tag "noscript"
object              =  tag "object"
olist               =  tag "ol"
optgroup            =  tag "optgroup"
option              =  tag "option"
paragraph           =  tag "p"
param               = itag "param"
pre                 =  tag "pre"
quote               =  tag "q"
sample              =  tag "samp"
script              =  tag "script"
select              =  tag "select"
small               =  tag "small"
strong              =  tag "strong"
style               =  tag "style"
sub                 =  tag "sub"
sup                 =  tag "sup"
table               =  tag "table"
tbody               =  tag "tbody"
td                  =  tag "td"
textarea            =  tag "textarea"
tfoot               =  tag "tfoot"
th                  =  tag "th"
thead               =  tag "thead"
thebase             = itag "base"
thecode             =  tag "code"
thediv              =  tag "div"
thehtml             =  tag "html"
thelink             =  tag "link"
themap              =  tag "map"
thespan             =  tag "span"
thetitle            =  tag "title"
tr                  =  tag "tr"
tt                  =  tag "tt"
ulist               =  tag "ul"
variable            =  tag "var"
