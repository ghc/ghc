{-# OPTIONS_HADDOCK hide #-}

module Text.XHtml.Transitional.Elements where

import Text.XHtml.Internals

-- * Extra elements in XHTML Transitional

{-# DEPRECATED applet "This element is deprecated in XHTML 1.0" #-}
applet              :: Html -> Html
applet              =  tag "applet"

{-# DEPRECATED basefont "This element is deprecated in XHTML 1.0" #-}
basefont            ::         Html
basefont            = itag "basefont"

{-# DEPRECATED center "This element is deprecated in XHTML 1.0" #-}
center              :: Html -> Html
center              =  tag "center"

{-# DEPRECATED dir "This element is deprecated in XHTML 1.0" #-}
dir                 :: Html -> Html
dir                 =  tag "dir"

{-# DEPRECATED font "This element is deprecated in XHTML 1.0" #-}
font                :: Html -> Html
font                =  tag "font"


iframe              :: Html -> Html
iframe              =  tag "iframe"

{-# DEPRECATED isindex "This element is deprecated in XHTML 1.0" #-}
isindex             :: Html
isindex             = itag "isindex"

{-# DEPRECATED themenu "This element is deprecated in XHTML 1.0" #-}
themenu             :: Html -> Html
themenu             =  tag "menu"

{-# DEPRECATED strike "This element is deprecated in XHTML 1.0" #-}
strike              :: Html -> Html
strike              =  tag "strike"

{-# DEPRECATED underline "This element is deprecated in XHTML 1.0" #-}
underline           :: Html -> Html
underline           =  tag "u"
