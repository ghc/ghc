{-# OPTIONS_HADDOCK hide #-}

module Text.XHtml.Frameset.Elements where

import Text.XHtml.Internals

-- * Extra elements in XHTML Frameset

frame               :: Html -> Html
frame               =  tag "frame"

frameset            :: Html -> Html
frameset            =  tag "frameset"

noframes            :: Html -> Html
noframes            =  tag "noframes"
