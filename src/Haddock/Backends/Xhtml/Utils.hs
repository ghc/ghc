-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Backends.Html.Util
-- Copyright   :  (c) Simon Marlow   2003-2006,
--                    David Waern    2006-2009,
--                    Mark Lentczner 2010
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
module Haddock.Backends.Xhtml.Utils (
  renderToString,

  namedAnchor, linkedAnchor,
  spliceURL,
  groupId,

  (<+>), (<=>), char,
  keyword, punctuate,

  braces, brackets, pabrackets, parens, parenList, ubxParenList,
  arrow, comma, dcolon, dot, darrow, equals, forallSymbol, quote,

  hsep, vcat,

  collapseSection, collapseToggle, collapseControl,
) where


import Haddock.GhcUtils
import Haddock.Utils

import Data.Maybe

import Text.XHtml hiding ( name, title, p, quote )
import qualified Text.XHtml as XHtml

import GHC      ( SrcSpan(..), srcSpanStartLine, Name )
import Module   ( Module )
import Name     ( getOccString, nameOccName, isValOcc )


spliceURL :: Maybe FilePath -> Maybe Module -> Maybe GHC.Name ->
             Maybe SrcSpan -> String -> String
spliceURL maybe_file maybe_mod maybe_name maybe_loc = run
 where
  file = fromMaybe "" maybe_file
  mdl = case maybe_mod of
          Nothing           -> ""
          Just m -> moduleString m

  (name, kind) =
    case maybe_name of
      Nothing             -> ("","")
      Just n | isValOcc (nameOccName n) -> (escapeStr (getOccString n), "v")
             | otherwise -> (escapeStr (getOccString n), "t")

  line = case maybe_loc of
    Nothing -> ""
    Just span_ ->
      case span_ of
      RealSrcSpan span__ ->
        show $ srcSpanStartLine span__
      UnhelpfulSpan _ ->
        error "spliceURL UnhelpfulSpan"

  run "" = ""
  run ('%':'M':rest) = mdl  ++ run rest
  run ('%':'F':rest) = file ++ run rest
  run ('%':'N':rest) = name ++ run rest
  run ('%':'K':rest) = kind ++ run rest
  run ('%':'L':rest) = line ++ run rest
  run ('%':'%':rest) = '%'   : run rest

  run ('%':'{':'M':'O':'D':'U':'L':'E':'}':rest) = mdl  ++ run rest
  run ('%':'{':'F':'I':'L':'E':'}':rest)         = file ++ run rest
  run ('%':'{':'N':'A':'M':'E':'}':rest)         = name ++ run rest
  run ('%':'{':'K':'I':'N':'D':'}':rest)         = kind ++ run rest

  run ('%':'{':'M':'O':'D':'U':'L':'E':'/':'.':'/':c:'}':rest) =
    map (\x -> if x == '.' then c else x) mdl ++ run rest

  run ('%':'{':'F':'I':'L':'E':'/':'/':'/':c:'}':rest) =
    map (\x -> if x == '/' then c else x) file ++ run rest

  run ('%':'{':'L':'I':'N':'E':'}':rest)         = line ++ run rest

  run (c:rest) = c : run rest


renderToString :: Bool -> Html -> String
renderToString debug html
  | debug = renderHtml html
  | otherwise = showHtml html


hsep :: [Html] -> Html
hsep [] = noHtml
hsep htmls = foldr1 (\a b -> a+++" "+++b) htmls

-- | Concatenate a series of 'Html' values vertically, with linebreaks in between.
vcat :: [Html] -> Html
vcat [] = noHtml
vcat htmls = foldr1 (\a b -> a+++br+++b) htmls


infixr 8 <+>
(<+>) :: Html -> Html -> Html
a <+> b = a +++ sep +++ b
  where
    sep = if isNoHtml a || isNoHtml b then noHtml else toHtml " "

-- | Join two 'Html' values together with a linebreak in between.
--   Has 'noHtml' as left identity.
infixr 8 <=>
(<=>) :: Html -> Html -> Html
a <=> b = a +++ sep +++ b
  where
    sep = if isNoHtml a then noHtml else br


keyword :: String -> Html
keyword s = thespan ! [theclass "keyword"] << toHtml s


equals, comma :: Html
equals = char '='
comma  = char ','


char :: Char -> Html
char c = toHtml [c]


quote :: Html -> Html
quote h = char '`' +++ h +++ '`'


parens, brackets, pabrackets, braces :: Html -> Html
parens h        = char '(' +++ h +++ char ')'
brackets h      = char '[' +++ h +++ char ']'
pabrackets h    = toHtml "[:" +++ h +++ toHtml ":]"
braces h        = char '{' +++ h +++ char '}'


punctuate :: Html -> [Html] -> [Html]
punctuate _ []     = []
punctuate h (d0:ds) = go d0 ds
                   where
                     go d [] = [d]
                     go d (e:es) = (d +++ h) : go e es


parenList :: [Html] -> Html
parenList = parens . hsep . punctuate comma


ubxParenList :: [Html] -> Html
ubxParenList = ubxparens . hsep . punctuate comma


ubxparens :: Html -> Html
ubxparens h = toHtml "(#" +++ h +++ toHtml "#)"


dcolon, arrow, darrow, forallSymbol :: Bool -> Html
dcolon unicode = toHtml (if unicode then "∷" else "::")
arrow  unicode = toHtml (if unicode then "→" else "->")
darrow unicode = toHtml (if unicode then "⇒" else "=>")
forallSymbol unicode = if unicode then toHtml "∀" else keyword "forall"


dot :: Html
dot = toHtml "."


-- | Generate a named anchor
namedAnchor :: String -> Html -> Html
namedAnchor n = anchor ! [XHtml.name n]


linkedAnchor :: String -> Html -> Html
linkedAnchor n = anchor ! [href ('#':n)]


-- | generate an anchor identifier for a group
groupId :: String -> String
groupId g = makeAnchorId ("g:" ++ g)

--
-- A section of HTML which is collapsible.
--

-- | Attributes for an area that can be collapsed
collapseSection :: String -> Bool -> String -> [HtmlAttr]
collapseSection id_ state classes = [ identifier sid, theclass cs ]
  where cs = unwords (words classes ++ [pick state "show" "hide"])
        sid = "section." ++ id_

-- | Attributes for an area that toggles a collapsed area
collapseToggle :: String -> [HtmlAttr]
collapseToggle id_ = [ strAttr "onclick" js ]
  where js = "toggleSection('" ++ id_ ++ "')";
  
-- | Attributes for an area that toggles a collapsed area,
-- and displays a control.
collapseControl :: String -> Bool -> String -> [HtmlAttr]
collapseControl id_ state classes =
  [ identifier cid, theclass cs ] ++ collapseToggle id_
  where cs = unwords (words classes ++ [pick state "collapser" "expander"])
        cid = "control." ++ id_


pick :: Bool -> a -> a -> a
pick True  t _ = t
pick False _ f = f
