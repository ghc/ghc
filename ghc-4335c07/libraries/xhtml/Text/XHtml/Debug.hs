{-# OPTIONS_HADDOCK hide #-}

-- | This module contains functions for displaying
--   HTML as a pretty tree.
module Text.XHtml.Debug ( HtmlTree(..), treeHtml, treeColors, debugHtml ) where

import Text.XHtml.Internals
import Text.XHtml.Extras
import Text.XHtml.Table
import Text.XHtml.Strict.Elements
import Text.XHtml.Strict.Attributes

--
-- * Tree Displaying Combinators
--

-- | The basic idea is you render your structure in the form
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
      treeHtml' _ (HtmlLeaf leaf) = cell
                                         (td ! [width "100%"] 
                                            << bold  
                                               << leaf)
      treeHtml' (c:cs@(c2:_)) (HtmlNode hopen ts hclose) =
          if null ts && isNoHtml hclose
          then
              cell hd 
          else if null ts
          then
              hd </> bar `beside` (td ! [bgcolor' c2] << spaceHtml)
                 </> tl
          else
              hd </> (bar `beside` treeHtmls morecolors ts)
                 </> tl
        where
              -- This stops a column of colors being the same
              -- color as the immeduately outside nesting bar.
              morecolors = filter ((/= c).head) (manycolors cs)
              bar = td ! [bgcolor' c,width "10"] << spaceHtml
              hd = td ! [bgcolor' c] << hopen
              tl = td ! [bgcolor' c] << hclose
      treeHtml' _ _ = error "The imposible happens"

instance HTML HtmlTree where
      toHtml x = treeHtml treeColors x

-- type "length treeColors" to see how many colors are here.
treeColors :: [String]
treeColors = ["#88ccff","#ffffaa","#ffaaff","#ccffff"] ++ treeColors


-- 
-- * Html Debugging Combinators
--

-- | This uses the above tree rendering function, and displays the
-- Html as a tree structure, allowing debugging of what is
-- actually getting produced.
debugHtml :: (HTML a) => a -> Html
debugHtml obj = table ! [border 0] << 
                  ( th ! [bgcolor' "#008888"]
                     << underline'
                       << "Debugging Output"
               </>  td << (toHtml (debug' (toHtml obj)))
              )
  where

      debug' :: Html -> [HtmlTree]
      debug' (Html markups) = map debug markups

      debug :: HtmlElement -> HtmlTree
      debug (HtmlString str) = HtmlLeaf (spaceHtml +++
                                              linesToHtml (lines str))
      debug (HtmlTag {
              markupTag = tag',
              markupContent = content',
              markupAttrs  = attrs
              }) =
              case content' of
                Html [] -> HtmlNode hd [] noHtml
                Html xs -> HtmlNode hd (map debug xs) tl
        where
              args = if null attrs
                     then ""
                     else "  " ++ unwords (map show attrs)
              hd = xsmallFont << ("<" ++ tag' ++ args ++ ">")
              tl = xsmallFont << ("</" ++ tag' ++ ">")

bgcolor' :: String -> HtmlAttr
bgcolor' c = thestyle ("background-color:" ++ c)

underline' :: Html -> Html
underline' = thespan ! [thestyle ("text-decoration:underline")]

xsmallFont :: Html -> Html
xsmallFont  = thespan ! [thestyle ("font-size:x-small")]
