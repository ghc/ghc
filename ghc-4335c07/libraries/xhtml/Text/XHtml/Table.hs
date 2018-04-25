-- | Table combinators for XHTML.
module Text.XHtml.Table (HtmlTable, HTMLTABLE(..),
                        (</>), above, (<->), beside, 
                         aboves, besides,
                         simpleTable) where

import Text.XHtml.Internals
import Text.XHtml.Strict.Elements
import Text.XHtml.Strict.Attributes

import qualified Text.XHtml.BlockTable as BT


infixr 3 </>  -- combining table cells 
infixr 4 <->  -- combining table cells

--
-- * Tables
--

class HTMLTABLE ht where
      cell :: ht -> HtmlTable

instance HTMLTABLE HtmlTable where
      cell = id

instance HTMLTABLE Html where
      cell h = 
         let
              cellFn x y = h ! (add x colspan $ add y rowspan $ [])
              add 1 _  rest = rest
              add n fn rest = fn n : rest
              r = BT.single cellFn
         in 
              mkHtmlTable r

-- | We internally represent the Cell inside a Table with an
-- object of the type
--
-- >	   Int -> Int -> Html
--
-- When we render it later, we find out how many columns
-- or rows this cell will span over, and can
-- include the correct colspan\/rowspan command.
newtype HtmlTable 
      = HtmlTable (BT.BlockTable (Int -> Int -> Html))


mkHtmlTable :: BT.BlockTable (Int -> Int -> Html) -> HtmlTable
mkHtmlTable r = HtmlTable r

-- We give both infix and nonfix, take your pick.
-- Notice that there is no concept of a row/column
-- of zero items.

(</>),above,(<->),beside :: (HTMLTABLE ht1,HTMLTABLE ht2)
                       => ht1 -> ht2 -> HtmlTable
above   a b = combine BT.above (cell a) (cell b)
(</>)         = above
beside  a b = combine BT.beside (cell a) (cell b)
(<->) = beside

combine :: (BT.BlockTable (Int -> Int -> Html) ->
            BT.BlockTable (Int -> Int -> Html) ->
            BT.BlockTable (Int -> Int -> Html))
        -> HtmlTable
        -> HtmlTable
        -> HtmlTable
combine fn (HtmlTable a) (HtmlTable b) = mkHtmlTable (a `fn` b)

-- Both aboves and besides presume a non-empty list.
-- here is no concept of a empty row or column in these
-- table combinators.

aboves :: (HTMLTABLE ht) => [ht] -> HtmlTable
aboves []  = error "aboves []"
aboves xs  = foldr1 (</>) (map cell xs)

besides :: (HTMLTABLE ht) => [ht] -> HtmlTable
besides [] = error "besides []"
besides xs = foldr1 (<->) (map cell xs)

-- | renderTable takes the HtmlTable, and renders it back into
--   and Html object.
renderTable :: BT.BlockTable (Int -> Int -> Html) -> Html
renderTable theTable
      = concatHtml
          [tr << [theCell x y | (theCell,(x,y)) <- theRow ]
                      | theRow <- BT.getMatrix theTable]

instance HTML HtmlTable where
      toHtml (HtmlTable tab) = renderTable tab

instance Show HtmlTable where
      showsPrec _ (HtmlTable tab) = shows (renderTable tab)


-- | If you can't be bothered with the above, then you
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
simpleTable :: [HtmlAttr] -> [HtmlAttr] -> [[Html]] -> Html
simpleTable attr cellAttr lst
      = table ! attr 
          <<  (aboves 
              . map (besides . map ((td ! cellAttr) . toHtml))
              ) lst
