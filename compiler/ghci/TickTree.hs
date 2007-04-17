-----------------------------------------------------------------------------
--
-- Trees of source spans used by the breakpoint machinery
--
-- (c) The University of Glasgow 2007
--
-----------------------------------------------------------------------------

module TickTree 
   ( TickTree, lookupTickTreeCoord, lookupTickTreeLine, tickTreeFromList )
   where

import SrcLoc

import Data.List (partition, foldl') 

type TickNumber = Int

newtype TickTree = Root [SpanTree]

data SpanTree 
   = Node 
     { spanTreeTick     :: TickNumber 
     , spanTreeLoc      :: SrcSpan
     , spanTreeChildren :: [SpanTree]
     }

mkNode :: TickNumber -> SrcSpan -> [SpanTree] -> SpanTree
mkNode tick loc kids
   = Node { spanTreeTick = tick, spanTreeLoc = loc, spanTreeChildren = kids }

emptyTickTree :: TickTree
emptyTickTree = Root []

tickTreeFromList :: [(TickNumber, SrcSpan)] -> TickTree
tickTreeFromList 
   = foldl' (\tree (tick,loc) -> insertTickTree tick loc tree) emptyTickTree 

insertTickTree :: TickNumber -> SrcSpan -> TickTree -> TickTree
insertTickTree tick loc (Root children)
   = Root $ insertSpanTree tick loc children

insertSpanTree :: TickNumber -> SrcSpan -> [SpanTree] -> [SpanTree]
insertSpanTree tick loc [] = [mkNode tick loc []] 
insertSpanTree tick loc children@(kid:siblings) 
   | null containedKids = insertDeeper tick loc children
   | otherwise = mkNode tick loc children : rest
   where
   (containedKids, rest) = getContainedKids loc children
   insertDeeper :: TickNumber -> SrcSpan -> [SpanTree] -> [SpanTree]
   insertDeeper tick loc [] = [mkNode tick loc []] 
   insertDeeper tick loc nodes@(kid:siblings)
      | srcSpanStart loc < srcSpanStart kidLoc = newBranch : nodes 
      | kidLoc `contains` loc = newKid : siblings 
      | otherwise = kid : insertDeeper tick loc siblings
      where
      newBranch = mkNode tick loc []
      kidLoc = spanTreeLoc kid
      newKid = mkNode (spanTreeTick kid) (spanTreeLoc kid)
                      (insertSpanTree tick loc $ spanTreeChildren kid)

getContainedKids :: SrcSpan -> [SpanTree] -> ([SpanTree], [SpanTree])
getContainedKids loc = Data.List.partition (\tree -> loc `contains` (spanTreeLoc tree)) 

-- True if the left loc contains the right loc
contains :: SrcSpan -> SrcSpan -> Bool
contains span1 span2
   = srcSpanStart span1 <= srcSpanStart span2 &&
     srcSpanEnd   span1 <= srcSpanEnd   span2   

type TickLoc = (TickNumber, SrcSpan)
type LineNumber = Int
type ColumnNumber = Int
type Coord = (LineNumber, ColumnNumber)

srcSpanStartLine = srcLocLine . srcSpanStart

lookupTickTreeLine :: LineNumber -> TickTree -> Maybe TickLoc 
lookupTickTreeLine line (Root children) = lookupSpanTreeLine line children

lookupSpanTreeLine :: LineNumber -> [SpanTree] -> Maybe TickLoc 
lookupSpanTreeLine line [] = Nothing 
lookupSpanTreeLine line (node:nodes)
   | startLine == line && endLine == line 
        = Just (spanTreeTick node, spanTreeLoc node) 
   | startLine > line  
        = lookupSpanTreeLine line nodes
   | otherwise = 
        case lookupSpanTreeLine line (spanTreeChildren node) of
                Nothing    -> lookupSpanTreeLine line nodes
                x@(Just _) -> x
   where
   startLine = srcSpanStartLine (spanTreeLoc node) 
   endLine = srcSpanEndLine (spanTreeLoc node) 

lookupTickTreeCoord :: Coord -> TickTree -> Maybe TickLoc 
lookupTickTreeCoord coord (Root children) = lookupSpanTreeCoord coord children Nothing

lookupSpanTreeCoord :: Coord -> [SpanTree] -> Maybe TickLoc -> Maybe TickLoc 
lookupSpanTreeCoord coord [] acc = acc 
lookupSpanTreeCoord coord (kid:siblings) acc
   | spanTreeLoc kid `spans` coord 
        = lookupSpanTreeCoord coord (spanTreeChildren kid) 
                              (Just (spanTreeTick kid, spanTreeLoc kid))
   | otherwise 
        = lookupSpanTreeCoord coord siblings acc
   where
   spans :: SrcSpan -> Coord -> Bool
   spans span (l,c) = srcSpanStart span <= loc && loc <= srcSpanEnd span
        where loc = mkSrcLoc (srcSpanFile span) l c
