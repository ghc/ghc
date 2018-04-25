module Pretty (
      Pretty,
      PprStyle(..),
      ppNil, ppStr, ppChar, ppInt, ppInteger, ppDouble,
      ppSP, pp'SP, ppLbrack, ppRbrack, ppLparen, ppRparen,
      ppSemi, ppComma,

      ppCat, ppBeside, ppBesides, ppAbove, ppAboves,
      ppNest, ppSep, ppHang, ppInterleave,
      ppShow, ppUnformatted,
      -- abstract type, to complete the interface...
      PrettyRep
) where

import CharSeq

ppNil         :: Pretty
ppSP, pp'SP, ppLbrack, ppRbrack, ppLparen, ppRparen, ppSemi, ppComma :: Pretty
ppStr         :: [Char] -> Pretty
ppChar        :: Char -> Pretty
ppInt         :: Int -> Pretty
ppInteger     :: Integer -> Pretty
ppDouble      :: Double -> Pretty
ppBeside      :: Pretty -> Pretty -> Pretty
ppBesides     :: [Pretty] -> Pretty
ppBesideSP    :: Pretty -> Pretty -> Pretty
ppCat         :: [Pretty] -> Pretty           -- i.e., ppBesidesSP
ppAbove       :: Pretty -> Pretty -> Pretty
ppAboves      :: [Pretty] -> Pretty
ppInterleave  :: Pretty -> [Pretty] -> Pretty
ppSep         :: [Pretty] -> Pretty
ppHang        :: Pretty -> Int -> Pretty -> Pretty
ppNest        :: Int -> Pretty -> Pretty
ppShow        :: Int -> Pretty -> [Char]
ppUnformatted :: Pretty -> [Char]
type Pretty = Int             -- The width to print in
	    -> Bool           -- True => vertical context
	    -> PrettyRep
data PrettyRep
  = MkPrettyRep       CSeq    -- The text
		      Int     -- No of chars in last line
		      Bool    -- True if empty object
		      Bool    -- Fits on a single line in specified width
  deriving ()
ppShow width p
 = cShow seq
 where (MkPrettyRep seq ll emp sl) = p width False
{- !!! this seems to tickle an nhc bug (works w/ hbc)
 = case (p width False) of
      MkPrettyRep seq sl ll -> cShow seq
-}

ppUnformatted p
 = cShow seq
 where (MkPrettyRep seq ll emp sl) = p 80 False
      -- ToDo: ppUnformatted doesn't do anything yet
ppNil    width is_vert = MkPrettyRep cNil 0 True (width >= 0)
			 -- Doesn't fit if width < 0, otherwise, ppNil
			 -- will make ppBesides always return True.

ppStr  s width is_vert = MkPrettyRep (cStr s) ls False (width >= ls)
			 where ls = length s
ppChar c width is_vert = MkPrettyRep (cCh c) 1 False (width >= 1)
ppInt  n               = ppStr (show n)
ppInteger n            = ppStr (show n)
ppDouble  n            = ppStr (show n)
ppSP      = ppChar ' '
pp'SP     = ppStr ", "
ppLbrack  = ppChar '['
ppRbrack  = ppChar ']'
ppLparen  = ppChar '('
ppRparen  = ppChar ')'
ppSemi    = ppChar ';'
ppComma   = ppChar ','
ppInterleave sep ps   = ppSep (pi ps)
 where
  pi []       = []
  pi [x]      = [x]
  pi (x:xs)   = (ppBeside x sep) : pi xs
ppBeside p1 p2 width is_vert
 = MkPrettyRep (seq1 `cAppend` (cIndent ll1 seq2))
	       (ll1 + ll2)
	       (emp1 `andL` emp2)
	       ((width >= 0) `andL` (sl1 `andL` sl2))
		      -- This sequence of andL's ensures that ppBeside
		      -- returns a False for sl as soon as possible.
 where
  MkPrettyRep seq1 ll1 emp1 sl1 = p1 width       False
  MkPrettyRep seq2 ll2 emp2 sl2 = p2 (width-ll1) False
      -- ToDo: if emp{1,2} then we really
      -- should be passing on "is_vert" to p{2,1}.
ppBesides [] = ppNil
ppBesides ps = foldr1 ppBeside ps
ppBesideSP p1 p2 width is_vert
 = MkPrettyRep (seq1 `cAppend` (sp `cAppend` (cIndent li seq2)))
	       (li + ll2)
	       (emp1 `andL` emp2)
	       ((width >= wi) `andL` (sl1 `andL` sl2))
 where
  MkPrettyRep seq1 ll1 emp1 sl1 = p1 width      False
  MkPrettyRep seq2 ll2 emp2 sl2 = p2 (width-li) False
  li, wi :: Int
  li = if emp1 then 0 else ll1+1
  wi = if emp1 then 0 else 1
  sp = if emp1 `orL` emp2 then cNil else (cCh ' ')
ppCat []  = ppNil
ppCat ps  = foldr1 ppBesideSP ps
ppAbove p1 p2 width is_vert
 = MkPrettyRep (seq1 `cAppend` (nl `cAppend` seq2))
	       ll2
	      -- ToDo: make ll depend on empties?
	       (emp1 `andL` emp2)
	       False
 where
  nl = if emp1 `orL` emp2 then cNil else cNL
  MkPrettyRep seq1 ll1 emp1 sl1 = p1 width True
  MkPrettyRep seq2 ll2 emp2 sl2 = p2 width True
      -- ToDo: ditto about passing is_vert if empties
ppAboves [] = ppNil
ppAboves ps = foldr1 ppAbove ps
ppNest n p width False = p width False
ppNest n p width True
 = MkPrettyRep (cIndent n seq) (ll+n) emp sl
 where
  MkPrettyRep seq ll emp sl = p (width-n) True
ppHang p1 n p2 width is_vert  -- This is a little bit stricter than it could
			      -- be made with a little more effort.
			      -- Eg the output always starts with seq1
 = if emp1 then
      p2 width is_vert
   else 
   if (ll1 <= n) `orL` sl2 then       -- very ppBesideSP'ish
      -- Hang it if p1 shorter than indent or if it doesn't fit
      MkPrettyRep (seq1 `cAppend` (cCh ' ') `cAppend` (cIndent (ll1+1) seq2))
		(ll1 + 1 + ll2)
		False
		(sl1 `andL` sl2)
  else
      -- Nest it (pretty ppAbove-ish)
      MkPrettyRep (seq1 `cAppend` (cNL `cAppend` (cIndent n seq2')))
		ll2' -- ToDo: depend on empties
		False
		False
 where
  MkPrettyRep seq1 ll1 emp1 sl1 = p1 width      False
  MkPrettyRep seq2 ll2 emp2 sl2 = p2 (width-(ll1+1)) False
      -- ToDo: more "is_vert if empty" stuff

  MkPrettyRep seq2' ll2' emp2' sl2' = p2 (width-n) False      -- ToDo: True?
ppSep []  width is_vert = ppNil width is_vert
ppSep [p] width is_vert = p     width is_vert
ppSep ps  width is_vert
 = if sl then
      pr                              -- Fits on one line
   else
      ppAboves ps width is_vert       -- Takes several lines
 where
  pr@(MkPrettyRep seq ll emp sl) = ppCat ps width is_vert

{- !!! suspected on same nhc-bug grounds
 = case (ppBesides ps width is_vert) of
      pr1@(MkPrettyRep seq1 sl1 ll1) ->
	  if (sl1 && ll1 <= width) then
	      pr1                             -- Fits on one line
	  else
	      ppAboves ps width is_vert       -- Takes several lines
-}
andL :: Bool -> Bool -> Bool
andL False x = False
andL True  x = x
orL :: Bool -> Bool -> Bool
orL True  x = True
orL False x = x
data PprStyle
  = PprForUser -- Pretty-print in a way that will make sense
	       -- to the ordinary user; must be very close to Haskell
	       -- syntax, etc.
	       -- ToDo: how diff is this from what pprInterface must do?
  | PprDebug   -- Standard debugging output
  | PprShowAll -- Debugging output which leaves nothing to the imagination
  | PprInterface -- Interface generation
