module CharSeq (
      CSeq,
      cNil, cAppend, cIndent, cNL, cStr, cCh, -- cAbove, cBeside, cConcat,
      cShow
)  where

cShow   :: CSeq -> [Char]
cNil    :: CSeq
cAppend :: CSeq -> CSeq -> CSeq
-- UNUSED: cConcat :: [CSeq] -> CSeq
cIndent :: Int -> CSeq -> CSeq
cNL     :: CSeq
cStr    :: [Char] -> CSeq
cCh     :: Char -> CSeq
{- old:
cAbove  :: CSeq -> CSeq -> CSeq       -- Separate them with a newline unless
				      -- one or t'other is empty.
cBeside :: CSeq -> CSeq -> CSeq       -- Similar; separates with a space.
-}
data CSeq = CNil
	  | CAppend CSeq CSeq
	  | CIndent Int CSeq
	  | CNewline
	  | CStr [Char]
	  | CCh Char
	  deriving ()
cNil      = CNil
-- cAppend CNil cs2  = cs2
-- cAppend cs1  CNil = cs1
cAppend cs1  cs2  = CAppend cs1 cs2
-- cIndent n CNil = CNil
cIndent n cs   = CIndent n cs

cNL       = CNewline
cStr      = CStr
cCh       = CCh
-- UNUSED: cConcat   = foldr cAppend CNil
{- old:
cAbove CNil cs2  = cs2
cAbove cs1  cs2  = CAppend cs1 (case cs2 of CNil -> CNil; other -> CAppend CNewline cs2)
-}
{- old:
cBeside CNil cs2
  = case cs2 of
      CIndent n cs3 -> CIndent (n-1) cs3
      other ->         cs2    -- oh well...

cBeside cs1  cs2 = CAppend cs1 (case cs2 of CNil -> CNil; other -> CAppend (CCh ' ') cs2)
-}
cShow seq = flatten 0 True seq []
flatten :: Int                -- Indentation
	-> Bool               -- True => just had a newline
	-> CSeq               -- Current seq to flatten
	-> [(Int,CSeq)]       -- Work list with indentation
	-> String
flatten n nlp CNil seqs = flattenS nlp seqs
flatten n nlp (CAppend seq1 seq2) seqs = flatten n nlp seq1 ((n,seq2) : seqs)
flatten n nlp (CIndent n' seq) seqs = flatten (n'+n) nlp seq seqs
flatten n nlp CNewline seqs = '\n' : flattenS True seqs
flatten n False (CStr s) seqs = s ++ flattenS False seqs
flatten n False (CCh c)  seqs = c :  flattenS False seqs
flatten n True  (CStr s) seqs = mkIndent n (s ++ flattenS False seqs)
flatten n True  (CCh c)  seqs = mkIndent n (c :  flattenS False seqs)
flattenS :: Bool -> [(Int, CSeq)] -> String
flattenS nlp [] = ""
flattenS nlp ((col,seq):seqs) = flatten col nlp seq seqs
mkIndent :: Int -> String -> String
mkIndent 0 s = s
mkIndent n s
 = if (n >= 8) then '\t' : mkIndent (n-8) s
	       else ' '  : mkIndent (n-1) s
-- A little Unix-y; ToDo: something?
