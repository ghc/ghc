-- A very simple, but useful, lexical analyser.
module SimpleLex(simpleLex) where

oper = "!#$%&*+./<=>?@\\^|:~-"
-- self-delim ()[]{},;`'"_
isalunum c = isAlphanum c || c == '_'

simpleLex :: String -> [String]
simpleLex "" = []
simpleLex (' ' :cs) = simpleLex cs			-- ignore white space
simpleLex ('\t':cs) = simpleLex cs
simpleLex ('\n':cs) = simpleLex cs
simpleLex ('-':cs@(c:_)) | isDigit c = 			-- negative numbers
	let (t:ts) = simpleLex cs 
	in  ('-':t) : ts
simpleLex (c:cs) | isDigit c = 				-- numbers (with optional .)
	let (nn, cs') = span isDigit cs 
	in  case cs' of
	    '.':cs'' -> let (d,r) = span isDigit cs'' 
	                in  (c:nn++'.':d) : simpleLex r
	    _ -> (c:nn) : simpleLex cs'
simpleLex (c:cs) | isAlpha c = 				-- identifiers
	let (nn, cs') = span isalunum cs in (c:nn) : simpleLex cs'
simpleLex (c:cs) | c `elem` oper = 			-- operator
	let (nn, cs') = span (`elem` oper) cs in (c:nn) : simpleLex cs'
simpleLex (c:cs) = [c] : simpleLex cs			-- self delimiting chars
