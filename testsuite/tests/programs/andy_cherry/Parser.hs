
 module Parser (pgnLexer,pgnParser) where

 import GenUtils
 import DataTypes
 import Data.Char -- 1.3



 pgnLexer :: String -> [Token]
 pgnLexer ('.':r) = PeriodToken  : pgnLexer r
 pgnLexer ('*':r) = AsterixToken : pgnLexer r
 pgnLexer ('[':r) = LeftSBToken  : pgnLexer r
 pgnLexer (']':r) = RightSBToken : pgnLexer r
 pgnLexer ('(':r) = LeftRBToken  : pgnLexer r
 pgnLexer (')':r) = RightRBToken : pgnLexer r
 pgnLexer ('<':r) = LeftABToken  : pgnLexer r
 pgnLexer ('>':r) = RightABToken : pgnLexer r
 pgnLexer ('"':r) = readString r ""
 pgnLexer ('{':r) = readComment r ""
 pgnLexer ('$':r) = readNAG r ""
 pgnLexer ('!':'?':r) = mkNAGToken 5 : pgnLexer r
 pgnLexer ('!':'!':r) = mkNAGToken 3 : pgnLexer r
 pgnLexer ('!':r)     = mkNAGToken 1 : pgnLexer r
 pgnLexer ('?':'?':r) = mkNAGToken 4 : pgnLexer r
 pgnLexer ('?':'!':r) = mkNAGToken 6 : pgnLexer r
 pgnLexer ('?':r)     = mkNAGToken 2 : pgnLexer r
 pgnLexer ('%':r) = pgnLexer (dropWhile (/= '\n') r)
 pgnLexer (c:r)
       | isSpace c = pgnLexer r
       | isAlpha c || isDigit c = pgnSymbolLexer r [c]
       | otherwise = error ("Error lexing: " ++ takeWhile (/= '\n') (c:r))
 pgnLexer [] = []

 pgnSymbolLexer (c:r) sym 
       | isAlpha c 
       || isDigit c 
       || elem c "_+#=:-/" = pgnSymbolLexer r (c:sym)
 pgnSymbolLexer r sym 
       | all isDigit sym = IntToken (read (reverse sym)) : pgnLexer r
 pgnSymbolLexer r sym   = SymbolToken (reverse sym) : pgnLexer r

 readString ('\\':'\\':r) str = readString r ('\\':str)
 readString ('\\':'"':r) str = readString r ('"':str)
 readString ('"':r) str     = StringToken (reverse str) : pgnLexer r
 readString (c:r) str       = readString r (c:str)

 readComment ('}':r) str = CommentToken (revwords str []) : pgnLexer r
 readComment (c:r) str = readComment r (c:str)

 revwords (c:r) wds
    | isSpace c = revwords r wds
    | otherwise = revwords' r [c] wds
 revwords [] wds = wds
 revwords' (c:r) wd wds 
    | isSpace c = revwords r (wd:wds)
    | otherwise = revwords' r (c:wd) wds
 revwords' [] wd wds = wd : wds

 readNAG (c:r) str
       | isDigit c = readNAG r (c:str)
 readNAG r str = mkNAGToken (read (reverse str)) : pgnLexer r

 mkNAGToken 1 = NAGAnnToken 1 "!" 
 mkNAGToken 2 = NAGAnnToken 2 "?" 
 mkNAGToken 3 = NAGAnnToken 3 "!!"
 mkNAGToken 4 = NAGAnnToken 4 "??"
 mkNAGToken 5 = NAGAnnToken 5 "!?"
 mkNAGToken 6 = NAGAnnToken 6 "?!"
 mkNAGToken n = NAGToken n




 pgnParser :: (Int -> Bool) -> String -> [AbsGame]
 pgnParser fn str = 
       [ game | (no,game) <- zip [1..] (parseTags (pgnLexer str) id),
                fn no]

 type FL a = [a] -> [a]

 parseTags :: [Token] -> FL TagStr -> [AbsGame]
 parseTags (LeftSBToken:SymbolToken sym:StringToken str:RightSBToken:rest) 
           other_tags = parseTags rest (other_tags . ((:) (TagStr sym str)))
 parseTags toks@(LeftSBToken:_) _
       = error ("BAD Token:" ++ unwords (map userFormat (take 10 toks)))
 parseTags toks tags = parseToks toks id tags

 parseToks :: [Token] 
       -> FL Token 
       -> FL TagStr
       -> [AbsGame]
 parseToks next@(LeftSBToken:_)     = \ toks tags ->
       Game (tags []) (toks []) : parseTags next id
 parseToks (tk:r)                    = pushToken tk r 
 parseToks [] = \ toks tags -> [Game (tags []) (toks [])]

 pushToken tok r toks = parseToks r (toks . ((:) tok))
