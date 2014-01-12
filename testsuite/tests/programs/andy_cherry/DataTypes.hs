
 module DataTypes where

 import GenUtils
 import Data.Array -- 1.3
 import Data.Ix
 import Data.Char
 infix 1 =: -- 1.3
 (=:) a b = (a,b)





 class Presentable a where
       userFormat :: a -> String       -- in prefered display format





 instance (Presentable a) => Presentable [a] where
     userFormat xs = unlines (map userFormat xs)




 data Piece
       = King
       | Queen
       | Rook
       | Knight
       | Bishop
       | Pawn deriving(Eq)

 instance Presentable Piece where
   userFormat King   = "K"
   userFormat Queen  = "Q"
   userFormat Rook   = "R"
   userFormat Knight = "N"
   userFormat Bishop = "B"
   userFormat Pawn   = "P"




 castleK = "O-O"
 castleQ = "O-O-O"





 data Colour = Black | White deriving (Eq)

 instance Presentable Colour where
       userFormat White = "White"
       userFormat Black = "Black"

 changeColour :: Colour -> Colour
 changeColour White = Black
 changeColour Black = White





 type ChessRank = Int  -- 1-8
 type ChessFile = Int  -- 1-8

 type BoardPos = (ChessFile,ChessRank) -- ChessFile (0-7) and ChessRank (0-7)
 type ExBoardPos = (Maybe ChessFile,Maybe ChessRank)

 extendBP :: BoardPos -> ExBoardPos 
 extendBP (a,b) = (Just a,Just b)

 compExBPandBP :: ExBoardPos -> BoardPos -> Bool
 compExBPandBP (a,b) (c,d) = a `cmp` c && b `cmp` d
    where 
       cmp Nothing  _ = True
       cmp (Just x) y = x == y

 userFormatBoardPos :: BoardPos -> String
 userFormatBoardPos (f,r) = userFormatFile f ++ userFormatRank r
 userFormatExBoardPos :: ExBoardPos -> String
 userFormatExBoardPos (Just f,Just r)  = userFormatFile f ++ userFormatRank r
 userFormatExBoardPos (Just f,Nothing) = userFormatFile f 
 userFormatExBoardPos (Nothing,Just r) = userFormatRank r
 userFormatExBoardPos _ = ""
 userFormatRank r = [toEnum (r + 48)]
 userFormatFile f = [toEnum (f + 96)]





 data MoveTok 
       = PieceTok Piece        -- Q,K,R,B,N
       | RankTok ChessRank     -- 1 .. 8
       | FileTok ChessFile     -- a .. h
       | PartCastleTok         -- 0 | O | o
       | CaptureTok            -- x
       | MoveToTok             -- -
       | QueensWith            -- =
       | CheckTok              -- +
       | MateTok               -- #

 charToMoveTok 'Q' = Just (PieceTok Queen)
 charToMoveTok 'K' = Just (PieceTok King)
 charToMoveTok 'R' = Just (PieceTok Rook)
 charToMoveTok 'B' = Just (PieceTok Bishop)
 charToMoveTok 'N' = Just (PieceTok Knight)
 charToMoveTok '1' = Just (RankTok 1)
 charToMoveTok '2' = Just (RankTok 2)
 charToMoveTok '3' = Just (RankTok 3)
 charToMoveTok '4' = Just (RankTok 4)
 charToMoveTok '5' = Just (RankTok 5)
 charToMoveTok '6' = Just (RankTok 6)
 charToMoveTok '7' = Just (RankTok 7)
 charToMoveTok '8' = Just (RankTok 8)
 charToMoveTok 'a' = Just (FileTok 1)
 charToMoveTok 'b' = Just (FileTok 2)
 charToMoveTok 'c' = Just (FileTok 3)
 charToMoveTok 'd' = Just (FileTok 4)
 charToMoveTok 'e' = Just (FileTok 5)
 charToMoveTok 'f' = Just (FileTok 6)
 charToMoveTok 'g' = Just (FileTok 7)
 charToMoveTok 'h' = Just (FileTok 8)
 charToMoveTok '0' = Just (PartCastleTok)
 charToMoveTok 'O' = Just (PartCastleTok)
 charToMoveTok 'o' = Just (PartCastleTok)
 charToMoveTok 'x' = Just (CaptureTok)
 charToMoveTok '-' = Just (MoveToTok)
 charToMoveTok '=' = Just (QueensWith)
 charToMoveTok '+' = Just (CheckTok)
 charToMoveTok '#' = Just (MateTok)
 charToMoveTok _   = Nothing



 data Quantum 
       = QuantumMove   String          -- Short Description of move
                       String          -- Check or Mate (+ or #)
                       String          -- !,??,?!, etc
                       Board           -- Snap Shot of Board
       | QuantumNAG Int                -- !,??,?! stuff
       | QuantumComment [String]       -- { comment }
       | QuantumResult String          -- 1-0, etc (marks end of game)
       | QuantumAnalysis [Quantum]     -- ( analysis )
       | QuantumPrintBoard             -- {^D}

 instance Presentable Quantum where
       userFormat (QuantumMove mv ch ann _) 
               = mv ++ ch ++ ann
       userFormat (QuantumNAG nag) = "$" ++ show nag
       userFormat (QuantumComment comment) 
               = "[" ++ unwords comment ++ "]"
       --userFormat (QuantumNumber num)  = userFormat num
       userFormat (QuantumResult str) = str
       userFormat (QuantumAnalysis anal) =
               "( " ++ unwords (map userFormat anal) ++ " )"



 data Result = Win | Draw | Loss | Unknown

 instance Presentable Result where
       userFormat Win     = "1-0"
       userFormat Draw    = "1/2-1/2"
       userFormat Loss    = "0-1"
       userFormat Unknown = "*"

 mkResult :: String -> Result
 mkResult "1-0"     = Win
 mkResult "1/2-1/2" = Draw
 mkResult "0-1"     = Loss
 mkResult _         = Unknown



 data TagStr = TagStr String String

 instance Presentable TagStr where
       userFormat (TagStr tag str) = "[" ++ tag ++ " \"" ++ str ++ "\"]"

 getTagStr :: String -> String -> [TagStr] -> String
 getTagStr str def [] = def
 getTagStr str def (TagStr st ans:rest) 
               | str == st = ans
               | otherwise = getTagStr str def rest

 getHeaderInfo 
       :: [TagStr]
       -> (
               String,         -- Date
               String,         -- Site
               Maybe Int,      -- Game Number
               Result,         -- W/D/L
               String,         -- White
               String,         -- Black
               String          -- Opening
       )
 getHeaderInfo tags = (
               date,
               site,
               gameno,
               result,
               white `par` whiteElo,
               black `par` blackElo,
               opening)
   where
       date   = case getTagStr "Date" "?" tags of
                  [a,b,c,d,'.','?','?','.','?','?'] -> [a,b,c,d]
                  [a,b,c,d,'.',x,y,'.','?','?'] -> getMonth [x,y] ++ " " ++ [a,b,c,d]
                  def -> "?"
       site     = getTagStr "Site" "?" tags
       gameno   = case getTagStr "GameNumber" "" tags of
                       xs | all isDigit xs && not (null xs) -> Just (read xs)
                       _ -> Nothing
       result   = mkResult (getTagStr "Result" "*" tags)
       white    = cannon (getTagStr "White" "?" tags)
       whiteElo = getTagStr "WhiteElo" "" tags
       black    = cannon (getTagStr "Black" "?" tags)
       blackElo = getTagStr "BlackElo" "" tags
       opening  = getOpening (getTagStr "ECO" "" tags)

       par xs "" = xs
       par xs ys = xs ++ " (" ++ ys ++ ")"

       getMonth "01" = "Jan"
       getMonth "02" = "Feb"
       getMonth "03" = "Mar"
       getMonth "04" = "Apr"
       getMonth "05" = "May"
       getMonth "06" = "Jun"
       getMonth "07" = "Jul"
       getMonth "08" = "Aug"
       getMonth "09" = "Sep"
       getMonth "10" = "Oct"
       getMonth "11" = "Nov"
       getMonth "12" = "Dec"

       cannon name = case span (/= ',') name of
                       (a,[',',' ',b]) -> b : ". " ++ a
                       (a,[',',b]) -> b : ". " ++ a
                       (a,',':' ':b) -> b ++ " " ++ a
                       (a,',':b) -> b ++ " " ++ a
                       _ -> name


 getOpening eco@[a,b,c] |  a >= 'A' && a <= 'E' && isDigit b && isDigit c 
    = getOpenName ((fromEnum a - fromEnum 'A') * 100 
               + (fromEnum b - fromEnum '0') * 10 
               + (fromEnum c - fromEnum '0')) ++ " " ++ eco
 getOpening other = other

 getOpenName :: Int -> String
 getOpenName eco 
       | otherwise = "Foo"
 {-
       | eco == 000 = "Irregular Openings"
       | eco == 001 = "Larsen Opening"
       | eco == 002 = "From's Gambit and Bird's Open"
       | eco == 003 = "Bird's Opening"
       | eco == 004 = "Dutch System"
       | eco == 005 = "Transposition to various Open"
       | eco == 006 = "Zukertort Opening"
       | eco >= 007 && eco <= 008
                    = "Barcza System"
       | eco == 009 = "Reti Opening"
       | eco == 010 = "Variations of Dutch, QI, KI"
       | eco >= 011 && eco <= 014
                    = "Reti Opening"
       | eco == 015 = "English counter King's Fianch"
       | eco >= 016 && eco <= 039
                    = "English Opening"
       | eco == 040 = "Unusual replies to 1.d4"
       | eco == 041 = "Modern Defence counter 1.d4"
       | eco == 042 = "Modern Defence with c2-c4"
       | eco >= 043 && eco <= 044
                    = "Old Benoni"
       | eco == 045 = "Queen's Pawn-Trompowski Var"
       | eco == 046 = "Queen's Pawn Opening"
       | eco == 047 = "Queen's Indian"
       | eco >= 048 && eco <= 049
                    = "King's Indian"
       | eco == 050 = "Queen's Indian"
       | eco >= 051 && eco <= 052
                    = "Budapest Defence"
       | eco >= 053 && eco <= 056
                    = "Old Indian Defence"
       | eco >= 057 && eco <= 059
                    = "Volga-Benko Gambit"
       | eco >= 060 && eco <= 079
                    = "Benoni"
       | eco >= 080 && eco <= 099
                    = "Dutch Defence"
       | eco == 100 = "Owen Def, Nimzowitsch Def"
       | eco == 101 = "Center Counter"
       | eco >= 102 && eco <= 105
                    = "Alekhine's Defence"
       | eco == 106 = "Modern Defence"
       | eco >= 107 && eco <= 109
                    = "Pirc Defence"
       | eco >= 110 && eco <= 119
                    = "Caro-Kann Defence"
       | eco >= 120 && eco <= 199
                    = "Sicilian Defence"
       | eco >= 200 && eco <= 219
                    = "French Defence"
       | eco == 220 = "Rare moves"
       | eco == 221 = "Nordic Gambit"
       | eco == 222 = "Central Gambit"
       | eco >= 223 && eco <= 224
                    = "Bishop's Opening"
       | eco >= 225 && eco <= 229
                    = "Vienna Game"
       | eco == 230 = "King's Gambit Declined"
       | eco >= 231 && eco <= 232
                    = "Falkbeer Counter Gambit"
       | eco >= 233 && eco <= 239
                    = "King's Gambit"
       | eco == 240 = "Latvian Gambit"
       | eco == 241 = "Philidor Defence"
       | eco >= 242 && eco <= 243
                    = "Russian Defence-Petrov"
       | eco >= 244 && eco <= 245
                    = "Scotch Opening"
       | eco >= 246 && eco <= 249
                    = "Four Knight's"
       | eco == 250 = "Italian Opening"
       | eco >= 251 && eco <= 252
                    = "Evans Gambit"
       | eco >= 253 && eco <= 254
                    = "Italian Opening"
       | eco >= 255 && eco <= 259
                    = "Two Knight's Play"
       | eco >= 260 && eco <= 299
                    = "Ruy Lopez"
       | eco >= 300 && eco <= 305
                    = "Queen Pawn's Opening"
       | eco >= 306 && eco <= 307
                    = "Queen's Gambit"
       | eco >= 308 && eco <= 309
                    = "Albins Counter Gambit"
       | eco >= 310 && eco <= 319
                    = "Slav Defence"
       | eco >= 320 && eco <= 329
                    = "Queen's Gambit Accepted"
       | eco >= 330 && eco <= 369
                    = "Queen's Gambit"
       | eco >= 370 && eco <= 399
                    = "Gruenfeld Defence"
       | eco >= 400 && eco <= 409
                    = "Catalan"
       | eco == 410 = "Blumenfeld Gambit"
       | eco >= 411 && eco <= 419
                    = "Queen's Indian"
       | eco >= 420 && eco <= 459
                    = "Nimzo Indian"
       | eco >= 460 && eco <= 499
                    = "King's Indian"
 -}



 data MoveNumber = MoveNumber Int Colour
 instance Presentable MoveNumber where
       userFormat (MoveNumber n White)  = show n ++ "."
       userFormat (MoveNumber n Black)  = show n ++ "..."

 initMoveNumber = MoveNumber 1 White
 incMove (MoveNumber i White) = MoveNumber i Black
 incMove (MoveNumber i Black) = MoveNumber (i+1) White
 decMove (MoveNumber i White) = MoveNumber (i-1) Black
 decMove (MoveNumber i Black) = MoveNumber i White
 getMoveColour :: MoveNumber -> Colour
 getMoveColour (MoveNumber _ c) = c



 data Token 



       = StringToken   String
       | AsterixToken
       | LeftABToken           -- ??
       | RightABToken          -- ??
       | NAGToken      Int     -- `normal' NAGS
       | NAGAnnToken   Int String
                               -- `special' move annotating NAGS (1-6)
       | SymbolToken   String
       | CommentToken  [String] -- list of words
       | LeftSBToken
       | RightSBToken
       | LeftRBToken
       | RightRBToken
       | IntToken      Int
       | PeriodToken



       | AnalToken     [Token]

 instance Presentable Token where
       userFormat (StringToken str) = show str
       userFormat (IntToken n)      = show n
       userFormat (PeriodToken)     = "."
       userFormat (AsterixToken)    = "*"
       userFormat (LeftSBToken)     = "["
       userFormat (RightSBToken)    = "]"
       userFormat (LeftRBToken)     = "("
       userFormat (RightRBToken)    = ")"
       userFormat (LeftABToken)     = "<"
       userFormat (RightABToken)    = ">"
       userFormat (NAGToken i)      = "$" ++ show i
       userFormat (NAGAnnToken i s) = "$" ++ show i
       userFormat (SymbolToken str) = str
       userFormat (CommentToken str) = "{" ++ unwords str ++ "}"
       userFormat (AnalToken toks) = "( " ++ unwords (map userFormat toks)
                                       ++ " )"





 data Game a = Game [TagStr] [a]

 type AbsGame = Game Token
 type RealGame = Game Quantum

 instance (Presentable a) => Presentable (Game a) where
       userFormat (Game tags toks) = 
               unlines (map userFormat tags 
                  ++ formatText 78 (map userFormat toks))






 data PlayMove
       = PlayMove
               Piece           -- with this
               BoardPos        -- from here
               BoardPos        -- to here (possibly capturing)
               SpecialMove

 mkPlayMove p f t = PlayMove p f t NothingSpecial

 data SpecialMove 
       = NothingSpecial        
       | BigPawnMove           -- allows e.p. next move
       | Queening Piece        -- queen with this
       | EnPassant             -- capture e.p.
    deriving (Eq)

 instance Presentable PlayMove where
       userFormat (PlayMove piece pos pos' sp) = 
               userFormat piece ++
               userFormatBoardPos pos ++ "-" ++
               userFormatBoardPos pos' ++ 
               userFormat sp

 instance Presentable SpecialMove where
       userFormat (NothingSpecial) = ""
       userFormat (BigPawnMove) = "{b.p.m.}"
       userFormat (Queening p) = "=" ++ userFormat p
       userFormat (EnPassant) = "e.p."

 extractSrcFromPlayMove :: PlayMove -> BoardPos
 extractSrcFromPlayMove (PlayMove _ src _ _) = src

 extractDestFromPlayMove :: PlayMove -> BoardPos
 extractDestFromPlayMove (PlayMove _ _ dest _)       = dest

 extractSpecialFromPlayMove :: PlayMove -> SpecialMove
 extractSpecialFromPlayMove (PlayMove _ _ _ sp)       = sp






 data BoardSquare
       = VacantSq
       | WhitesSq Piece
       | BlacksSq Piece

 data SquareContent
       = Vacant
       | Friendly
       | Baddy
       | OffBoard deriving (Eq)

 instance Presentable SquareContent where
       userFormat Vacant   = "."
       userFormat Friendly = "*"
       userFormat Baddy    = "#"
       userFormat OffBoard = "?"






 data Board 
       = Board (Array BoardPos BoardSquare)
               MoveNumber              -- current player & and move
               (Maybe ChessFile)       -- e.p. possibilties.



 displayBoard :: Colour -> Board -> [String]
 displayBoard col (Board arr _ ep) = 
       ([cjustify 33 (userFormat (changeColour col)),""] ++
       [
   concat [ (case (even x,even y) of
       (True,True)   -> showSq (x `div` 2) (y `div` 2)
       (False,False) -> "+"
       (True,False)  -> "---"
       (False,True)  -> (if x == 17 then "| " ++ show (y `div` 2) else "|"))
               | x <- [1..17::Int]]
               | y <- reverse [1..17::Int]] ++
       [concat [ "  " ++ [x] ++ " " | x <- "abcdefgh" ]] ++
       ["",cjustify 33 (userFormat col),"",
               case ep of
                Nothing -> ""
                Just p -> "EnPassant:" ++ userFormatFile p ])
    where
       make n str = take n (str ++ repeat ' ')
       lookupPlace :: Int -> Int -> BoardSquare
       lookupPlace x' y' = arr ! (x',y')

       bold :: String -> String
       bold str = map toLower str

       showSq x y = case lookupPlace x y of
               VacantSq     -> [if_dot,if_dot,if_dot]
               (WhitesSq p) -> (if_dot : userFormat p) ++ [if_dot]
               (BlacksSq p)  -> (if_dot : bold (userFormat p)) ++ [if_dot]
          where
               if_dot = if (x - y) `rem` 2 == 0 then '.' else ' '

 instance Presentable Board where
   userFormat = unlines . displayBoard White

 boardSize :: (BoardPos,BoardPos)
 boardSize = ((1,1),(8,8))




 buildBoard :: String -> Board
 buildBoard str = Board brd initMoveNumber Nothing
    where
       brd = array boardSize (zipWith (=:) allSq (mkPieces str))
       allSq = [ (x,y) | y <- reverse [1..8::Int],x <- [1..8::Int]]
       mkPieces :: String -> [BoardSquare]
       mkPieces (hd:rest) | hd `elem` "KQRNBPkqrnbp" = pc : mkPieces rest
          where
               pc = case hd of
                       'K' -> WhitesSq King    
                       'Q' -> WhitesSq Queen
                       'R' -> WhitesSq Rook
                       'N' -> WhitesSq Knight
                       'B' -> WhitesSq Bishop  
                       'P' -> WhitesSq Pawn
                       'k' -> BlacksSq King    
                       'q' -> BlacksSq Queen
                       'r' -> BlacksSq Rook
                       'n' -> BlacksSq Knight
                       'b' -> BlacksSq Bishop  
                       'p' -> BlacksSq Pawn
       mkPieces ('/':rest) = mkPieces rest
       mkPieces (c:rest) | isDigit c =
               case span isDigit rest of
                 (cs,rest') -> take (read (c:cs)) (repeat VacantSq) 
                                       ++ mkPieces rest'
       mkPieces [] = []

 startBoard :: Board   -- the uni before the big bang.
 startBoard = buildBoard "rnbqkbnr/pppppppp/32/PPPPPPPP/RNBQKBNR"

 lookupSquare :: Colour -> BoardSquare -> SquareContent
 lookupSquare _      VacantSq    = Vacant
 lookupSquare White (WhitesSq p) = Friendly
 lookupSquare Black (WhitesSq p) = Baddy
 lookupSquare White (BlacksSq p) = Baddy
 lookupSquare Black (BlacksSq p) = Friendly

 lookupBoard :: Board -> BoardPos -> SquareContent
 lookupBoard (Board arr col _) pos = 
       if inRange boardSize pos
       then lookupSquare (getMoveColour col) (arr ! pos)
       else OffBoard

 lookupBoardSquare :: Board -> BoardPos -> BoardSquare
 lookupBoardSquare (Board arr _ _) pos = arr ! pos

 getSquarePiece :: BoardSquare -> Maybe Piece
 getSquarePiece VacantSq    = Nothing
 getSquarePiece (WhitesSq p) = Just p
 getSquarePiece (BlacksSq p) = Just p

 lookupBoardPiece :: Board -> BoardPos -> Maybe Piece
 lookupBoardPiece (Board arr _ _) pos = 
     case arr ! pos of
       VacantSq -> Nothing
       WhitesSq piece -> Just piece
       BlacksSq piece -> Just piece



 {-# INLINE mkColBoardSq #-}
 mkColBoardSq :: Colour -> Piece -> BoardSquare
 mkColBoardSq White p = WhitesSq p
 mkColBoardSq Black p = BlacksSq p

 getBoardColour (Board _ mv _) = getMoveColour mv

