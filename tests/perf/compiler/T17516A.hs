-- Reduced from Codec.MIME.String.Internal.ABNF from mime-string-0.5
module T17516A
      (Parser, parse,
       pPred, pSucceed, pEOI, (<*>), (<|>), (<| ), (<!>),
       pChar, pString, (<$>), (<$ ), (<* ),
       pMany, pAtLeast, pOptDef, pMaybe
      ) where

import Prelude hiding ( (<*>), (<$>), (<*), (<$) )

newtype Parser inp res = Parser ([(inp, Pos)] -> ParseResult inp res)

data ParseResult inp res = Success res [(inp, Pos)] !Pos
                         | Fail !Pos

type Line = Integer
type Column = Integer
data Pos = Pos !Line !Column
         | EOI
    deriving (Eq, Ord)

get_pos :: [(a, Pos)] -> Pos
get_pos [] = EOI
get_pos ((_, p):_) = p

show_pos :: Pos -> String
show_pos EOI = "End of input"
show_pos (Pos l c) = "Line " ++ show l ++ ", column " ++ show c

infixl 6 <$>, <$, <*>, <*
infixr 3 <|>, <|

posify :: String -> [(Char, Pos)]
posify = f 1 1
    where f _ _ []        = []
          f l c ('\n':xs) = ('\n', Pos l c):f (l+1) 1     xs
          f l c (x   :xs) = (x,    Pos l c):f l     (c+1) xs

parse :: Parser Char a -> String -> Either a String
parse (Parser p) xs
 = case p $ posify xs of
       Success res [] _ -> Left res
       Success _ ((_, pos):_) _ ->
           Right ("Error: Only consumed up to " ++ show_pos pos)
       Fail pos ->
           Right ("Error: Failed at " ++ show_pos pos)

-- Primitive combinators

pPred :: (inp -> Bool) -> Parser inp inp
pPred p = Parser
        $ \inp -> case inp of
                      ((x, pos):inp')
                       | p x -> Success x inp' pos
                      _ -> Fail (get_pos inp)

pSucceed :: res -> Parser a res
pSucceed x = Parser $ \inp -> Success x inp (get_pos inp)

pEOI :: Parser a ()
pEOI = Parser $ \inp -> case inp of
                            [] -> Success () [] EOI
                            _ -> Fail (get_pos inp)

(<*>) :: Parser inp (a -> b) -> Parser inp a -> Parser inp b
Parser p <*> Parser q = Parser $ \inp ->
                        case p inp of
                            Fail pos -> Fail pos
                            Success f inp' pos ->
                                case q inp' of
                                    Fail pos' -> Fail (pos `max` pos')
                                    Success x inp'' pos' ->
                                        Success (f x) inp'' (pos `max` pos')

(<|>) :: Parser inp a -> Parser inp a -> Parser inp a
Parser p <|> Parser q = Parser $ \inp ->
                        case (p inp, q inp) of
                            (Fail posp, Fail posq) -> Fail (posp `max` posq)
                            (Fail posp, Success x inp' posq) ->
                                Success x inp' (posp `max` posq)
                            (Success x inp' posp, Fail posq) ->
                                Success x inp' (posp `max` posq)
                            (rp@(Success _ _ posp), rq@(Success _ _ posq))
                                -> if posp >= posq then rp else rq

(<| ) :: Parser inp a -> Parser inp a -> Parser inp a
Parser p <|  Parser q = Parser $ \inp ->
                        case p inp of
                            Fail posp ->
                                case q inp of
                                    Fail posq -> Fail (posp `max` posq)
                                    Success x inp' posq ->
                                        Success x inp' (posp `max` posq)
                            s -> s

(<!>) :: Parser inp a -> Parser inp b -> Parser inp a
Parser p <!> Parser q = Parser $ \inp -> case q inp of
                                             Fail _ ->
                                                 p inp
                                             Success _ _ pos -> Fail pos

check_fails_empty :: Parser inp a -> ()
check_fails_empty (Parser p) = case p [] of
                                   Fail _ -> ()
                                   _ -> error "check_fails_empty failed"

-- Derived combinators

pChar :: Char -> Parser Char Char
pChar c = pPred (c ==)

pString :: String -> Parser Char String
pString "" = pSucceed ""
pString (c:cs) = (:) <$> pChar c <*> pString cs

(<$>) :: (a -> b) -> Parser inp a -> Parser inp b
x <$> q = pSucceed x <*> q

(<$ ) :: a -> Parser inp b -> Parser inp a
x <$  q = pSucceed x <*  q

(<* ) :: Parser inp a -> Parser inp b -> Parser inp a
p <*  q = (\x _ -> x) <$> p <*> q

pMany :: Parser inp a -> Parser inp [a]
pMany p = check_fails_empty p `seq` ((:) <$> p <*> pMany p) <|  pSucceed []

pAtLeast :: Word -> Parser inp a -> Parser inp [a]
pAtLeast 0 p = pMany p
pAtLeast n p = (:) <$> p <*> pAtLeast (n-1) p

pOptDef :: a -> Parser inp a -> Parser inp a
pOptDef x p = p <|  pSucceed x

pMaybe :: Parser inp a -> Parser inp (Maybe a)
pMaybe p = Just <$> p <|  pSucceed Nothing
