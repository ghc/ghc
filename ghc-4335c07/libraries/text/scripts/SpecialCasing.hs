-- This script processes the following source file:
--
--   http://unicode.org/Public/UNIDATA/SpecialCasing.txt

module SpecialCasing
    (
      SpecialCasing(..)
    , Case(..)
    , parseSC
    , mapSC
    ) where

import Arsec

data SpecialCasing = SC { scComments :: [Comment], scCasing :: [Case] }
                   deriving (Show)

data Case = Case {
      code :: Char
    , lower :: [Char]
    , title :: [Char]
    , upper :: [Char]
    , conditions :: String
    , name :: String
    } deriving (Eq, Ord, Show)

entries :: Parser SpecialCasing
entries = SC <$> many comment <*> many (entry <* many comment)
  where
    entry = Case <$> unichar <* semi
                 <*> unichars
                 <*> unichars
                 <*> unichars
                 <*> manyTill anyToken (string "# ")
                 <*> manyTill anyToken (char '\n')

parseSC :: FilePath -> IO (Either ParseError SpecialCasing)
parseSC name = parse entries name <$> readFile name

mapSC :: String -> (Case -> String) -> (Char -> Char) -> SpecialCasing
         -> [String]
mapSC which access twiddle (SC _ ms) =
    typ ++ (map nice . filter p $ ms) ++ [last]
  where
    typ = [which ++ "Mapping :: forall s. Char -> s -> Step (CC s) Char"
           ,"{-# NOINLINE " ++ which ++ "Mapping #-}"]
    last = which ++ "Mapping c s = Yield (to" ++ ucFirst which ++ " c) (CC s '\\0' '\\0')"
    nice c = "-- " ++ name c ++ "\n" ++
             which ++ "Mapping " ++ showC (code c) ++ " s = Yield " ++ x ++ " (CC s " ++ y ++ " " ++ z ++ ")"
       where [x,y,z] = (map showC . take 3) (access c ++ repeat '\0')
    p c = [k] /= a && a /= [twiddle k] && null (conditions c)
        where a = access c
              k = code c

ucFirst (c:cs) = toUpper c : cs
ucFirst [] = []
