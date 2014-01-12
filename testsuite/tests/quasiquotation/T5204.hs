{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, NamedFieldPuns, ScopedTypeVariables #-}
module Pnm where
import qualified Data.Char as Char
-- import Language.Pads.Padsc 
import Control.Monad

_ws = one_or_more Char.isSpace
  where one_or_more = undefined
        
ws, wsnl, whitechar :: RE

ws   = REd "[ \t\n\r]+" " "                      -- whitespace
wsnl = let REd wplus _ = ws in REd wplus "\n"    -- whitespace output as \n
whitechar = REd "[ \t\n\r]" "\n"                 -- one white character


[pads|

  data PGMx a = PGM "P5" ws Header whitechar (Pixmap a)
               
  data Header = Header  -- fields should be separated by whitespace
    {      width  :: Int
    ws   , height :: Int
    wsnl , constrain denominator :: Int
                    where <| 0 <= denominator && denominator < 65536 |>
    }

  data Pixmap a (h::Header) = Rows   [Row a h | wsnl] length <| height h |>
  data Row    a (h::Header) = Pixels [a h     | ws]   length <| width h |> 

  newtype Greypix (h::Header) =
     G constrain g::Int16 where <| 0 <= g && g <= denominator h |>
                              
  data PGM = PGMx Int16 Greypix

]

pgm file = do (rep, md) <- parseFile file
              return rep
