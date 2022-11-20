{-# LANGUAGE MagicHash #-}

module Main where

import Prelude hiding ((<>))
import Data.Foldable (for_, traverse_)
import Control.Monad (unless)
import Data.Maybe (listToMaybe)
import GHC.Data.FastString
import GHC.Utils.Ppr

check_docHead :: Doc -> IO ()
check_docHead d = do
  let str = renderStyle style{mode = LeftMode} d
  unless (fst (docHead d) == listToMaybe str) $
    putStrLn $ "Fail: " ++ show str

main :: IO ()
main =
  traverse_ check_docHead $
    units ++ pairs ++ triples ++ misc
  where
    units   = [id, nest 4] <*> [empty, text "", char 'x']
    ops     = [(<>), (<+>), ($$), ($+$), \a b -> hang a 4 b]
    pairs   = [id, nest 4] <*> (ops <*> units <*> units)
    triples =
      (ops <*> pairs <*> units) ++
      (ops <*> units <*> pairs)
    misc =
      [
        text "xString",
        ftext (fsLit "xFastString"),
        ftext (fsLit "") <> char 'x',
        ztext (zEncodeFS (fsLit "xFastZString")),
        ztext (zEncodeFS (fsLit "")) <> char 'x',
        ptext (mkPtrString# "xPtrString"#),
        ptext (mkPtrString# ""#)
      ]