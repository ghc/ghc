module Table where

import Data.Char
import Data.List
import Data.Maybe (isJust, fromMaybe)
import qualified DList

type Row = [String]

type ColWidth = Int

type WrappedString = [String]

-- | Wrap a string to lines of at most the given length on whitespace
-- if possible.
wrapAt :: Int -> String -> WrappedString
wrapAt width = wrapLine
  where
    wrapLine :: String -> WrappedString
    wrapLine s =
      go width mempty (take width s : wrapLine (drop width s)) s

    go :: Int               -- ^ remaining width
       -> DList.DList Char  -- ^ accumulator
       -> WrappedString     -- ^ last good wrapping
       -> String            -- ^ remaining string
       -> WrappedString
    go 0 _     back _        = back
    go n accum _    (c:rest)
      | breakable c          = go (n-1) accum'
                                  (DList.toList accum' : wrapLine rest) rest
      where accum' = accum `DList.snoc` c
    go n accum back (c:rest) = go (n-1) (accum `DList.snoc` c) back rest
    go _ accum _    []       = [DList.toList accum]

    breakable = isSpace

transpose' :: [[a]] -> [[Maybe a]]
transpose' = goRow
  where
    peel :: [a] -> (Maybe a, [a])
    peel (x:xs) = (Just x, xs)
    peel []     = (Nothing, [])

    goRow xs =
      case unzip $ map peel xs of
        (xs', ys)
          | any isJust xs' -> xs' : goRow ys
          | otherwise      -> []

table :: [ColWidth] -> Row -> [Row] -> String
table widths hdr rows = unlines $
    [rule '-'] ++
    [formatRow hdr] ++
    [rule '='] ++
    intersperse (rule '-') (map formatRow rows) ++
    [rule '-']
  where
    formatRow :: Row -> String
    formatRow cols =
        intercalate "\n"
        $ map (rawRow . map (fromMaybe ""))
        $ transpose'
        $ zipWith wrapAt (map (subtract 4) widths) cols

    rawRow :: Row -> String
    rawRow cols = "| " ++ intercalate " | " (zipWith padTo widths cols) ++ " |"
    padTo width content = take width $ content ++ repeat ' '

    rule :: Char -> String
    rule lineChar =
      ['+',lineChar]
      ++intercalate [lineChar,'+',lineChar]
                    (map (\n -> replicate n lineChar) widths)
      ++[lineChar,'+']
