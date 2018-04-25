{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances,
             OverlappingInstances, IncoherentInstances #-}

-- | Pretty printing utils.
-- Taken from BuildBox 2.1.0.1:
--      http://hackage.haskell.org/packages/archive/buildbox/2.1.0.1/doc/html/BuildBox-Pretty.html
module Pretty
	( module Text.PrettyPrint
	, Pretty(..)
	, padRc, padR
	, padLc, padL
	, blank
	, pprEngDouble
	, pprEngInteger

    , pprTimestampAbs
    , pprTimestampEng

    , pprValidate
    , pprMap
    , renderLong
    
    , padLines
    , chunks'
    , chunks
    , trunc)
where
import Text.PrettyPrint
import Text.Printf
import Control.Monad

import GHC.RTS.Events (Timestamp)

import qualified Data.Map as M
import Data.Maybe (fromMaybe)

-- Things that can be pretty printed
class Pretty a where
 	ppr :: a -> Doc

-- Basic instances
instance Pretty Doc where
	ppr = id
	
instance Pretty Float where
	ppr = text . show

instance Pretty Int where
	ppr = int
	
instance Pretty Integer where
	ppr = text . show

{-
instance Pretty UTCTime where
	ppr = text . show
        -}

instance Pretty Timestamp where
	ppr = text . show

	
instance Pretty a => Pretty [a] where
	ppr xx 
		= lbrack <> (hcat $ punctuate (text ", ") (map ppr xx)) <> rbrack

instance Pretty String where
	ppr = text


-- | Right justify a doc, padding with a given character.
padRc :: Int -> Char -> Doc -> Doc
padRc n c str
	= (text $ replicate (n - length (render str)) c) <> str
	

-- | Right justify a string with spaces.
padR :: Int -> Doc -> Doc
padR n str	= padRc n ' ' str


-- | Left justify a string, padding with a given character.
padLc :: Int -> Char -> Doc -> Doc
padLc n c str
	= str <> (text $ replicate (n - length (render str)) c)


-- | Left justify a string with spaces.
padL :: Int -> Doc -> Doc
padL n str	= padLc n ' ' str

-- | Blank text. This is different different from `empty` because it comes out a a newline when used in a `vcat`.
blank :: Doc
blank = ppr ""


-- | Like `pprEngDouble` but don't display fractional part when the value is < 1000.
--   Good for units where fractional values might not make sense (like bytes).
pprEngInteger :: String -> Integer -> Maybe Doc
pprEngInteger unit k
    | k < 0	 = liftM (text "-" <>) $ pprEngInteger unit (-k)
    | k > 1000	 = pprEngDouble unit (fromRational $ toRational k)
    | otherwise  = Just $ text $ printf "%5d%s " k unit


-- | Pretty print an engineering value, to 4 significant figures.
--   Valid range is  10^(-24) (y\/yocto) to 10^(+24) (Y\/Yotta).
--   Out of range values yield Nothing.
--
--   examples:
--
--   @
--   liftM render $ pprEngDouble \"J\" 102400    ==>   Just \"1.024MJ\"
--   liftM render $ pprEngDouble \"s\" 0.0000123 ==>   Just \"12.30us\"
--   @
--
pprEngDouble :: String -> Double -> Maybe Doc
pprEngDouble unit k
    | k < 0      = liftM (text "-" <>) $ pprEngDouble unit (-k)
    | k >= 1e+27 = Nothing
    | k >= 1e+24 = Just $ (k*1e-24) `with` ("Y" ++ unit)
    | k >= 1e+21 = Just $ (k*1e-21) `with` ("Z" ++ unit)
    | k >= 1e+18 = Just $ (k*1e-18) `with` ("E" ++ unit)
    | k >= 1e+15 = Just $ (k*1e-15) `with` ("P" ++ unit)
    | k >= 1e+12 = Just $ (k*1e-12) `with` ("T" ++ unit)
    | k >= 1e+9  = Just $ (k*1e-9)  `with` ("G" ++ unit)
    | k >= 1e+6  = Just $ (k*1e-6)  `with` ("M" ++ unit)
    | k >= 1e+3  = Just $ (k*1e-3)  `with` ("k" ++ unit)
    | k >= 1     = Just $ k         `with` (unit ++ " ")
    | k >= 1e-3  = Just $ (k*1e+3)  `with` ("m" ++ unit)
    | k >= 1e-6  = Just $ (k*1e+6)  `with` ("u" ++ unit)
    | k >= 1e-9  = Just $ (k*1e+9)  `with` ("n" ++ unit)
    | k >= 1e-12 = Just $ (k*1e+12) `with` ("p" ++ unit)
    | k >= 1e-15 = Just $ (k*1e+15) `with` ("f" ++ unit)
    | k >= 1e-18 = Just $ (k*1e+18) `with` ("a" ++ unit)
    | k >= 1e-21 = Just $ (k*1e+21) `with` ("z" ++ unit)
    | k >= 1e-24 = Just $ (k*1e+24) `with` ("y" ++ unit)
    | k >= 1e-27 = Nothing
    | otherwise  = Just $ text $ printf "%5.0f%s " k unit
     where with (t :: Double) (u :: String)
		| t >= 1e3  = text $ printf "%.0f%s" t u
		| t >= 1e2  = text $ printf "%.1f%s" t u
		| t >= 1e1  = text $ printf "%.2f%s" t u
		| otherwise = text $ printf "%.3f%s" t u

-- | print an absolute time, in the format used by threadscope 
pprTimestampAbs :: Timestamp -> Doc
pprTimestampAbs v
 = text (printf "%.9fs" v')
 where
  v' = fromIntegral v / 1e+9 :: Double

pprTimestampEng :: Timestamp -> Doc
pprTimestampEng v
 = fromMaybe (text "-") (pprEngDouble "s" v')
 where
  v' = fromIntegral v / 1e+9

pprValidate :: (s -> Doc) -> (i -> Doc) -> Either (s, i) s -> Doc
pprValidate pprState pprInput (Left (state, input)) =
  vcat
    [ text "Invalid eventlog:"
    , text "State:"
    , pprState state 
    , text "Input:"
    , pprInput input
    ]
pprValidate pprState _ (Right state) =
  vcat [ text "Valid eventlog: ", pprState state ]

pprMap :: Ord k => (k -> Doc) -> (a -> Doc) -> M.Map k a -> Doc
pprMap pprKey pprValue m =
  vcat $ zipWith (<>)
    (map pprKey . M.keys $ m)
    (map (pprValue . (M.!) m) . M.keys $ m)



renderLong :: Doc -> String
renderLong = renderStyle (style { lineLength = 200 })

padLines :: Doc -> String -> Doc
padLines left right
 = let (x:xs) = chunks' trunc_len right
       pad'   = text $ replicate (length (render left)) ' '
   in  vcat
    $ (left <> text x) : map (\s -> pad' <> text s) xs

trunc_len :: Int
trunc_len = 100

trunc :: String -> String
trunc l
  | length l > trunc_len
  = take (trunc_len-4) l ++ " ..."
  | otherwise
  = l

chunks' :: Int -> String -> [String]
chunks' len str
 = case chunks len str of
        (x:xs) -> (x:xs)
        []     -> [""]

chunks :: Int -> String -> [String]
chunks len [] = []
chunks len str
 = let (f,r) = splitAt len str
   in  f : chunks len r
