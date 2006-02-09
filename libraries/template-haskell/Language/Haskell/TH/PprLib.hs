{-# OPTIONS_GHC -fglasgow-exts #-}

-- Monadic front-end to Text.PrettyPrint.HughesPJ

module Language.Haskell.TH.PprLib (

	-- * The document type
        Doc,            -- Abstract, instance of Show
        PprM,

	-- * Primitive Documents
        empty,
        semi, comma, colon, space, equals,
        lparen, rparen, lbrack, rbrack, lbrace, rbrace,

	-- * Converting values into documents
        text, char, ptext,
        int, integer, float, double, rational,

	-- * Wrapping documents in delimiters
        parens, brackets, braces, quotes, doubleQuotes,

	-- * Combining documents
        (<>), (<+>), hcat, hsep, 
        ($$), ($+$), vcat, 
        sep, cat, 
        fsep, fcat, 
	nest,
        hang, punctuate,
        
	-- * Predicates on documents
	isEmpty,

    to_HPJ_Doc, pprName
  ) where


import Language.Haskell.TH.Syntax (Name(..), NameFlavour(..))
import qualified Text.PrettyPrint.HughesPJ as HPJ
import Control.Monad (liftM, liftM2)
import Data.Map ( Map )
import qualified Data.Map as Map ( lookup, insert, empty )
import GHC.Base (Int(..))

infixl 6 <> 
infixl 6 <+>
infixl 5 $$, $+$

-- ---------------------------------------------------------------------------
-- The interface

-- The primitive Doc values

instance Show Doc where
   show d = HPJ.render (to_HPJ_Doc d)

isEmpty :: Doc    -> PprM Bool;  -- ^ Returns 'True' if the document is empty

empty   :: Doc;			-- ^ An empty document
semi	:: Doc;			-- ^ A ';' character
comma	:: Doc;			-- ^ A ',' character
colon	:: Doc;			-- ^ A ':' character
space	:: Doc;			-- ^ A space character
equals	:: Doc;			-- ^ A '=' character
lparen	:: Doc;			-- ^ A '(' character
rparen	:: Doc;			-- ^ A ')' character
lbrack	:: Doc;			-- ^ A '[' character
rbrack	:: Doc;			-- ^ A ']' character
lbrace	:: Doc;			-- ^ A '{' character
rbrace	:: Doc;			-- ^ A '}' character

text	 :: String   -> Doc
ptext	 :: String   -> Doc
char 	 :: Char     -> Doc
int      :: Int      -> Doc
integer  :: Integer  -> Doc
float    :: Float    -> Doc
double   :: Double   -> Doc
rational :: Rational -> Doc


parens       :: Doc -> Doc; 	-- ^ Wrap document in @(...)@
brackets     :: Doc -> Doc;  	-- ^ Wrap document in @[...]@
braces	     :: Doc -> Doc;   	-- ^ Wrap document in @{...}@
quotes	     :: Doc -> Doc;	-- ^ Wrap document in @\'...\'@
doubleQuotes :: Doc -> Doc;	-- ^ Wrap document in @\"...\"@

-- Combining @Doc@ values

(<>)   :: Doc -> Doc -> Doc;     -- ^Beside
hcat   :: [Doc] -> Doc;          -- ^List version of '<>'
(<+>)  :: Doc -> Doc -> Doc;     -- ^Beside, separated by space
hsep   :: [Doc] -> Doc;          -- ^List version of '<+>'

($$)   :: Doc -> Doc -> Doc;     -- ^Above; if there is no
                                -- overlap it \"dovetails\" the two
($+$)   :: Doc -> Doc -> Doc;	 -- ^Above, without dovetailing.
vcat   :: [Doc] -> Doc;          -- ^List version of '$$'

cat    :: [Doc] -> Doc;          -- ^ Either hcat or vcat
sep    :: [Doc] -> Doc;          -- ^ Either hsep or vcat
fcat   :: [Doc] -> Doc;          -- ^ \"Paragraph fill\" version of cat
fsep   :: [Doc] -> Doc;          -- ^ \"Paragraph fill\" version of sep

nest   :: Int -> Doc -> Doc;     -- ^ Nested


-- GHC-specific ones.

hang :: Doc -> Int -> Doc -> Doc;	-- ^ @hang d1 n d2 = sep [d1, nest n d2]@
punctuate :: Doc -> [Doc] -> [Doc];      -- ^ @punctuate p [d1, ... dn] = [d1 \<> p, d2 \<> p, ... dn-1 \<> p, dn]@


-- ---------------------------------------------------------------------------
-- The "implementation"

type State = (Map Name HPJ.Doc, Int)
data PprM a = PprM { runPprM :: State -> (a, State) }

pprName :: Name -> Doc
pprName n@(Name o (NameU _))
 = PprM $ \s@(fm, i@(I# i'))
        -> case Map.lookup n fm of
               Just d -> (d, s)
               Nothing -> let d = HPJ.text $ show $ Name o (NameU i')
                          in (d, (Map.insert n d fm, i + 1))
pprName n = text $ show n

{-
instance Show Name where
  show (Name occ (NameU u))    = occString occ ++ "_" ++ show (I# u)
  show (Name occ NameS)        = occString occ
  show (Name occ (NameG ns m)) = modString m ++ "." ++ occString occ
      
data Name = Name OccName NameFlavour

data NameFlavour
  | NameU Int#			-- A unique local name
-}

to_HPJ_Doc :: Doc -> HPJ.Doc
to_HPJ_Doc d = fst $ runPprM d (Map.empty, 0)

instance Monad PprM where
    return x = PprM $ \s -> (x, s)
    m >>= k  = PprM $ \s -> let (x, s') = runPprM m s
                            in runPprM (k x) s'

type Doc = PprM HPJ.Doc

-- The primitive Doc values

isEmpty = liftM HPJ.isEmpty

empty = return HPJ.empty
semi = return HPJ.semi
comma = return HPJ.comma
colon = return HPJ.colon
space = return HPJ.space
equals = return HPJ.equals
lparen = return HPJ.lparen
rparen = return HPJ.rparen
lbrack = return HPJ.lbrack
rbrack = return HPJ.rbrack
lbrace = return HPJ.lbrace
rbrace = return HPJ.rbrace

text = return . HPJ.text
ptext = return . HPJ.ptext
char = return . HPJ.char
int = return . HPJ.int
integer = return . HPJ.integer
float = return . HPJ.float
double = return . HPJ.double
rational = return . HPJ.rational


parens = liftM HPJ.parens
brackets = liftM HPJ.brackets
braces = liftM HPJ.braces
quotes = liftM HPJ.quotes
doubleQuotes = liftM HPJ.doubleQuotes

-- Combining @Doc@ values

(<>) = liftM2 (HPJ.<>)
hcat = liftM HPJ.hcat . sequence
(<+>) = liftM2 (HPJ.<+>)
hsep = liftM HPJ.hsep . sequence

($$) = liftM2 (HPJ.$$)
($+$) = liftM2 (HPJ.$+$)
vcat = liftM HPJ.vcat . sequence

cat  = liftM HPJ.cat . sequence
sep  = liftM HPJ.sep . sequence
fcat = liftM HPJ.fcat . sequence
fsep = liftM HPJ.fsep . sequence

nest n = liftM (HPJ.nest n)

hang d1 n d2 = do d1' <- d1
                  d2' <- d2
                  return (HPJ.hang d1' n d2')

-- punctuate uses the same definition as Text.PrettyPrint.HughesPJ
punctuate p []     = []
punctuate p (d:ds) = go d ds
                   where
                     go d [] = [d]
                     go d (e:es) = (d <> p) : go e es

