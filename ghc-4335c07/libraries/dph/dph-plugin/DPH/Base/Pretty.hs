
-- | Pretty printer utilities.
--
--   This is a re-export of Daan Leijen's pretty printer package (@wl-pprint@),
--   but with a `Pretty` class that includes a `pprPrec` function.
module DPH.Base.Pretty
        ( module Text.PrettyPrint.Leijen
        , Pretty(..)
        , pprParen

        -- * Rendering
        , RenderMode (..)
        , render
        , renderPlain
        , renderIndent
        , putDoc, putDocLn)
where
import Data.Set                          (Set)
import qualified Data.Set                as Set
import qualified Text.PrettyPrint.Leijen as P
import Text.PrettyPrint.Leijen           
       hiding (Pretty(..), renderPretty, putDoc)


-- Utils ---------------------------------------------------------------------
-- | Wrap a `Doc` in parens if the predicate is true.
pprParen :: Bool -> Doc -> Doc
pprParen b c
 = if b then parens c
        else c

-- Pretty Class --------------------------------------------------------------
class Pretty a where
 ppr     :: a   -> Doc
 ppr     = pprPrec 0 

 pprPrec :: Int -> a -> Doc
 pprPrec _ = ppr

instance Pretty () where
 ppr = text . show

instance Pretty Bool where
 ppr = text . show

instance Pretty Int where
 ppr = text . show

instance Pretty Integer where
 ppr = text . show

instance Pretty Char where
 ppr = text . show

instance Pretty a => Pretty [a] where
 ppr xs  = encloseSep lbracket rbracket comma 
         $ map ppr xs

instance Pretty a => Pretty (Set a) where
 ppr xs  = encloseSep lbracket rbracket comma 
         $ map ppr $ Set.toList xs

instance (Pretty a, Pretty b) => Pretty (a, b) where
 ppr (a, b) = parens $ ppr a <> comma <> ppr b


-- Rendering ------------------------------------------------------------------
-- | How to pretty print a doc.
data RenderMode
        -- | Render the doc with indenting.
        = RenderPlain

        -- | Render the doc without indenting.
        | RenderIndent
        deriving (Eq, Show)


-- | Render a doc with the given mode.
render :: RenderMode -> Doc -> String
render mode doc
 = case mode of
        RenderPlain  -> eatSpace True $ displayS (renderCompact doc) ""
        RenderIndent -> displayS (P.renderPretty 0.8 100 doc) ""

 where  eatSpace :: Bool -> String -> String
        eatSpace _    []        = []
        eatSpace True (c:cs)
         = case c of
                ' '     -> eatSpace True cs
                '\n'    -> eatSpace True cs
                _       -> c   : eatSpace False cs

        eatSpace False (c:cs)
         = case c of
                ' '     -> ' ' : eatSpace True cs
                '\n'    -> ' ' : eatSpace True cs
                _       -> c   : eatSpace False cs


-- | Convert a `Doc` to a string without indentation.
renderPlain :: Doc -> String
renderPlain = render RenderPlain


-- | Convert a `Doc` to a string with indentation
renderIndent :: Doc -> String
renderIndent = render RenderIndent


-- | Put a `Doc` to `stdout` using the given mode.
putDoc :: RenderMode -> Doc -> IO ()
putDoc mode doc
        = putStr   $ render mode doc

-- | Put a `Doc` to `stdout` using the given mode.
putDocLn  :: RenderMode -> Doc -> IO ()
putDocLn mode doc
        = putStrLn $ render mode doc
