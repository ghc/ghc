#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.Annotated.HughesPJClass
-- Copyright   :  (c) Trevor Elliott <revor@galois.com> 2015
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  David Terei <code@davidterei.com>
-- Stability   :  stable
-- Portability :  portable
--
-- Pretty printing class, simlar to 'Show' but nicer looking.
--
-- Note that the precedence level is a 'Rational' so there is an unlimited
-- number of levels. This module re-exports
-- 'Text.PrettyPrint.Annotated.HughesPJ'.
--
-----------------------------------------------------------------------------

module Text.PrettyPrint.Annotated.HughesPJClass (
    -- * Pretty typeclass
    Pretty(..),

    PrettyLevel(..), prettyNormal,
    prettyShow, prettyParen,

    -- re-export HughesPJ
    module Text.PrettyPrint.Annotated.HughesPJ
  ) where

import Text.PrettyPrint.Annotated.HughesPJ

-- | Level of detail in the pretty printed output. Level 0 is the least
-- detail.
newtype PrettyLevel = PrettyLevel Int
  deriving (Eq, Ord, Show)

-- | The "normal" (Level 0) of detail.
prettyNormal :: PrettyLevel
prettyNormal = PrettyLevel 0

-- | Pretty printing class. The precedence level is used in a similar way as in
-- the 'Show' class. Minimal complete definition is either 'pPrintPrec' or
-- 'pPrint'.
class Pretty a where
  pPrintPrec :: PrettyLevel -> Rational -> a -> Doc ann
  pPrintPrec _ _ = pPrint

  pPrint :: a -> Doc ann
  pPrint = pPrintPrec prettyNormal 0

  pPrintList :: PrettyLevel -> [a] -> Doc ann
  pPrintList l = brackets . fsep . punctuate comma . map (pPrintPrec l 0)

#if __GLASGOW_HASKELL__ >= 708
  {-# MINIMAL pPrintPrec | pPrint #-}
#endif

-- | Pretty print a value with the 'prettyNormal' level.
prettyShow :: (Pretty a) => a -> String
prettyShow = render . pPrint

pPrint0 :: (Pretty a) => PrettyLevel -> a -> Doc ann
pPrint0 l = pPrintPrec l 0

appPrec :: Rational
appPrec = 10

-- | Parenthesize an value if the boolean is true.
{-# DEPRECATED prettyParen "Please use 'maybeParens' instead" #-}
prettyParen :: Bool -> Doc ann -> Doc ann
prettyParen = maybeParens

-- Various Pretty instances
instance Pretty Int where pPrint = int

instance Pretty Integer where pPrint = integer

instance Pretty Float where pPrint = float

instance Pretty Double where pPrint = double

instance Pretty () where pPrint _ = text "()"

instance Pretty Bool where pPrint = text . show

instance Pretty Ordering where pPrint = text . show

instance Pretty Char where
  pPrint = char
  pPrintList _ = text . show

instance (Pretty a) => Pretty (Maybe a) where
  pPrintPrec _ _ Nothing = text "Nothing"
  pPrintPrec l p (Just x) =
    prettyParen (p > appPrec) $ text "Just" <+> pPrintPrec l (appPrec+1) x

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  pPrintPrec l p (Left x) =
    prettyParen (p > appPrec) $ text "Left" <+> pPrintPrec l (appPrec+1) x
  pPrintPrec l p (Right x) =
    prettyParen (p > appPrec) $ text "Right" <+> pPrintPrec l (appPrec+1) x

instance (Pretty a) => Pretty [a] where
  pPrintPrec l _ = pPrintList l

instance (Pretty a, Pretty b) => Pretty (a, b) where
  pPrintPrec l _ (a, b) =
    parens $ fsep $ punctuate comma [pPrint0 l a, pPrint0 l b]

instance (Pretty a, Pretty b, Pretty c) => Pretty (a, b, c) where
  pPrintPrec l _ (a, b, c) =
    parens $ fsep $ punctuate comma [pPrint0 l a, pPrint0 l b, pPrint0 l c]

instance (Pretty a, Pretty b, Pretty c, Pretty d) => Pretty (a, b, c, d) where
  pPrintPrec l _ (a, b, c, d) =
    parens $ fsep $ punctuate comma
      [pPrint0 l a, pPrint0 l b, pPrint0 l c, pPrint0 l d]

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e) => Pretty (a, b, c, d, e) where
  pPrintPrec l _ (a, b, c, d, e) =
    parens $ fsep $ punctuate comma
      [pPrint0 l a, pPrint0 l b, pPrint0 l c, pPrint0 l d, pPrint0 l e]

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e, Pretty f) => Pretty (a, b, c, d, e, f) where
  pPrintPrec l _ (a, b, c, d, e, f) =
    parens $ fsep $ punctuate comma
      [pPrint0 l a, pPrint0 l b, pPrint0 l c,
        pPrint0 l d, pPrint0 l e, pPrint0 l f]

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e, Pretty f, Pretty g) =>
         Pretty (a, b, c, d, e, f, g) where
  pPrintPrec l _ (a, b, c, d, e, f, g) =
    parens $ fsep $ punctuate comma
      [pPrint0 l a, pPrint0 l b, pPrint0 l c,
        pPrint0 l d, pPrint0 l e, pPrint0 l f, pPrint0 l g]

instance (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e, Pretty f, Pretty g, Pretty h) =>
         Pretty (a, b, c, d, e, f, g, h) where
  pPrintPrec l _ (a, b, c, d, e, f, g, h) =
    parens $ fsep $ punctuate comma
      [pPrint0 l a, pPrint0 l b, pPrint0 l c,
        pPrint0 l d, pPrint0 l e, pPrint0 l f, pPrint0 l g, pPrint0 l h]

