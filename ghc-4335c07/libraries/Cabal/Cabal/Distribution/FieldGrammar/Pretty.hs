{-# LANGUAGE DeriveFunctor #-}
module Distribution.FieldGrammar.Pretty (
    PrettyFieldGrammar,
    prettyFieldGrammar,
    ) where

import           Distribution.Compat.Lens
import           Distribution.Compat.Newtype
import           Distribution.Compat.Prelude
import           Distribution.Pretty         (Pretty (..))
import           Distribution.Simple.Utils   (fromUTF8BS)
import           Prelude ()
import           Text.PrettyPrint            (Doc)
import qualified Text.PrettyPrint            as PP

import Distribution.FieldGrammar.Class
import Distribution.ParseUtils         (ppField)

newtype PrettyFieldGrammar s a = PrettyFG
    { fieldGrammarPretty :: s -> Doc
    }
  deriving (Functor)

instance Applicative (PrettyFieldGrammar s) where
    pure _ = PrettyFG (\_ -> mempty)
    PrettyFG f <*> PrettyFG x = PrettyFG (\s -> f s PP.$$ x s)

-- | We can use 'PrettyFieldGrammar' to pp print the @s@.
prettyFieldGrammar :: PrettyFieldGrammar s a -> s -> Doc
prettyFieldGrammar = fieldGrammarPretty

instance FieldGrammar PrettyFieldGrammar where
    blurFieldGrammar f (PrettyFG pp) = PrettyFG (pp . aview f)

    uniqueFieldAla fn _pack l = PrettyFG $ \s ->
        ppField (fromUTF8BS fn) (pretty (pack' _pack (aview l s)))

    booleanFieldDef fn l def = PrettyFG pp
      where
        pp s
            | b == def  = mempty
            | otherwise = ppField (fromUTF8BS fn) (PP.text (show b))
          where
            b = aview l s

    optionalFieldAla fn _pack l = PrettyFG pp
      where
        pp s = case aview l s of
            Nothing -> mempty
            Just a  -> ppField (fromUTF8BS fn) (pretty (pack' _pack a))

    monoidalFieldAla fn _pack l = PrettyFG pp
      where
        pp s = ppField  (fromUTF8BS fn) (pretty (pack' _pack (aview l s)))

    prefixedFields _fnPfx l = PrettyFG (pp . aview l)
      where
        pp xs = PP.vcat
            -- always print the field, even its Doc is empty
            -- i.e. don't use ppField
            [ PP.text n <<>> PP.colon PP.<+> (PP.vcat $ map PP.text $ lines s)
            | (n, s) <- xs
            -- fnPfx `isPrefixOf` n
            ]

    knownField _           = pure ()
    deprecatedSince [] _ _ = PrettyFG (\_ -> mempty)
    deprecatedSince _  _ x = x
    availableSince _       = id
    hiddenField _          = PrettyFG (\_ -> mempty)
