{-# LANGUAGE CPP #-}

#ifdef __HLINT__
#elif !MIN_VERSION_base(4,5,0)
# error This module doesn't provide compat-shims for versions prior to base-4.5
#endif

-- | Bridge impedance mismatch of different @base@ versions back till @base-4.5@ (GHC 7.4.2)
module CompatPrelude
  ( ($>)
  , isSymbolChar
  ) where

#if MIN_VERSION_base(4,7,0)
import           Data.Functor                ( ($>) )
#else
import           Data.Functor                ( (<$) )
#endif

#if MIN_VERSION_base(4,9,0)
import           Text.Read.Lex                      (isSymbolChar)
#else
import           Data.Char (GeneralCategory(..), generalCategory)
#endif

#if !MIN_VERSION_base(4,7,0)
infixl 4 $>

-- | Flipped version of '<$'.
--
-- @since 4.7.0.0
($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)
#endif

#if !MIN_VERSION_base(4,9,0)
-- inlined from base-4.10.0.0
isSymbolChar :: Char -> Bool
isSymbolChar c = not (isPuncChar c) && case generalCategory c of
    MathSymbol           -> True
    CurrencySymbol       -> True
    ModifierSymbol       -> True
    OtherSymbol          -> True
    DashPunctuation      -> True
    OtherPunctuation     -> c `notElem` "'\""
    ConnectorPunctuation -> c /= '_'
    _                    -> False
  where
    -- | The @special@ character class as defined in the Haskell Report.
    isPuncChar :: Char -> Bool
    isPuncChar = (`elem` (",;()[]{}`" :: String))
#endif
