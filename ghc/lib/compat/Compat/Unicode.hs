{-# OPTIONS -cpp #-}
module Compat.Unicode (
    GeneralCategory(..), generalCategory,
  ) where

#if __GLASGOW_HASKELL__ > 604

import Data.Char (GeneralCategory(..), generalCategory)

#else

import Foreign.C	( CInt )
import Data.Char	( ord )

-- | Unicode General Categories (column 2 of the UnicodeData table)
-- in the order they are listed in the Unicode standard.

data GeneralCategory
        = UppercaseLetter       -- Lu  Letter, Uppercase
        | LowercaseLetter       -- Ll  Letter, Lowercase
        | TitlecaseLetter       -- Lt  Letter, Titlecase
        | ModifierLetter        -- Lm  Letter, Modifier
        | OtherLetter           -- Lo  Letter, Other
        | NonSpacingMark        -- Mn  Mark, Non-Spacing
        | SpacingCombiningMark  -- Mc  Mark, Spacing Combining
        | EnclosingMark         -- Me  Mark, Enclosing
        | DecimalNumber         -- Nd  Number, Decimal
        | LetterNumber          -- Nl  Number, Letter
        | OtherNumber           -- No  Number, Other
        | ConnectorPunctuation  -- Pc  Punctuation, Connector
        | DashPunctuation       -- Pd  Punctuation, Dash
        | OpenPunctuation       -- Ps  Punctuation, Open
        | ClosePunctuation      -- Pe  Punctuation, Close
        | InitialQuote          -- Pi  Punctuation, Initial quote
        | FinalQuote            -- Pf  Punctuation, Final quote
        | OtherPunctuation      -- Po  Punctuation, Other
        | MathSymbol            -- Sm  Symbol, Math
        | CurrencySymbol        -- Sc  Symbol, Currency
        | ModifierSymbol        -- Sk  Symbol, Modifier
        | OtherSymbol           -- So  Symbol, Other
        | Space                 -- Zs  Separator, Space
        | LineSeparator         -- Zl  Separator, Line
        | ParagraphSeparator    -- Zp  Separator, Paragraph
        | Control               -- Cc  Other, Control
        | Format                -- Cf  Other, Format
        | Surrogate             -- Cs  Other, Surrogate
        | PrivateUse            -- Co  Other, Private Use
        | NotAssigned           -- Cn  Other, Not Assigned
        deriving (Eq, Ord, Enum, Read, Show, Bounded)

-- | Retrieves the general Unicode category of the character.
generalCategory :: Char -> GeneralCategory
generalCategory c = toEnum (wgencat (fromIntegral (ord c)))

foreign import ccall unsafe "u_gencat"
  wgencat :: CInt -> Int
#endif
