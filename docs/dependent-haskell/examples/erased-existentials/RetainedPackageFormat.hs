{-# LANGUAGE DataKinds, ExperimentalDependentTypes, LinearTypes, RankNTypes, ScopedTypeVariables #-}

module RetainedPackageFormat
    ( OpenValue, chooseValue, relay
    , Printable, Formatter, formatValue, formattedText
    ) where

type OpenValue a = exists state. (state, state %1 -> a)

-- | Store a choice and a reader that consumes it once.
chooseValue :: forall a. Bool -> a -> a -> OpenValue a
chooseValue flag yes no = (flag, readChoice)
  where
    readChoice :: Bool %1 -> a
    readChoice True = yes
    readChoice False = no

-- | Apply a polymorphic transform to a value of the caller's type.
relay :: (forall x. x -> x) -> a -> a
relay transform value = transform value

class Show a => Printable a
instance Printable Bool
instance Printable Char

data Formatted (tag :: Bool) = Formatted String

type Formatter = foreach (tag :: Bool) -> forall a. Printable a => a -> Formatted tag

-- | Format the value. The tag indexes the result and can stay unevaluated.
formatValue :: foreach (tag :: Bool) -> forall a. Show a => a -> Formatted tag
formatValue _ value = Formatted (show value)

-- | Read the text from a formatted result.
formattedText :: Formatted tag -> String
formattedText (Formatted text) = text
