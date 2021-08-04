module Options.Applicative.Builder.Internal (
  -- * Internals
  Mod(..),
  HasName(..),
  HasCompleter(..),
  HasValue(..),
  HasMetavar(..),
  OptionFields(..),
  FlagFields(..),
  CommandFields(..),
  ArgumentFields(..),
  DefaultProp(..),

  optionMod,
  fieldMod,

  baseProps,
  mkCommand,
  mkParser,
  mkOption,
  mkProps,

  internal,
  noGlobal
  ) where

import Control.Applicative
import Control.Monad (mplus)
import Data.Semigroup hiding (Option)
import Prelude

import Options.Applicative.Common
import Options.Applicative.Types

data OptionFields a = OptionFields
  { optNames :: [OptName]
  , optCompleter :: Completer
  , optNoArgError :: String -> ParseError }

data FlagFields a = FlagFields
  { flagNames :: [OptName]
  , flagActive :: a }

data CommandFields a = CommandFields
  { cmdCommands :: [(String, ParserInfo a)]
  , cmdGroup :: Maybe String }

data ArgumentFields a = ArgumentFields
  { argCompleter :: Completer }

class HasName f where
  name :: OptName -> f a -> f a

instance HasName OptionFields where
  name n fields = fields { optNames = n : optNames fields }

instance HasName FlagFields where
  name n fields = fields { flagNames = n : flagNames fields }

class HasCompleter f where
  modCompleter :: (Completer -> Completer) -> f a -> f a

instance HasCompleter OptionFields where
  modCompleter f p = p { optCompleter = f (optCompleter p) }

instance HasCompleter ArgumentFields where
  modCompleter f p = p { argCompleter = f (argCompleter p) }

class HasValue f where
  -- this is just so that it is not necessary to specify the kind of f
  hasValueDummy :: f a -> ()
instance HasValue OptionFields where
  hasValueDummy _ = ()
instance HasValue ArgumentFields where
  hasValueDummy _ = ()

class HasMetavar f where
  hasMetavarDummy :: f a -> ()
instance HasMetavar OptionFields where
  hasMetavarDummy _ = ()
instance HasMetavar ArgumentFields where
  hasMetavarDummy _ = ()
instance HasMetavar CommandFields where
  hasMetavarDummy _ = ()

-- mod --

data DefaultProp a = DefaultProp
  (Maybe a)
  (Maybe (a -> String))

instance Monoid (DefaultProp a) where
  mempty = DefaultProp Nothing Nothing
  mappend = (<>)

instance Semigroup (DefaultProp a) where
  (DefaultProp d1 s1) <> (DefaultProp d2 s2) =
    DefaultProp (d1 `mplus` d2) (s1 `mplus` s2)

-- | An option modifier.
--
-- Option modifiers are values that represent a modification of the properties
-- of an option.
--
-- The type parameter @a@ is the return type of the option, while @f@ is a
-- record containing its properties (e.g. 'OptionFields' for regular options,
-- 'FlagFields' for flags, etc...).
--
-- An option modifier consists of 3 elements:
--
--  - A field modifier, of the form @f a -> f a@. These are essentially
--  (compositions of) setters for some of the properties supported by @f@.
--
--  - An optional default value and function to display it.
--
--  - A property modifier, of the form @OptProperties -> OptProperties@. This
--  is just like the field modifier, but for properties applicable to any
--  option.
--
-- Modifiers are instances of 'Monoid', and can be composed as such.
--
-- One rarely needs to deal with modifiers directly, as most of the times it is
-- sufficient to pass them to builders (such as 'strOption' or 'flag') to
-- create options (see 'Options.Applicative.Builder').
data Mod f a = Mod (f a -> f a)
                   (DefaultProp a)
                   (OptProperties -> OptProperties)

optionMod :: (OptProperties -> OptProperties) -> Mod f a
optionMod = Mod id mempty

fieldMod :: (f a -> f a) -> Mod f a
fieldMod f = Mod f mempty id

instance Monoid (Mod f a) where
  mempty = Mod id mempty id
  mappend = (<>)

-- | @since 0.13.0.0
instance Semigroup (Mod f a) where
  Mod f1 d1 g1 <> Mod f2 d2 g2
    = Mod (f2 . f1) (d2 <> d1) (g2 . g1)

-- | Base default properties.
baseProps :: OptProperties
baseProps = OptProperties
  { propMetaVar = ""
  , propVisibility = Visible
  , propHelp = mempty
  , propShowDefault = Nothing
  , propDescMod = Nothing
  , propShowGlobal = True
  }

mkCommand :: Mod CommandFields a -> (Maybe String, [String], String -> Maybe (ParserInfo a))
mkCommand m = (group, map fst cmds, (`lookup` cmds))
  where
    Mod f _ _ = m
    CommandFields cmds group = f (CommandFields [] Nothing)

mkParser :: DefaultProp a
         -> (OptProperties -> OptProperties)
         -> OptReader a
         -> Parser a
mkParser d@(DefaultProp def _) g rdr =
  let
    o = liftOpt $ mkOption d g rdr
  in
    maybe o (\a -> o <|> pure a) def

mkOption :: DefaultProp a
         -> (OptProperties -> OptProperties)
         -> OptReader a
         -> Option a
mkOption d g rdr = Option rdr (mkProps d g)

mkProps :: DefaultProp a
        -> (OptProperties -> OptProperties)
        -> OptProperties
mkProps (DefaultProp def sdef) g = props
  where
    props = (g baseProps)
      { propShowDefault = sdef <*> def }

-- | Hide this option completely from the help text
--
-- Use 'hidden' if the option should remain visible in the full description.
internal :: Mod f a
internal = optionMod $ \p -> p { propVisibility = Internal }


-- | Suppress this option from appearing in global options
noGlobal :: Mod f a
noGlobal = optionMod $ \pp -> pp { propShowGlobal = False }
