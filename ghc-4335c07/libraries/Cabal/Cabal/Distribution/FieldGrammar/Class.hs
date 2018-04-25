module Distribution.FieldGrammar.Class (
    FieldGrammar (..),
    uniqueField,
    optionalField,
    optionalFieldDef,
    optionalFieldDefAla,
    monoidalField,
    deprecatedField',
    ) where

import Distribution.Compat.Lens
import Distribution.Compat.Prelude
import Prelude ()

import Data.Functor.Identity (Identity (..))

import Distribution.Compat.Newtype (Newtype)
import Distribution.Parsec.Class   (Parsec)
import Distribution.Parsec.Field
import Distribution.Pretty         (Pretty)

-- | 'FieldGrammar' is parametrised by
--
-- * @s@ which is a structure we are parsing. We need this to provide prettyprinter
-- functionality
--
-- * @a@ type of the field.
--
-- /Note:/ We'd like to have @forall s. Applicative (f s)@ context.
--
class FieldGrammar g where
    -- | Unfocus, zoom out, /blur/ 'FieldGrammar'.
    blurFieldGrammar :: ALens' a b -> g b c -> g a c

    -- | Field which should be defined, exactly once.
    uniqueFieldAla
        :: (Parsec b, Pretty b, Newtype b a)
        => FieldName   -- ^ field name
        -> (a -> b)    -- ^ 'Newtype' pack
        -> ALens' s a  -- ^ lens into the field
        -> g s a

    -- | Boolean field with a default value.
    booleanFieldDef
        :: FieldName     -- ^ field name
        -> ALens' s Bool -- ^ lens into the field
        -> Bool          -- ^ default
        -> g s Bool

    -- | Optional field.
    optionalFieldAla
        :: (Parsec b, Pretty b, Newtype b a)
        => FieldName          -- ^ field name
        -> (a -> b)           -- ^ 'pack'
        -> ALens' s (Maybe a) -- ^ lens into the field
        -> g s (Maybe a)

    -- | Monoidal field.
    --
    -- Values are combined with 'mappend'.
    --
    -- /Note:/ 'optionalFieldAla' is a @monoidalField@ with 'Last' monoid.
    --
    monoidalFieldAla
        :: (Parsec b, Pretty b, Monoid a, Newtype b a)
        => FieldName   -- ^ field name
        -> (a -> b)    -- ^ 'pack'
        -> ALens' s a  -- ^ lens into the field
        -> g s a

    -- | Parser matching all fields with a name starting with a prefix.
    prefixedFields
        :: FieldName                    -- ^ field name prefix
        -> ALens' s [(String, String)]  -- ^ lens into the field
        -> g s [(String, String)]

    -- | Known field, which we don't parse, neither pretty print.
    knownField :: FieldName -> g s ()

    -- | Field which is parsed but not pretty printed.
    hiddenField :: g s a -> g s a

    -- | Deprecated since
    deprecatedSince
        :: [Int]   -- ^ version
        -> String  -- ^ deprecation message
        -> g s a
        -> g s a

    -- | Annotate field with since spec-version.
    availableSince
        :: [Int]  -- ^ spec version
        -> g s a
        -> g s a

-- | Field which can be defined at most once.
uniqueField
    :: (FieldGrammar g, Parsec a, Pretty a)
    => FieldName   -- ^ field name
    -> ALens' s a  -- ^ lens into the field
    -> g s a
uniqueField fn = uniqueFieldAla fn Identity

-- | Field which can be defined at most once.
optionalField
    :: (FieldGrammar g, Parsec a, Pretty a)
    => FieldName          -- ^ field name
    -> ALens' s (Maybe a) -- ^ lens into the field
    -> g s (Maybe a)
optionalField fn = optionalFieldAla fn Identity

-- | Optional field with default value.
optionalFieldDef
    :: (FieldGrammar g, Functor (g s), Parsec a, Pretty a, Eq a, Show a)
    => FieldName   -- ^ field name
    -> LensLike' (Pretext (Maybe a) (Maybe a)) s a -- ^ @'Lens'' s a@: lens into the field
    -> a           -- ^ default value
    -> g s a
optionalFieldDef fn = optionalFieldDefAla fn Identity

-- | Optional field with default value.
optionalFieldDefAla
    :: (FieldGrammar g, Functor (g s), Parsec b, Pretty b, Newtype b a, Eq a, Show a)
    => FieldName   -- ^ field name
    -> (a -> b)    -- ^ 'Newtype' pack
    -> LensLike' (Pretext (Maybe a) (Maybe a)) s a -- ^ @'Lens'' s a@: lens into the field
    -> a           -- ^ default value
    -> g s a
optionalFieldDefAla fn pack l def =
    fromMaybe def <$> optionalFieldAla fn pack (l . fromNon def)

-- | Field which can be define multiple times, and the results are @mappend@ed.
monoidalField
    :: (FieldGrammar g, Parsec a, Pretty a, Monoid a)
    => FieldName   -- ^ field name
    -> ALens' s a  -- ^ lens into the field
    -> g s a
monoidalField fn = monoidalFieldAla fn Identity

-- | Deprecated field. If found, warning is issued.
--
-- /Note:/ also it's not pretty printed!
--
deprecatedField'
    :: FieldGrammar g
    => String  -- ^ deprecation message
    -> g s a
    -> g s a
deprecatedField' = deprecatedSince []
