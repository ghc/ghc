{-# LANGUAGE OverloadedStrings #-}
-- | A renderer that produces a lazy 'L.Text' value, using the Text Builder.
--
module Text.Blaze.Renderer.Text
    ( renderMarkupBuilder
    , renderMarkupBuilderWith
    , renderMarkup
    , renderMarkupWith
    , renderHtmlBuilder
    , renderHtmlBuilderWith
    , renderHtml
    , renderHtmlWith
    ) where

import Data.Monoid (mappend, mempty)
import Data.List (isInfixOf)

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as L
import Data.ByteString (ByteString)
import qualified Data.ByteString as S (isInfixOf)

import Text.Blaze.Internal
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B

-- | Escape predefined XML entities in a text value
--
escapeMarkupEntities :: Text     -- ^ Text to escape
                   -> Builder  -- ^ Resulting text builder
escapeMarkupEntities = T.foldr escape mempty
  where
    escape :: Char -> Builder -> Builder
    escape '<'  b = B.fromText "&lt;"   `mappend` b
    escape '>'  b = B.fromText "&gt;"   `mappend` b
    escape '&'  b = B.fromText "&amp;"  `mappend` b
    escape '"'  b = B.fromText "&quot;" `mappend` b
    escape '\'' b = B.fromText "&#39;"  `mappend` b
    escape x    b = B.singleton x       `mappend` b

-- | Render a 'ChoiceString'. TODO: Optimization possibility, apply static
-- argument transformation.
--
fromChoiceString :: (ByteString -> Text)  -- ^ Decoder for bytestrings
                 -> ChoiceString          -- ^ String to render
                 -> Builder               -- ^ Resulting builder
fromChoiceString _ (Static s)     = B.fromText $ getText s
fromChoiceString _ (String s)     = escapeMarkupEntities $ T.pack s
fromChoiceString _ (Text s)       = escapeMarkupEntities s
fromChoiceString d (ByteString s) = B.fromText $ d s
fromChoiceString d (PreEscaped x) = case x of
    String s -> B.fromText $ T.pack s
    Text   s -> B.fromText s
    s        -> fromChoiceString d s
fromChoiceString d (External x) = case x of
    -- Check that the sequence "</" is *not* in the external data.
    String s     -> if "</" `isInfixOf` s then mempty else B.fromText (T.pack s)
    Text   s     -> if "</" `T.isInfixOf` s then mempty else B.fromText s
    ByteString s -> if "</" `S.isInfixOf` s then mempty else B.fromText (d s)
    s            -> fromChoiceString d s
fromChoiceString d (AppendChoiceString x y) =
    fromChoiceString d x `mappend` fromChoiceString d y
fromChoiceString _ EmptyChoiceString = mempty
{-# INLINE fromChoiceString #-}

-- | Render markup to a text builder
renderMarkupBuilder :: Markup -> Builder
renderMarkupBuilder = renderMarkupBuilderWith decodeUtf8
{-# INLINE renderMarkupBuilder #-}

renderHtmlBuilder :: Markup -> Builder
renderHtmlBuilder = renderMarkupBuilder
{-# INLINE renderHtmlBuilder #-}
{-# DEPRECATED renderHtmlBuilder
    "Use renderHtmlBuilder from Text.Blaze.Html.Renderer.Text instead" #-}

-- | Render some 'Markup' to a Text 'Builder'.
--
renderMarkupBuilderWith :: (ByteString -> Text)  -- ^ Decoder for bytestrings
                        -> Markup                -- ^ Markup to render
                        -> Builder               -- ^ Resulting builder
renderMarkupBuilderWith d = go mempty
  where
    go :: Builder -> MarkupM b -> Builder
    go attrs (Parent _ open close content) =
        B.fromText (getText open)
            `mappend` attrs
            `mappend` B.singleton '>'
            `mappend` go mempty content
            `mappend` B.fromText (getText close)
    go attrs (CustomParent tag content) =
        B.singleton '<'
            `mappend` fromChoiceString d tag
            `mappend` attrs
            `mappend` B.singleton '>'
            `mappend` go mempty content
            `mappend` B.fromText "</"
            `mappend` fromChoiceString d tag
            `mappend` B.singleton '>'
    go attrs (Leaf _ begin end _) =
        B.fromText (getText begin)
            `mappend` attrs
            `mappend` B.fromText (getText end)
    go attrs (CustomLeaf tag close _) =
        B.singleton '<'
            `mappend` fromChoiceString d tag
            `mappend` attrs
            `mappend` (if close then B.fromText " />" else B.singleton '>')
    go attrs (AddAttribute _ key value h) =
        go (B.fromText (getText key)
            `mappend` fromChoiceString d value
            `mappend` B.singleton '"'
            `mappend` attrs) h
    go attrs (AddCustomAttribute key value h) =
        go (B.singleton ' '
            `mappend` fromChoiceString d key
            `mappend` B.fromText "=\""
            `mappend` fromChoiceString d value
            `mappend` B.singleton '"'
            `mappend` attrs) h
    go _ (Content content _) = fromChoiceString d content
    go _ (Comment comment _) =
        B.fromText "<!-- "
            `mappend` fromChoiceString d comment
            `mappend` " -->"
    go attrs (Append h1 h2) = go attrs h1 `mappend` go attrs h2
    go _ (Empty _) = mempty
    {-# NOINLINE go #-}
{-# INLINE renderMarkupBuilderWith #-}

renderHtmlBuilderWith :: (ByteString -> Text)  -- ^ Decoder for bytestrings
                      -> Markup                -- ^ Markup to render
                      -> Builder               -- ^ Resulting builder
renderHtmlBuilderWith = renderMarkupBuilderWith
{-# INLINE renderHtmlBuilderWith #-}
{-# DEPRECATED renderHtmlBuilderWith
    "Use renderHtmlBuilderWith from Text.Blaze.Html.Renderer.Text instead" #-}

-- | Render markup to a lazy Text value. If there are any ByteString's in the
-- input markup, this function will consider them as UTF-8 encoded values and
-- decode them that way.
--
renderMarkup :: Markup -> L.Text
renderMarkup = renderMarkupWith decodeUtf8
{-# INLINE renderMarkup #-}

renderHtml :: Markup -> L.Text
renderHtml = renderMarkup
{-# INLINE renderHtml #-}
{-# DEPRECATED renderHtml
    "Use renderHtml from Text.Blaze.Html.Renderer.Text instead" #-}

-- | Render markup to a lazy Text value. This function allows you to specify what
-- should happen with ByteString's in the input HTML. You can decode them or
-- drop them, this depends on the application...
--
renderMarkupWith :: (ByteString -> Text)  -- ^ Decoder for ByteString's.
                 -> Markup                -- ^ Markup to render
                 -> L.Text                -- Resulting lazy text
renderMarkupWith d = B.toLazyText . renderMarkupBuilderWith d

renderHtmlWith :: (ByteString -> Text)  -- ^ Decoder for ByteString's.
               -> Markup                -- ^ Markup to render
               -> L.Text                -- ^ Resulting lazy text
renderHtmlWith = renderMarkupWith
{-# DEPRECATED renderHtmlWith
    "Use renderHtmlWith from Text.Blaze.Html.Renderer.Text instead" #-}
