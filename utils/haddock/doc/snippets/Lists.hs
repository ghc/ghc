module Lists where

-- | This is a bulleted list:
--
--     * first item
--
--     * second item
data Bulleted

-- | __Missing blank lines__ before a list:
--     * first item
--     * second item
data Before

-- | This is an, (n) n., enumerated list:
--
--     (1) first item
--
--     2. second item
data Enumerated

-- | This is an enumerated list, with items not separated by newlines:
--
--     (1) first item
--     2. second item
--
-- This is a bulleted list, with items not separated by newlines:
--
--     * first item
--     * second item
data NotNewline

-- |
-- * first item
-- and more content for the first item
-- * second item
-- and more content for the second item
data MultilineItem

{-|
* Beginning of list
This belongs to the list above!

    > nested
    > bird
    > tracks

    * Next list
    More of the indented list.

        * Deeper

            @
            even code blocks work
            @

            * Deeper

                    1. Even deeper!
                    2. No newline separation even in indented lists.
-}
data NestedItem

{-|
    * foo

    * bar
-}
data Indentation

-- | This is a definition list:
--
--   [@foo@]: The description of @foo@.
--
--   [@bar@]: The description of @bar@.
data DefinitionList