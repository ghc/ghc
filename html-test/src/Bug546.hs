{-# LANGUAGE Haskell2010 #-}
module Bug546 where

-- |Test:
--
-- [@[code with square \\ brackets\]@] lorem ipsum
x = 1

-- |
--
-- [@[..\]@]  Matches any of the enclosed characters. Ranges of characters can
--            be specified by separating the endpoints with a @\'-'@. @\'-'@ or
--            @']'@ can be matched by including them as the first character(s)
--            in the list. Never matches path separators: @[\/]@ matches
--            nothing at all. Named character classes can also be matched:
--            @[:x:]@ within @[]@ specifies the class named @x@, which matches
--            certain predefined characters. See below for a full list.
--
-- [@[^..\]@ or @[!..\]@] Like @[..]@, but matches any character /not/ listed.
--                        Note that @[^-x]@ is not the inverse of @[-x]@, but
--                        the range @[^-x]@.
--
-- [@\<m-n>@] Matches any integer in the range m to n, inclusive. The range may
--            be open-ended by leaving out either number: @\"\<->\"@, for
--            instance, matches any integer.
--
-- [@**/@]    Matches any number of characters, including path separators,
--            excluding the empty string.
--
-- Supported character classes:
--
-- [@[:alnum:\]@]  Equivalent to @\"0-9A-Za-z\"@.
--
-- [@[:alpha:\]@]  Equivalent to @\"A-Za-z\"@.
--
-- [@[:blank:\]@]  Equivalent to @\"\\t \"@.
--
-- [@[:cntrl:\]@]  Equivalent to @\"\\0-\\x1f\\x7f\"@.
--
-- [@[:digit:\]@]  Equivalent to @\"0-9\"@.
--
-- [@[:graph:\]@]  Equivalent to @\"!-~\"@.
--
-- [@[:lower:\]@]  Equivalent to @\"a-z\"@.
--
-- [@[:print:\]@]  Equivalent to @\" -~\"@.
--
-- [@[:punct:\]@]  Equivalent to @\"!-\/:-\@[-`{-~\"@.
--
-- [@[:space:\]@]  Equivalent to @\"\\t-\\r \"@.
--
-- [@[:upper:\]@]  Equivalent to @\"A-Z\"@.
--
-- [@[:xdigit:\]@] Equivalent to @\"0-9A-Fa-f\"@.
compile :: String -> String
compile = id