
{-# OPTIONS_NO_SUCH_PRAGMA --no-such-flag #-}

-- We should parse the above as an unrecognised pragma, not as an OPTIONS
-- pragma containing "_NO_SUCH_PRAGMA -wibble". #2847.

module Test where

