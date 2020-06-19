{-# LANGUAGE NegativeLiterals, LexicalNegation #-}

module LexNegVsNegLit where

-- NegativeLiterals specifies that we parse x-1 as x (-1), even though it's
-- considered a shortcoming.
--
-- LexicalNegation does not change that.
--
b :: Bool
b = even-1  -- parsed as: even (-1)
            -- so it is well-typed.
            --
            -- with LexicalNegation alone, we'd get (-) even 1,
            -- but NegativeLiterals takes precedence here.

-- See also: GHC Proposal #344
