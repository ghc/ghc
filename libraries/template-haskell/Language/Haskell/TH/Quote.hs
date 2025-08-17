{-# LANGUAGE Safe #-}
{- |
Module : Language.Haskell.TH.Quote
Description : Quasi-quoting support for Template Haskell

Template Haskell supports quasiquoting, which permits users to construct
program fragments by directly writing concrete syntax.  A quasiquoter is
essentially a function with takes a string to a Template Haskell AST.
This module defines the 'QuasiQuoter' datatype, which specifies a
quasiquoter @q@ which can be invoked using the syntax
@[q| ... string to parse ... |]@ when the @QuasiQuotes@ language
extension is enabled, and some utility functions for manipulating
quasiquoters.  Nota bene: this package does not define any parsers,
that is up to you.
-}
module Language.Haskell.TH.Quote
  ( QuasiQuoter(..)
  , quoteFile
  , namedDefaultQuasiQuoter
  , defaultQuasiQuoter
  -- * For backwards compatibility
  , dataToQa, dataToExpQ, dataToPatQ
  ) where

import GHC.Boot.TH.Monad
import GHC.Boot.TH.Quote
import Language.Haskell.TH.Syntax (dataToQa, dataToExpQ, dataToPatQ)


-- | 'quoteFile' takes a 'QuasiQuoter' and lifts it into one that read
-- the data out of a file.  For example, suppose @asmq@ is an
-- assembly-language quoter, so that you can write [asmq| ld r1, r2 |]
-- as an expression. Then if you define @asmq_f = quoteFile asmq@, then
-- the quote [asmq_f|foo.s|] will take input from file @"foo.s"@ instead
-- of the inline text
quoteFile :: QuasiQuoter -> QuasiQuoter
quoteFile (QuasiQuoter { quoteExp = qe, quotePat = qp, quoteType = qt, quoteDec = qd })
  = QuasiQuoter { quoteExp = get qe, quotePat = get qp, quoteType = get qt, quoteDec = get qd }
  where
   get :: (String -> Q a) -> String -> Q a
   get old_quoter file_name = do { file_cts <- runIO (readFile file_name)
                                 ; addDependentFile file_name
                                 ; old_quoter file_cts }

-- | A 'QuasiQuoter' that fails with a helpful error message in every
-- context. It is intended to be modified to create a 'QuasiQuoter' that
-- fails in all inappropriate contexts.
--
-- For example, you could write
--
-- @
-- myPatQQ = (namedDefaultQuasiQuoter "myPatQQ")
--   { quotePat = ... }
-- @
--
-- If 'myPatQQ' is used in an expression context, the compiler will report
-- that, naming 'myPatQQ'.
--
-- See also 'defaultQuasiQuoter', which does not name the 'QuasiQuoter' in
-- the error message, and might therefore be more appropriate when
-- the users of a particular 'QuasiQuoter' tend to define local \"synonyms\"
-- for it.
namedDefaultQuasiQuoter :: String -> QuasiQuoter
namedDefaultQuasiQuoter name = QuasiQuoter
  { quoteExp = f "use in expression contexts."
  , quotePat = f "use in pattern contexts."
  , quoteType = f "use in types."
  , quoteDec = f "creating declarations."
  }
  where
    f m _ = fail $ "The " ++ name ++ " quasiquoter is not for " ++ m

-- | A 'QuasiQuoter' that fails with a helpful error message in every
-- context. It is intended to be modified to create a 'QuasiQuoter' that
-- fails in all inappropriate contexts.
--
-- For example, you could write
--
-- @
-- myExpressionQQ = defaultQuasiQuoter
--   { quoteExp = ... }
-- @
--
-- See also 'namedDefaultQuasiQuoter', which names the 'QuasiQuoter' in the
-- error messages.
defaultQuasiQuoter :: QuasiQuoter
defaultQuasiQuoter = QuasiQuoter
  { quoteExp = f "use in expression contexts."
  , quotePat = f "use in pattern contexts."
  , quoteType = f "use in types."
  , quoteDec = f "creating declarations."
  }
  where
    f m _ = fail $ "This quasiquoter is not for " ++ m
