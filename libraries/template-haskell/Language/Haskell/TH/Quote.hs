{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
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
module Language.Haskell.TH.Quote(
        QuasiQuoter(..),
        quoteFile,
        -- * For backwards compatibility
        dataToQa, dataToExpQ, dataToPatQ
    ) where

import Language.Haskell.TH.Syntax

-- | The 'QuasiQuoter' type, a value @q@ of this type can be used
-- in the syntax @[q| ... string to parse ...|]@.  In fact, for
-- convenience, a 'QuasiQuoter' actually defines multiple quasiquoters
-- to be used in different splice contexts; if you are only interested
-- in defining a quasiquoter to be used for expressions, you would
-- define a 'QuasiQuoter' with only 'quoteExp', and leave the other
-- fields stubbed out with errors.
data QuasiQuoter = QuasiQuoter {
    -- | Quasi-quoter for expressions, invoked by quotes like @lhs = $[q|...]@
    quoteExp  :: String -> Q Exp,
    -- | Quasi-quoter for patterns, invoked by quotes like @f $[q|...] = rhs@
    quotePat  :: String -> Q Pat,
    -- | Quasi-quoter for types, invoked by quotes like @f :: $[q|...]@
    quoteType :: String -> Q Type,
    -- | Quasi-quoter for declarations, invoked by top-level quotes
    quoteDec  :: String -> Q [Dec]
    }

-- | 'quoteFile' takes a 'QuasiQuoter' and lifts it into one that read
-- the data out of a file.  For example, suppose 'asmq' is an 
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
