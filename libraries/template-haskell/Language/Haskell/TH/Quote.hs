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
        dataToQa, dataToExpQ, dataToPatQ,
        quoteFile
    ) where

import Data.Data
import Language.Haskell.TH.Lib
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

-- | 'dataToQa' is a generic utility function for constructing generic
-- conversion functions from types with 'Data' instances to various
-- quasi-quoting representations.  It's used by 'dataToExpQ' and
-- 'dataToPatQ'
dataToQa  ::  forall a k q. Data a
          =>  (Name -> k)
          ->  (Lit -> Q q)
          ->  (k -> [Q q] -> Q q)
          ->  (forall b . Data b => b -> Maybe (Q q))
          ->  a
          ->  Q q
dataToQa mkCon mkLit appCon antiQ t =
    case antiQ t of
      Nothing ->
          case constrRep constr of
            AlgConstr _ ->
                appCon (mkCon conName) conArgs
              where
                conName :: Name
                conName =
                    case showConstr constr of
                      "(:)"       -> Name (mkOccName ":") (NameG DataName (mkPkgName "ghc-prim") (mkModName "GHC.Types"))
                      con@"[]"    -> Name (mkOccName con) (NameG DataName (mkPkgName "ghc-prim") (mkModName "GHC.Types"))
                      con@('(':_) -> Name (mkOccName con) (NameG DataName (mkPkgName "ghc-prim") (mkModName "GHC.Tuple"))
                      con         -> mkNameG_d (tyConPackage tycon)
                                               (tyConModule tycon)
                                               con
                  where
                    tycon :: TyCon
                    tycon = (typeRepTyCon . typeOf) t

                conArgs :: [Q q]
                conArgs = gmapQ (dataToQa mkCon mkLit appCon antiQ) t
            IntConstr n ->
                mkLit $ integerL n
            FloatConstr n ->
                mkLit $ rationalL n
            CharConstr c ->
                mkLit $ charL c
        where
          constr :: Constr
          constr = toConstr t

      Just y -> y

-- | 'dataToExpQ' converts a value to a 'Q Exp' representation of the
-- same value, in the SYB style. It is generalized to take a function
-- override type-specific cases; a useful default is 'const Nothing'
-- for no overriding.
dataToExpQ  ::  Data a
            =>  (forall b . Data b => b -> Maybe (Q Exp))
            ->  a
            ->  Q Exp
dataToExpQ = dataToQa conE litE (foldl appE)

-- | 'dataToPatQ' converts a value to a 'Q Pat' representation of the same
-- value, in the SYB style. It takes a function to handle type-specific cases,
-- alternatively, pass @const Nothing@ to get default behavior.
dataToPatQ  ::  Data a
            =>  (forall b . Data b => b -> Maybe (Q Pat))
            ->  a
            ->  Q Pat
dataToPatQ = dataToQa id litP conP

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
