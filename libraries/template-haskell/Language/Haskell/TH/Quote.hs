{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Language.Haskell.TH.Quote(
	QuasiQuoter(..),
        dataToQa, dataToExpQ, dataToPatQ,
        quoteFile
    ) where

import Data.Data
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax

data QuasiQuoter = QuasiQuoter { quoteExp  :: String -> Q Exp,
                                 quotePat  :: String -> Q Pat,
                                 quoteType :: String -> Q Type,
                                 quoteDec  :: String -> Q [Dec] }

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

-- | 'dataToExpQ' converts a value to a 'Q Exp' representation of the same
-- value. It takes a function to handle type-specific cases.
dataToExpQ  ::  Data a
            =>  (forall b . Data b => b -> Maybe (Q Exp))
            ->  a
            ->  Q Exp
dataToExpQ = dataToQa conE litE (foldl appE)

-- | 'dataToPatQ' converts a value to a 'Q Pat' representation of the same
-- value. It takes a function to handle type-specific cases.
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
