{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}

module T22122_aux where

import Language.Haskell.TH.Syntax
  ( Name, Type(ConT), Lit(CharL, StringL)
  , Dec(DataD, FunD), Con(RecC), Exp(LitE, VarE, RecUpdE), Pat(VarP)
  , Clause(Clause), Body(NormalB)
  , Bang(..), SourceUnpackedness(..), SourceStrictness(..)
  , newNameIO )
import System.IO.Unsafe
  ( unsafePerformIO )


data Names a
  = Names { d1_name, d2_name
          , mkd1_name, mkd2a_name, mkd2b_name
          , d1_fld1_name, d1_fld2_name, d2_fld1_name, d2_fld2_name
          , upd_name, upd_var_name :: a }
  deriving stock ( Functor, Foldable, Traversable )

string_names :: Names String
string_names =
  Names
    { d1_name      = "D1"
    , d2_name      = "D2"
    , mkd1_name    = "MkD1"
    , mkd2a_name   = "MkD2A"
    , mkd2b_name   = "MkD2B"
    , d1_fld1_name = "fld" -- these are deliberately the same,
    , d1_fld2_name = "fld" -- to check that we correctly use the exact Names
    , d2_fld1_name = "fld" -- in a record update, and not simply the
    , d2_fld2_name = "fld" -- field label strings
    , upd_name     = "upd"
    , upd_var_name = "r"
    }

names :: Names Name
names = unsafePerformIO $ traverse newNameIO string_names

noBang :: Bang
noBang = Bang NoSourceUnpackedness NoSourceStrictness

-- data D1 = MkD1 { fld1 :: Char, fld2 :: String }
-- data D2 = MkD2A { fld1 :: Char } | MkD2B { fld2 :: String }
data_decls :: [ Dec ]
data_decls = [ d1, d2 ]
  where
    Names { .. } = names
    d1 = DataD [] d1_name [] Nothing [mkd1] []
    d2 = DataD [] d2_name [] Nothing [mkd2_a, mkd2_b] []
    mkd1 = RecC mkd1_name [(d1_fld1_name, noBang, ConT ''Char), (d1_fld2_name, noBang, ConT ''String)]
    mkd2_a = RecC mkd2a_name [(d2_fld1_name, noBang, ConT ''Char)]
    mkd2_b = RecC mkd2b_name [(d2_fld2_name, noBang, ConT ''String)]

-- upd r = r { fld1 = 'c', fld2 = "foo" }
record_upds :: [ Dec ]
record_upds = [ rec_upd ]
  where
    Names { .. } = names
    rec_upd = FunD upd_name [upd_clause]
    upd_clause = Clause [VarP upd_var_name] (NormalB rec_upd_body) []
    rec_upd_body = RecUpdE (VarE upd_var_name)
                     [ (d1_fld1_name, LitE (CharL 'c'))
                     , (d1_fld2_name, LitE (StringL "foo")) ]
