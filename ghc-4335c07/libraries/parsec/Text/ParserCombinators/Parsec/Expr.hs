-----------------------------------------------------------------------------
-- |
-- Module      :  Text.ParserCombinators.Parsec.Expr
-- Copyright   :  (c) Paolo Martini 2007
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  derek.a.elkins@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Parsec compatibility module
--
-----------------------------------------------------------------------------

module Text.ParserCombinators.Parsec.Expr
    ( Assoc (AssocNone,AssocLeft,AssocRight),
      Operator(..),
      OperatorTable,
      buildExpressionParser
    ) where

import Text.Parsec.Expr(Assoc(..))
import qualified Text.Parsec.Expr as N
import Text.ParserCombinators.Parsec(GenParser)

import Control.Monad.Identity

data Operator tok st a   = Infix  (GenParser tok st (a -> a -> a)) Assoc
                         | Prefix (GenParser tok st (a -> a))
                         | Postfix (GenParser tok st (a -> a))

type OperatorTable tok st a = [[Operator tok st a]]

convert :: Operator tok st a -> N.Operator [tok] st Identity a
convert (Infix p a) = N.Infix p a
convert (Prefix p)  = N.Prefix p
convert (Postfix p)  = N.Postfix p

buildExpressionParser :: OperatorTable tok st a
                      -> GenParser tok st a
                      -> GenParser tok st a
buildExpressionParser = N.buildExpressionParser . map (map convert)
