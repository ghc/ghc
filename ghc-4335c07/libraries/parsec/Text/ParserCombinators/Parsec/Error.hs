-----------------------------------------------------------------------------
-- |
-- Module      :  Text.ParserCombinators.Parsec.Error
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

module Text.ParserCombinators.Parsec.Error
    ( Message (SysUnExpect,UnExpect,Expect,Message),
      messageString,
      messageCompare,
      messageEq,
      ParseError,
      errorPos,
      errorMessages,
      errorIsUnknown,
      showErrorMessages,
      newErrorMessage,
      newErrorUnknown,
      addErrorMessage,
      setErrorPos,
      setErrorMessage,
      mergeError
    ) where

import Text.Parsec.Error


messageCompare :: Message -> Message -> Ordering
messageCompare = compare

messageEq :: Message -> Message -> Bool
messageEq = (==)
