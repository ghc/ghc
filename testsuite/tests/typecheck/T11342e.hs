{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module T11342e where

import GHC.TypeLits
import Data.Type.Equality

testIsControl :: '(IsControl '\n', IsControl 'x') :~: '(True, False)
testIsControl = Refl

testIsSpace :: '(IsSpace '\n', IsSpace '6') :~: '(True, False)
testIsSpace = Refl

testIsLower :: '(IsLower 'x', IsLower 'X') :~: '(True, False)
testIsLower = Refl

testIsUpper :: '(IsUpper 'x', IsUpper 'X') :~: '(False, True)
testIsUpper = Refl

testIsAlpha :: '[IsAlpha 'X', IsAlpha 'x', IsAlpha '6', IsAlpha '/']
  :~: '[True, True, False, False]
testIsAlpha = Refl

testIsAlphaNum :: '[IsAlphaNum ',', IsAlphaNum 'x', IsAlphaNum '6', IsAlphaNum '/']
  :~: '[False, True, True, False]
testIsAlphaNum = Refl

testIsPrint :: '[IsPrint '\n', IsPrint ' ', IsPrint 'a', IsPrint ';']
  :~: '[False, True, True, True]
testIsPrint = Refl

testIsDigit :: '(IsDigit '0', IsDigit 'z') :~: '(True, False)
testIsDigit = Refl

testIsOctDigit :: '(IsOctDigit '7', IsOctDigit '8') :~: '(True, False)
testIsOctDigit = Refl

testIsHexDigit :: '[IsHexDigit 'a', IsHexDigit 'e', IsHexDigit 'd', IsHexDigit 'f', IsHexDigit 't']
  :~: '[True, True, True, True, False]
testIsHexDigit = Refl

testIsLetter :: '(IsLetter 'a', IsLetter '7') :~: '(True, False)
testIsLetter = Refl

testIsMark :: '(IsMark 'a', IsMark '\768') :~: '(False, True)
testIsMark = Refl

testIsNumber :: '[IsNumber 'Ⅸ', IsNumber '9', IsNumber '%'] :~: '[True, True, False]
testIsNumber = Refl

testIsPunctuation :: '[IsPunctuation '.', IsPunctuation ',', IsPunctuation '/', IsPunctuation '?', IsPunctuation 'r']
  :~: '[True, True, True, True, False]
testIsPunctuation = Refl

testIsSymbol :: '(IsSymbol '+', IsSymbol '0') :~: '(True, False)
testIsSymbol = Refl

testIsSeparator :: '(IsSeparator ' ', IsSeparator '\t') :~: '(True, False)
testIsSeparator = Refl

testConsSymbol :: '[ConsSymbol 'a' "bcd", ConsSymbol ' ' "hi mark"] :~: '["abcd", " hi mark"]
testConsSymbol = Refl

testUnconsSymbol :: '[UnconsSymbol "abc", UnconsSymbol ""] :~: [Just '( 'a', "bc" ), Nothing]
testUnconsSymbol = Refl

testToLower :: '[ToLower ';', ToLower 'A', ToLower 'a'] :~: '[ ';', 'a', 'a' ]
testToLower = Refl

testToUpper :: '[ToUpper ';', ToUpper 'A', ToUpper 'a'] :~: '[ ';', 'A', 'A' ]
testToUpper = Refl

testToTitle :: '[ToTitle 'ч', ToTitle 'a', ToTitle ';'] :~: [ '\1063', 'A', ';']
testToTitle = Refl

charToNatTest :: '[CharToNat 'a', CharToNat '9', CharToNat '/'] :~: '[97, 57, 47]
charToNatTest = Refl

natToCharTest :: '[ NatToChar 97, NatToChar 57, NatToChar 47] :~: ' [ 'a', '9', '/']
natToCharTest = Refl

testIsAscii :: '[ IsAscii 'a', IsAscii '1', IsAscii '\x80'] :~: '[ True, True, False]
testIsAscii = Refl

testIsLatin1 :: '[ IsLatin1 'ф', IsLatin1 'b'] :~: '[ False, True]
testIsLatin1 = Refl

testIsAsciiUpper :: '[ IsAsciiUpper 'A', IsAsciiUpper 'a'] :~: '[ True, False]
testIsAsciiUpper = Refl

testIsAsciiLower :: '[ IsAsciiLower 'A', IsAsciiLower 'a'] :~: '[ False, True]
testIsAsciiLower = Refl

testLeqChar :: LeqChar 'a' 'a' :~: True
testLeqChar = Refl
