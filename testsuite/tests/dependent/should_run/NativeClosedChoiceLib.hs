module NativeClosedChoiceLib where

import qualified Data.Either as E
import Numeric.Natural (Natural)
import Prelude hiding (Either (..))

-- | Return either alternative, including an empty optional value.
classify :: Natural -> E.Either String (Maybe (Natural, Bool))
classify 0 = E.Left "a\n\"λ"
classify 1 = E.Right Nothing
classify n = E.Right (Just (n, odd n))

-- | Read all fields of a reconstructed value.
score :: E.Either String (Maybe (Natural, Bool)) -> Natural
score (E.Left "a\n\"λ") = 10
score (E.Right Nothing) = 20
score (E.Right (Just (n, True))) = 30 + n
score _ = 0

-- | Fail if the interpreter demands a present field.
badJust :: Natural -> Maybe Natural
badJust _ = Just (errorWithoutStackTrace "forced the present field")

-- | Fail if the interpreter starts before checking the result kind.
unsupported :: Natural -> E.Either Natural Int
unsupported _ = errorWithoutStackTrace "ran an unsupported result"

-- | Return a value without observing the argument.
ignoreUnsupported :: E.Either Natural Int -> Natural
ignoreUnsupported _ = 19

-- | An ordinary datatype with the same type name as the library datatype.
data Either a b = First a | Second b

-- | Construct the ordinary datatype for the closed evaluator.
ownEither :: Natural -> Either Natural Bool
ownEither = First

-- | Fail if result evaluation starts with incomplete constructor imports.
otherEither :: Natural -> Either Natural Bool
otherEither _ = errorWithoutStackTrace "ran an unrelated Either"
