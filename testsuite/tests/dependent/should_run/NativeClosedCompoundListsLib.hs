module NativeClosedCompoundListsLib where

import Numeric.Natural (Natural)

-- | Supply optional elements with both constructors.
maybes :: Natural -> [Maybe Natural]
maybes n = [Nothing, Just n, Nothing, Just (n + 1)]

-- | Supply sum elements with escaped and non-ASCII characters.
alternatives :: Natural -> [Either Char Natural]
alternatives n = [Right n, Left '\n', Right (n + 1), Left 'é']

-- | Return an empty list at a compound element kind.
emptyCompound :: Natural -> [Maybe Natural]
emptyCompound _ = []

-- | Leave an exception in the last present field.
bottomElement :: Natural -> [Maybe Natural]
bottomElement n = [Just n, Just (errorWithoutStackTrace "compound list element forced")]

-- | Return a constant without inspecting the argument.
ignoreCompound :: [Maybe Natural] -> Natural
ignoreCompound _ = 7

-- | Raise an exception if the unsupported result is evaluated.
unsupported :: Natural -> [Maybe Int]
unsupported _ = errorWithoutStackTrace "unsupported compound shape ran"

-- | Use the input type to fix the missing alternatives of constructor literals.
readPair :: (Maybe Natural, Either Bool Char) -> Natural
readPair (Just n, Right 'x') = n + 1
readPair _ = 0

-- | Use the last arguments to determine the absent Left alternative.
selected :: (Eq a) => Either a Char -> a -> a -> Bool
selected (Left x) _ y = x == y
selected (Right 'x') x y = x == y
selected _ _ _ = False

-- | The empty list and the present value have the same parameter.
emptyWith :: [a] -> Maybe a -> Bool
emptyWith [] (Just _) = True
emptyWith _ _ = False

-- | The closed literal alone must supply the type parameter.
ambiguous :: Maybe a -> Natural
ambiguous _ = errorWithoutStackTrace "ambiguous operand ran"

-- | Detect premature execution of a child.
explode :: Natural -> Natural
explode _ = errorWithoutStackTrace "nested operand ran"

-- | No instance supplies this dictionary.
class Missing a

-- | Require a dictionary after the argument kinds have been determined.
needsEvidence :: (Missing a) => Natural -> Either a Char -> a -> Natural
needsEvidence _ _ _ = 0
