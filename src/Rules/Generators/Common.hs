module Rules.Generators.Common (trackSource, yesNo, cppify) where

import Base
import Expression

-- | Track a given source file when constructing an expression.
trackSource :: FilePath -> Expr ()
trackSource file = expr $ need [ sourcePath -/- file ]

-- | Turn a 'Bool' computed by an 'Action' into a 'String' expression returning
-- "YES" (when the Boolean is 'True') or "NO" (when the Boolean is 'False').
yesNo :: Action Bool -> Expr String
yesNo = expr . fmap (\x -> if x then "YES" else "NO")

-- | Given a 'String' replace charaters '.' and '-' by underscores ('_') so that
-- the resulting 'String' becomes a valid C identifier.
cppify :: String -> String
cppify = replaceEq '-' '_' . replaceEq '.' '_'
