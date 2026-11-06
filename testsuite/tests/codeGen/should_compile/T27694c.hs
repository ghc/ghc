-- The switch on the general category of a character names the same block
-- in several entries of its jump table. The register allocator needs a
-- fixup block on that edge. jumpDestsOfInstr listed the block once per
-- entry, so the allocator made one fixup block per entry: all but the first
-- were dead code, and the CFG used for code layout had an edge to each of
-- them. -dasm-lint checks that the jumps and the CFG edges agree.
module T27694c (parseVersion) where

import Data.Char (isDigit, isAlphaNum)
import Text.ParserCombinators.ReadP

parseVersion :: ReadP ([Int], [String])
parseVersion = do branch <- sepBy1 (fmap read (munch1 isDigit)) (char '.')
                  tags   <- many (char '-' *> munch1 isAlphaNum)
                  pure (branch, tags)
