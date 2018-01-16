{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Distribution.Types.ModuleRenaming (
    ModuleRenaming(..),
    interpModuleRenaming,
    defaultRenaming,
    isDefaultRenaming,
) where

import Distribution.Compat.Prelude hiding (empty)
import Prelude ()

import Distribution.ModuleName
import Distribution.Parsec.Class
import Distribution.Pretty
import Distribution.Text

import qualified Data.Map                   as Map
import qualified Data.Set                   as Set
import qualified Distribution.Compat.Parsec as P
import           Distribution.Compat.ReadP  ((<++))
import qualified Distribution.Compat.ReadP  as Parse
import           Text.PrettyPrint           (hsep, parens, punctuate, text, (<+>), comma)

-- | Renaming applied to the modules provided by a package.
-- The boolean indicates whether or not to also include all of the
-- original names of modules.  Thus, @ModuleRenaming False []@ is
-- "don't expose any modules, and @ModuleRenaming True [("Data.Bool", "Bool")]@
-- is, "expose all modules, but also expose @Data.Bool@ as @Bool@".
-- If a renaming is omitted you get the 'DefaultRenaming'.
--
-- (NB: This is a list not a map so that we can preserve order.)
--
data ModuleRenaming
        -- | A module renaming/thinning; e.g., @(A as B, C as C)@
        -- brings @B@ and @C@ into scope.
        = ModuleRenaming [(ModuleName, ModuleName)]
        -- | The default renaming, bringing all exported modules
        -- into scope.
        | DefaultRenaming
        -- | Hiding renaming, e.g., @hiding (A, B)@, bringing all
        -- exported modules into scope except the hidden ones.
        | HidingRenaming [ModuleName]
    deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

-- | Interpret a 'ModuleRenaming' as a partial map from 'ModuleName'
-- to 'ModuleName'.  For efficiency, you should partially apply it
-- with 'ModuleRenaming' and then reuse it.
interpModuleRenaming :: ModuleRenaming -> ModuleName -> Maybe ModuleName
interpModuleRenaming DefaultRenaming = Just
interpModuleRenaming (ModuleRenaming rns) =
    let m = Map.fromList rns
    in \k -> Map.lookup k m
interpModuleRenaming (HidingRenaming hs) =
    let s = Set.fromList hs
    in \k -> if k `Set.member` s then Nothing else Just k

-- | The default renaming, if something is specified in @build-depends@
-- only.
defaultRenaming :: ModuleRenaming
defaultRenaming = DefaultRenaming

-- | Tests if its the default renaming; we can use a more compact syntax
-- in 'Distribution.Types.IncludeRenaming.IncludeRenaming' in this case.
isDefaultRenaming :: ModuleRenaming -> Bool
isDefaultRenaming DefaultRenaming = True
isDefaultRenaming _ = False

instance Binary ModuleRenaming where

-- NB: parentheses are mandatory, because later we may extend this syntax
-- to allow "hiding (A, B)" or other modifier words.
instance Pretty ModuleRenaming where
  pretty DefaultRenaming = mempty
  pretty (HidingRenaming hides)
        = text "hiding" <+> parens (hsep (punctuate comma (map pretty hides)))
  pretty (ModuleRenaming rns)
        = parens . hsep $ punctuate comma (map dispEntry rns)
    where dispEntry (orig, new)
            | orig == new = pretty orig
            | otherwise = pretty orig <+> text "as" <+> pretty new

instance Parsec ModuleRenaming where
    -- NB: try not necessary as the first token is obvious
    parsec = P.choice [ parseRename, parseHiding, return DefaultRenaming ]
      where
        parseRename = do
            rns <- P.between (P.char '(') (P.char ')') parseList
            P.spaces
            return (ModuleRenaming rns)
        parseHiding = do
            _ <- P.string "hiding"
            P.spaces
            hides <- P.between (P.char '(') (P.char ')')
                        (P.sepBy parsec (P.char ',' >> P.spaces))
            return (HidingRenaming hides)
        parseList =
            P.sepBy parseEntry (P.char ',' >> P.spaces)
        parseEntry = do
            orig <- parsec
            P.spaces
            P.option (orig, orig) $ do
                _ <- P.string "as"
                P.spaces
                new <- parsec
                P.spaces
                return (orig, new)



instance Text ModuleRenaming where
  parse = do fmap ModuleRenaming parseRns
             <++ parseHidingRenaming
             <++ return DefaultRenaming
    where parseRns = do
             rns <- Parse.between (Parse.char '(') (Parse.char ')') parseList
             Parse.skipSpaces
             return rns
          parseHidingRenaming = do
            _ <- Parse.string "hiding"
            Parse.skipSpaces
            hides <- Parse.between (Parse.char '(') (Parse.char ')')
                        (Parse.sepBy parse (Parse.char ',' >> Parse.skipSpaces))
            return (HidingRenaming hides)
          parseList =
            Parse.sepBy parseEntry (Parse.char ',' >> Parse.skipSpaces)
          parseEntry :: Parse.ReadP r (ModuleName, ModuleName)
          parseEntry = do
            orig <- parse
            Parse.skipSpaces
            (do _ <- Parse.string "as"
                Parse.skipSpaces
                new <- parse
                Parse.skipSpaces
                return (orig, new)
             <++
                return (orig, orig))
