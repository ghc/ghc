{-# LANGUAGE OverloadedStrings #-}

module Haddock.Backends.Hyperlinker.Utils
  ( hypSrcDir
  , hypSrcModuleFile
  , hypSrcModuleFile'
  , hypSrcModuleUrl
  , hypSrcModuleUrl'
  , hypSrcNameUrl
  , hypSrcLineUrl
  , hypSrcModuleNameUrl
  , hypSrcModuleLineUrl
  , hypSrcModuleUrlFormat
  , hypSrcModuleNameUrlFormat
  , hypSrcModuleLineUrlFormat
  , hypSrcModuleUrlToNameFormat
  , hypSrcPkgUrlToModuleFormat
  , spliceURL
  , spliceURL'

    -- * HIE file processing
  , PrintedType
  , recoverFullIfaceTypes
  ) where

import Haddock.Backends.Xhtml.Utils
import Haddock.Utils

import GHC
import GHC.Driver.Ppr (showSDoc)
import GHC.Iface.Ext.Types (HieAST (..), HieArgs (..), HieType (..), HieTypeFlat, TypeIndex)
import GHC.Iface.Type
import GHC.Types.Name (getOccFS, getOccString)
import GHC.Types.Var (TypeOrConstraint (..), VarBndr (..), invisArg, visArg)

import System.FilePath.Posix ((<.>), (</>))

import qualified Data.Array as A

{-# INLINE hypSrcDir #-}
hypSrcDir :: FilePath
hypSrcDir = "src"

{-# INLINE hypSrcModuleFile #-}
hypSrcModuleFile :: Module -> FilePath
hypSrcModuleFile m = moduleNameString (moduleName m) <.> "html"

hypSrcModuleFile' :: ModuleName -> FilePath
hypSrcModuleFile' mdl =
  spliceURL'
    (Just mdl)
    Nothing
    Nothing
    moduleFormat

hypSrcModuleUrl :: Module -> String
hypSrcModuleUrl = hypSrcModuleFile

hypSrcModuleUrl' :: ModuleName -> String
hypSrcModuleUrl' = hypSrcModuleFile'

{-# INLINE hypSrcNameUrl #-}
hypSrcNameUrl :: Name -> String
hypSrcNameUrl = escapeStr . getOccString

{-# INLINE hypSrcLineUrl #-}
hypSrcLineUrl :: Int -> String
hypSrcLineUrl line = "line-" ++ show line

{-# INLINE hypSrcModuleNameUrl #-}
hypSrcModuleNameUrl :: Module -> Name -> String
hypSrcModuleNameUrl mdl name = hypSrcModuleUrl mdl ++ "#" ++ hypSrcNameUrl name

{-# INLINE hypSrcModuleLineUrl #-}
hypSrcModuleLineUrl :: Module -> Int -> String
hypSrcModuleLineUrl mdl line = hypSrcModuleUrl mdl ++ "#" ++ hypSrcLineUrl line

hypSrcModuleUrlFormat :: String
hypSrcModuleUrlFormat = hypSrcDir </> moduleFormat

hypSrcModuleNameUrlFormat :: String
hypSrcModuleNameUrlFormat = hypSrcModuleUrlFormat ++ "#" ++ nameFormat

hypSrcModuleLineUrlFormat :: String
hypSrcModuleLineUrlFormat = hypSrcModuleUrlFormat ++ "#" ++ lineFormat

hypSrcModuleUrlToNameFormat :: String -> String
hypSrcModuleUrlToNameFormat url = url ++ "#" ++ nameFormat

hypSrcPkgUrlToModuleFormat :: String -> String
hypSrcPkgUrlToModuleFormat url = url </> moduleFormat

moduleFormat :: String
moduleFormat = "%{MODULE}.html"

nameFormat :: String
nameFormat = "%{NAME}"

lineFormat :: String
lineFormat = "line-%{LINE}"

-- * HIE file processing

-- This belongs in GHC.Iface.Ext.Utils...

-- | Pretty-printed type, ready to be turned into HTML by @xhtml@
type PrintedType = String

-- | Expand the flattened HIE AST into one where the types printed out and
-- ready for end-users to look at.
--
-- Using just primitives found in GHC's HIE utilities, we could write this as
-- follows:
--
-- > 'recoverFullIfaceTypes' dflags hieTypes hieAst
-- >     = 'fmap' (\ti -> 'showSDoc' df .
-- >                      'pprIfaceType' $
-- >                      'recoverFullType' ti hieTypes)
-- >       hieAst
--
-- However, this is very inefficient (both in time and space) because the
-- multiple calls to 'recoverFullType' don't share intermediate results. This
-- function fixes that.
recoverFullIfaceTypes
  :: DynFlags
  -> A.Array TypeIndex HieTypeFlat
  -- ^ flat types
  -> HieAST TypeIndex
  -- ^ flattened AST
  -> HieAST PrintedType
  -- ^ full AST
recoverFullIfaceTypes df flattened ast = fmap (printed A.!) ast
  where
    -- Splitting this out into its own array is also important: we don't want
    -- to pretty print the same type many times
    printed :: A.Array TypeIndex PrintedType
    printed = fmap (showSDoc df . pprIfaceType) unflattened

    -- The recursion in 'unflattened' is crucial - it's what gives us sharing
    -- between the IfaceType's produced
    unflattened :: A.Array TypeIndex IfaceType
    unflattened = fmap (\flatTy -> go (fmap (unflattened A.!) flatTy)) flattened

    -- Unfold an 'HieType' whose subterms have already been unfolded
    go :: HieType IfaceType -> IfaceType
    go (HTyVarTy n) = IfaceTyVar (mkIfLclName $ getOccFS n)
    go (HAppTy a b) = IfaceAppTy a (hieToIfaceArgs b)
    go (HLitTy l) = IfaceLitTy l
    go (HForAllTy ((n, k), af) t) =
      let b = (mkIfLclName $ getOccFS n, k)
       in IfaceForAllTy (Bndr (IfaceTvBndr b) af) t
    go (HFunTy w a b) = IfaceFunTy (visArg TypeLike) w a b -- t1 -> t2
    go (HQualTy con b) = IfaceFunTy (invisArg TypeLike) many_ty con b -- c => t
    go (HCastTy a) = a
    go HCoercionTy = IfaceTyVar $ mkIfLclName "<coercion type>"
    go (HTyConApp a xs) = IfaceTyConApp a (hieToIfaceArgs xs)

    -- This isn't fully faithful - we can't produce the 'Inferred' case
    hieToIfaceArgs :: HieArgs IfaceType -> IfaceAppArgs
    hieToIfaceArgs (HieArgs args) = go' args
      where
        go' [] = IA_Nil
        go' ((True, x) : xs) = IA_Arg x Required $ go' xs
        go' ((False, x) : xs) = IA_Arg x Specified $ go' xs
