{-# LANGUAGE ExistentialQuantification, NamedFieldPuns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.ParseUtils
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Parsing utilities.
-----------------------------------------------------------------------------

module Distribution.Client.ParseUtils (

    -- * Fields and field utilities
    FieldDescr(..),
    liftField,
    liftFields,
    filterFields,
    mapFieldNames,
    commandOptionToField,
    commandOptionsToFields,

    -- * Sections and utilities
    SectionDescr(..),
    liftSection,

    -- * Parsing and printing flat config
    parseFields,
    ppFields,
    ppSection,

    -- * Parsing and printing config with sections and subsections
    parseFieldsAndSections,
    ppFieldsAndSections,

    -- ** Top level of config files
    parseConfig,
    showConfig,
  )
       where

import Distribution.ParseUtils
         ( FieldDescr(..), ParseResult(..), warning, LineNo, lineNo
         , Field(..), liftField, readFieldsFlat )
import Distribution.Simple.Command
         ( OptionField, viewAsFieldDescr )

import Control.Monad    ( foldM )
import Text.PrettyPrint ( (<>), (<+>), ($+$) )
import qualified Data.Map as Map
import qualified Text.PrettyPrint as Disp
         ( Doc, text, colon, vcat, empty, isEmpty, nest )


-------------------------
-- FieldDescr utilities
--

liftFields :: (b -> a)
           -> (a -> b -> b)
           -> [FieldDescr a]
           -> [FieldDescr b]
liftFields get set = map (liftField get set)


-- | Given a collection of field descriptions, keep only a given list of them,
-- identified by name.
--
filterFields :: [String] -> [FieldDescr a] -> [FieldDescr a]
filterFields includeFields = filter ((`elem` includeFields) . fieldName)

-- | Apply a name mangling function to the field names of all the field
-- descriptions. The typical use case is to apply some prefix.
--
mapFieldNames :: (String -> String) -> [FieldDescr a] -> [FieldDescr a]
mapFieldNames mangleName =
    map (\descr -> descr { fieldName = mangleName (fieldName descr) })


-- | Reuse a command line 'OptionField' as a config file 'FieldDescr'.
--
commandOptionToField :: OptionField a -> FieldDescr a
commandOptionToField = viewAsFieldDescr

-- | Reuse a bunch of command line 'OptionField's as config file 'FieldDescr's.
--
commandOptionsToFields :: [OptionField a] -> [FieldDescr a]
commandOptionsToFields = map viewAsFieldDescr


------------------------------------------
-- SectionDescr definition and utilities
--

-- | The description of a section in a config file. It can contain both
-- fields and optionally further subsections. See also 'FieldDescr'.
--
data SectionDescr a = forall b. SectionDescr {
       sectionName        :: String,
       sectionFields      :: [FieldDescr b],
       sectionSubsections :: [SectionDescr b],
       sectionGet         :: a -> [(String, b)],
       sectionSet         :: LineNo -> String -> b -> a -> ParseResult a,
       sectionEmpty       :: b
     }

-- | To help construction of config file descriptions in a modular way it is
-- useful to define fields and sections on local types and then hoist them
-- into the parent types when combining them in bigger descriptions.
--
-- This is essentially a lens operation for 'SectionDescr' to help embedding
-- one inside another.
--
liftSection :: (b -> a)
            -> (a -> b -> b)
            -> SectionDescr a
            -> SectionDescr b
liftSection get' set' (SectionDescr name fields sections get set empty) =
    let sectionGet' = get . get'
        sectionSet' lineno param x y = do
          x' <- set lineno param x (get' y)
          return (set' x' y)
     in SectionDescr name fields sections sectionGet' sectionSet' empty


-------------------------------------
-- Parsing and printing flat config
--

-- | Parse a bunch of semi-parsed 'Field's according to a set of field
-- descriptions. It accumulates the result on top of a given initial value.
--
-- This only covers the case of flat configuration without subsections. See
-- also 'parseFieldsAndSections'.
--
parseFields :: [FieldDescr a] -> a -> [Field] -> ParseResult a
parseFields fieldDescrs =
    foldM setField
  where
    fieldMap = Map.fromList [ (fieldName f, f) | f <- fieldDescrs ]

    setField accum (F line name value) =
      case Map.lookup name fieldMap of
        Just (FieldDescr _ _ set) -> set line value accum
        Nothing -> do
          warning $ "Unrecognized field " ++ name ++ " on line " ++ show line
          return accum

    setField accum f = do
      warning $ "Unrecognized stanza on line " ++ show (lineNo f)
      return accum

-- | This is a customised version of the functions from Distribution.ParseUtils
-- that also optionally print default values for empty fields as comments.
--
ppFields :: [FieldDescr a] -> (Maybe a) -> a -> Disp.Doc
ppFields fields def cur =
    Disp.vcat [ ppField name (fmap getter def) (getter cur)
              | FieldDescr name getter _ <- fields]

ppField :: String -> (Maybe Disp.Doc) -> Disp.Doc -> Disp.Doc
ppField name mdef cur
  | Disp.isEmpty cur = maybe Disp.empty
                       (\def -> Disp.text "--" <+> Disp.text name
                                <> Disp.colon <+> def) mdef
  | otherwise        = Disp.text name <> Disp.colon <+> cur

-- | Pretty print a section.
--
-- Since 'ppFields' does not cover subsections you can use this to add them.
-- Or alternatively use a 'SectionDescr' and use 'ppFieldsAndSections'.
--
ppSection :: String -> String -> [FieldDescr a] -> (Maybe a) -> a -> Disp.Doc
ppSection name arg fields def cur
  | Disp.isEmpty fieldsDoc = Disp.empty
  | otherwise              = Disp.text name <+> argDoc
                             $+$ (Disp.nest 2 fieldsDoc)
  where
    fieldsDoc = ppFields fields def cur
    argDoc | arg == "" = Disp.empty
           | otherwise = Disp.text arg


-----------------------------------------
-- Parsing and printing non-flat config
--

-- | Much like 'parseFields' but it also allows subsections. The permitted
-- subsections are given by a list of 'SectionDescr's.
--
parseFieldsAndSections :: [FieldDescr a] -> [SectionDescr a] -> a
                       -> [Field] -> ParseResult a
parseFieldsAndSections fieldDescrs sectionDescrs =
    foldM setField
  where
    fieldMap   = Map.fromList [ (fieldName   f, f) | f <- fieldDescrs   ]
    sectionMap = Map.fromList [ (sectionName s, s) | s <- sectionDescrs ]

    setField a (F line name value) =
      case Map.lookup name fieldMap of
        Just (FieldDescr _ _ set) -> set line value a
        Nothing -> do
          warning $ "Unrecognized field '" ++ name
                 ++ "' on line " ++ show line
          return a

    setField a (Section line name param fields) =
      case Map.lookup name sectionMap of
        Just (SectionDescr _ fieldDescrs' sectionDescrs' _ set sectionEmpty) -> do
          b <- parseFieldsAndSections fieldDescrs' sectionDescrs' sectionEmpty fields
          set line param b a
        Nothing -> do
          warning $ "Unrecognized section '" ++ name
                 ++ "' on line " ++ show line
          return a

    setField accum (block@IfBlock {}) = do
      warning $ "Unrecognized stanza on line " ++ show (lineNo block)
      return accum

-- | Much like 'ppFields' but also pretty prints any subsections. Subsection
-- are only shown if they are non-empty.
--
-- Note that unlike 'ppFields', at present it does not support printing
-- default values. If needed, adding such support would be quite reasonable.
--
ppFieldsAndSections :: [FieldDescr a] -> [SectionDescr a] -> a -> Disp.Doc
ppFieldsAndSections fieldDescrs sectionDescrs val =
    ppFields fieldDescrs Nothing val
      $+$
    Disp.vcat
      [ Disp.text "" $+$ sectionDoc
      | SectionDescr {
          sectionName, sectionGet,
          sectionFields, sectionSubsections
        } <- sectionDescrs
      , (param, x) <- sectionGet val
      , let sectionDoc = ppSectionAndSubsections
                           sectionName param
                           sectionFields sectionSubsections x
      , not (Disp.isEmpty sectionDoc)
      ]

-- | Unlike 'ppSection' which has to be called directly, this gets used via
-- 'ppFieldsAndSections' and so does not need to be exported.
--
ppSectionAndSubsections :: String -> String
                        -> [FieldDescr a] -> [SectionDescr a] -> a -> Disp.Doc
ppSectionAndSubsections name arg fields sections cur
  | Disp.isEmpty fieldsDoc = Disp.empty
  | otherwise              = Disp.text name <+> argDoc
                             $+$ (Disp.nest 2 fieldsDoc)
  where
    fieldsDoc = showConfig fields sections cur
    argDoc | arg == "" = Disp.empty
           | otherwise = Disp.text arg


-----------------------------------------------
-- Top level config file parsing and printing
--

-- | Parse a string in the config file syntax into a value, based on a
-- description of the configuration file in terms of its fields and sections.
--
-- It accumulates the result on top of a given initial (typically empty) value.
--
parseConfig :: [FieldDescr a] -> [SectionDescr a] -> a
            -> String -> ParseResult a
parseConfig fieldDescrs sectionDescrs empty str =
      parseFieldsAndSections fieldDescrs sectionDescrs empty
  =<< readFieldsFlat str

-- | Render a value in the config file syntax, based on a description of the
-- configuration file in terms of its fields and sections.
--
showConfig :: [FieldDescr a] -> [SectionDescr a] -> a -> Disp.Doc
showConfig = ppFieldsAndSections

