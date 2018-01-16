-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.ParseUtils
-- Copyright   :  (c) The University of Glasgow 2004
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Utilities for parsing 'PackageDescription' and 'InstalledPackageInfo'.
--
-- The @.cabal@ file format is not trivial, especially with the introduction
-- of configurations and the section syntax that goes with that. This module
-- has a bunch of parsing functions that is used by the @.cabal@ parser and a
-- couple others. It has the parsing framework code and also little parsers for
-- many of the formats we get in various @.cabal@ file fields, like module
-- names, comma separated lists etc.

-- This module is meant to be local-only to Distribution...

{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE Rank2Types #-}
module Distribution.ParseUtils (
        LineNo, PError(..), PWarning(..), locatedErrorMsg, syntaxError, warning,
        runP, runE, ParseResult(..), catchParseError, parseFail, showPWarning,
        Field(..), fName, lineNo,
        FieldDescr(..), ppField, ppFields, readFields, readFieldsFlat,
        showFields, showSingleNamedField, showSimpleSingleNamedField,
        parseFields, parseFieldsFlat,
        parseFilePathQ, parseTokenQ, parseTokenQ',
        parseModuleNameQ,
        parseOptVersion, parsePackageName,
        parseTestedWithQ, parseLicenseQ, parseLanguageQ, parseExtensionQ,
        parseSepList, parseCommaList, parseOptCommaList,
        showFilePath, showToken, showTestedWith, showFreeText, parseFreeText,
        field, simpleField, listField, listFieldWithSep, spaceListField,
        commaListField, commaListFieldWithSep, commaNewLineListField,
        optsField, liftField, boolField, parseQuoted, parseMaybeQuoted, indentWith,
        readPToMaybe,

        UnrecFieldParser, warnUnrec, ignoreUnrec,
  ) where

import Prelude ()
import Distribution.Compat.Prelude hiding (get)

import Distribution.Compiler
import Distribution.License
import Distribution.Version
import Distribution.ModuleName
import qualified Distribution.Compat.MonadFail as Fail
import Distribution.Compat.ReadP as ReadP hiding (get)
import Distribution.ReadE
import Distribution.Compat.Newtype
import Distribution.Parsec.Newtypes (TestedWith (..))
import Distribution.Text
import Distribution.Utils.Generic
import Distribution.Pretty
import Language.Haskell.Extension

import Text.PrettyPrint
    ( Doc, render, style, renderStyle
    , text, colon, nest, punctuate, comma, sep
    , fsep, hsep, isEmpty, vcat, mode, Mode (..)
    , ($+$), (<+>)
    )
import Data.Tree as Tree (Tree(..), flatten)
import qualified Data.Map as Map
import System.FilePath (normalise)

-- -----------------------------------------------------------------------------

type LineNo    = Int

data PError = AmbiguousParse String LineNo
            | NoParse String LineNo
            | TabsError LineNo
            | FromString String (Maybe LineNo)
        deriving (Eq, Show)

data PWarning = PWarning String
              | UTFWarning LineNo String
        deriving (Eq, Show)

showPWarning :: FilePath -> PWarning -> String
showPWarning fpath (PWarning msg) =
  normalise fpath ++ ": " ++ msg
showPWarning fpath (UTFWarning line fname) =
  normalise fpath ++ ":" ++ show line
        ++ ": Invalid UTF-8 text in the '" ++ fname ++ "' field."

data ParseResult a = ParseFailed PError | ParseOk [PWarning] a
        deriving Show

instance Functor ParseResult where
        fmap _ (ParseFailed err) = ParseFailed err
        fmap f (ParseOk ws x) = ParseOk ws $ f x

instance Applicative ParseResult where
        pure = ParseOk []
        (<*>) = ap


instance Monad ParseResult where
        return = pure
        ParseFailed err >>= _ = ParseFailed err
        ParseOk ws x >>= f = case f x of
                               ParseFailed err -> ParseFailed err
                               ParseOk ws' x' -> ParseOk (ws'++ws) x'
        fail = Fail.fail

instance Fail.MonadFail ParseResult where
        fail s = ParseFailed (FromString s Nothing)

catchParseError :: ParseResult a -> (PError -> ParseResult a)
                -> ParseResult a
p@(ParseOk _ _) `catchParseError` _ = p
ParseFailed e `catchParseError` k   = k e

parseFail :: PError -> ParseResult a
parseFail = ParseFailed

runP :: LineNo -> String -> ReadP a a -> String -> ParseResult a
runP line fieldname p s =
  case [ x | (x,"") <- results ] of
    [a] -> ParseOk (utf8Warnings line fieldname s) a
    --TODO: what is this double parse thing all about?
    --      Can't we just do the all isSpace test the first time?
    []  -> case [ x | (x,ys) <- results, all isSpace ys ] of
             [a] -> ParseOk (utf8Warnings line fieldname s) a
             []  -> ParseFailed (NoParse fieldname line)
             _   -> ParseFailed (AmbiguousParse fieldname line)
    _   -> ParseFailed (AmbiguousParse fieldname line)
  where results = readP_to_S p s

runE :: LineNo -> String -> ReadE a -> String -> ParseResult a
runE line fieldname p s =
    case runReadE p s of
      Right a -> ParseOk (utf8Warnings line fieldname s) a
      Left  e -> syntaxError line $
        "Parse of field '" ++ fieldname ++ "' failed (" ++ e ++ "): " ++ s

utf8Warnings :: LineNo -> String -> String -> [PWarning]
utf8Warnings line fieldname s =
  take 1 [ UTFWarning n fieldname
         | (n,l) <- zip [line..] (lines s)
         , '\xfffd' `elem` l ]

locatedErrorMsg :: PError -> (Maybe LineNo, String)
locatedErrorMsg (AmbiguousParse f n) = (Just n,
                                        "Ambiguous parse in field '"++f++"'.")
locatedErrorMsg (NoParse f n)        = (Just n,
                                        "Parse of field '"++f++"' failed.")
locatedErrorMsg (TabsError n)        = (Just n, "Tab used as indentation.")
locatedErrorMsg (FromString s n)     = (n, s)

syntaxError :: LineNo -> String -> ParseResult a
syntaxError n s = ParseFailed $ FromString s (Just n)

tabsError :: LineNo -> ParseResult a
tabsError ln = ParseFailed $ TabsError ln

warning :: String -> ParseResult ()
warning s = ParseOk [PWarning s] ()

-- | Field descriptor.  The parameter @a@ parameterizes over where the field's
--   value is stored in.
data FieldDescr a
  = FieldDescr
      { fieldName     :: String
      , fieldGet      :: a -> Doc
      , fieldSet      :: LineNo -> String -> a -> ParseResult a
        -- ^ @fieldSet n str x@ Parses the field value from the given input
        -- string @str@ and stores the result in @x@ if the parse was
        -- successful.  Otherwise, reports an error on line number @n@.
      }

field :: String -> (a -> Doc) -> ReadP a a -> FieldDescr a
field name showF readF =
  FieldDescr name showF (\line val _st -> runP line name readF val)

-- Lift a field descriptor storing into an 'a' to a field descriptor storing
-- into a 'b'.
liftField :: (b -> a) -> (a -> b -> b) -> FieldDescr a -> FieldDescr b
liftField get set (FieldDescr name showF parseF)
 = FieldDescr name (showF . get)
        (\line str b -> do
            a <- parseF line str (get b)
            return (set a b))

-- Parser combinator for simple fields.  Takes a field name, a pretty printer,
-- a parser function, an accessor, and a setter, returns a FieldDescr over the
-- compoid structure.
simpleField :: String -> (a -> Doc) -> ReadP a a
            -> (b -> a) -> (a -> b -> b) -> FieldDescr b
simpleField name showF readF get set
  = liftField get set $ field name showF readF

commaListFieldWithSep :: Separator -> String -> (a -> Doc) -> ReadP [a] a
                      -> (b -> [a]) -> ([a] -> b -> b) -> FieldDescr b
commaListFieldWithSep separator name showF readF get set =
   liftField get set' $
     field name showF' (parseCommaList readF)
   where
     set' xs b = set (get b ++ xs) b
     showF'    = separator . punctuate comma . map showF

commaListField :: String -> (a -> Doc) -> ReadP [a] a
                 -> (b -> [a]) -> ([a] -> b -> b) -> FieldDescr b
commaListField = commaListFieldWithSep fsep

commaNewLineListField :: String -> (a -> Doc) -> ReadP [a] a
                 -> (b -> [a]) -> ([a] -> b -> b) -> FieldDescr b
commaNewLineListField = commaListFieldWithSep sep

spaceListField :: String -> (a -> Doc) -> ReadP [a] a
                 -> (b -> [a]) -> ([a] -> b -> b) -> FieldDescr b
spaceListField name showF readF get set =
  liftField get set' $
    field name showF' (parseSpaceList readF)
  where
    set' xs b = set (get b ++ xs) b
    showF'    = fsep . map showF

listFieldWithSep :: Separator -> String -> (a -> Doc) -> ReadP [a] a
                 -> (b -> [a]) -> ([a] -> b -> b) -> FieldDescr b
listFieldWithSep separator name showF readF get set =
  liftField get set' $
    field name showF' (parseOptCommaList readF)
  where
    set' xs b = set (get b ++ xs) b
    showF'    = separator . map showF

listField :: String -> (a -> Doc) -> ReadP [a] a
          -> (b -> [a]) -> ([a] -> b -> b) -> FieldDescr b
listField = listFieldWithSep fsep

optsField :: String -> CompilerFlavor -> (b -> [(CompilerFlavor,[String])])
             -> ([(CompilerFlavor,[String])] -> b -> b) -> FieldDescr b
optsField name flavor get set =
   liftField (fromMaybe [] . lookup flavor . get)
             (\opts b -> set (reorder (update flavor opts (get b))) b) $
        field name showF (sepBy parseTokenQ' (munch1 isSpace))
  where
        update _ opts l | all null opts = l  --empty opts as if no opts
        update f opts [] = [(f,opts)]
        update f opts ((f',opts'):rest)
           | f == f'   = (f, opts' ++ opts) : rest
           | otherwise = (f',opts') : update f opts rest
        reorder = sortBy (comparing fst)
        showF   = hsep . map text

-- TODO: this is a bit smelly hack. It's because we want to parse bool fields
--       liberally but not accept new parses. We cannot do that with ReadP
--       because it does not support warnings. We need a new parser framework!
boolField :: String -> (b -> Bool) -> (Bool -> b -> b) -> FieldDescr b
boolField name get set = liftField get set (FieldDescr name showF readF)
  where
    showF = text . show
    readF line str _
      |  str == "True"  = ParseOk [] True
      |  str == "False" = ParseOk [] False
      | lstr == "true"  = ParseOk [caseWarning] True
      | lstr == "false" = ParseOk [caseWarning] False
      | otherwise       = ParseFailed (NoParse name line)
      where
        lstr = lowercase str
        caseWarning = PWarning $
          "The '" ++ name ++ "' field is case sensitive, use 'True' or 'False'."

ppFields :: [FieldDescr a] -> a -> Doc
ppFields fields x =
   vcat [ ppField name (getter x) | FieldDescr name getter _ <- fields ]

ppField :: String -> Doc -> Doc
ppField name fielddoc
   | isEmpty fielddoc         = mempty
   | name `elem` nestedFields = text name <<>> colon $+$ nest indentWith fielddoc
   | otherwise                = text name <<>> colon <+> fielddoc
   where
      nestedFields =
         [ "description"
         , "build-depends"
         , "data-files"
         , "extra-source-files"
         , "extra-tmp-files"
         , "exposed-modules"
         , "asm-sources"
         , "cmm-sources"
         , "c-sources"
         , "js-sources"
         , "extra-libraries"
         , "includes"
         , "install-includes"
         , "other-modules"
         , "autogen-modules"
         , "depends"
         ]

showFields :: [FieldDescr a] -> a -> String
showFields fields = render . ($+$ text "") . ppFields fields

showSingleNamedField :: [FieldDescr a] -> String -> Maybe (a -> String)
showSingleNamedField fields f =
  case [ get | (FieldDescr f' get _) <- fields, f' == f ] of
    []      -> Nothing
    (get:_) -> Just (render . ppField f . get)

showSimpleSingleNamedField :: [FieldDescr a] -> String -> Maybe (a -> String)
showSimpleSingleNamedField fields f =
  case [ get | (FieldDescr f' get _) <- fields, f' == f ] of
    []      -> Nothing
    (get:_) -> Just (renderStyle myStyle . get)
 where myStyle = style { mode = LeftMode }

parseFields :: [FieldDescr a] -> a -> String -> ParseResult a
parseFields fields initial str =
  readFields str >>= accumFields fields initial

parseFieldsFlat :: [FieldDescr a] -> a -> String -> ParseResult a
parseFieldsFlat fields initial str =
  readFieldsFlat str >>= accumFields fields initial

accumFields :: [FieldDescr a] -> a -> [Field] -> ParseResult a
accumFields fields = foldM setField
  where
    fieldMap = Map.fromList
      [ (name, f) | f@(FieldDescr name _ _) <- fields ]
    setField accum (F line name value) = case Map.lookup name fieldMap of
      Just (FieldDescr _ _ set) -> set line value accum
      Nothing -> do
        warning ("Unrecognized field " ++ name ++ " on line " ++ show line)
        return accum
    setField accum f = do
      warning ("Unrecognized stanza on line " ++ show (lineNo f))
      return accum

-- | The type of a function which, given a name-value pair of an
--   unrecognized field, and the current structure being built,
--   decides whether to incorporate the unrecognized field
--   (by returning  Just x, where x is a possibly modified version
--   of the structure being built), or not (by returning Nothing).
type UnrecFieldParser a = (String,String) -> a -> Maybe a

-- | A default unrecognized field parser which simply returns Nothing,
--   i.e. ignores all unrecognized fields, so warnings will be generated.
warnUnrec :: UnrecFieldParser a
warnUnrec _ _ = Nothing

-- | A default unrecognized field parser which silently (i.e. no
--   warnings will be generated) ignores unrecognized fields, by
--   returning the structure being built unmodified.
ignoreUnrec :: UnrecFieldParser a
ignoreUnrec _ = Just

------------------------------------------------------------------------------

-- The data type for our three syntactic categories
data Field
    = F LineNo String String
      -- ^ A regular @<property>: <value>@ field
    | Section LineNo String String [Field]
      -- ^ A section with a name and possible parameter.  The syntactic
      -- structure is:
      --
      -- @
      --   <sectionname> <arg> {
      --     <field>*
      --   }
      -- @
    | IfBlock LineNo String [Field] [Field]
      -- ^ A conditional block with an optional else branch:
      --
      -- @
      --  if <condition> {
      --    <field>*
      --  } else {
      --    <field>*
      --  }
      -- @
      deriving (Show
               ,Eq)   -- for testing

lineNo :: Field -> LineNo
lineNo (F n _ _) = n
lineNo (Section n _ _ _) = n
lineNo (IfBlock n _ _ _) = n

fName :: Field -> String
fName (F _ n _) = n
fName (Section _ n _ _) = n
fName _ = error "fname: not a field or section"

readFields :: String -> ParseResult [Field]
readFields input = ifelse
               =<< traverse (mkField 0)
               =<< mkTree tokens

  where ls = (lines . normaliseLineEndings) input
        tokens = (concatMap tokeniseLine . trimLines) ls

readFieldsFlat :: String -> ParseResult [Field]
readFieldsFlat input = traverse (mkField 0)
                   =<< mkTree tokens
  where ls = (lines . normaliseLineEndings) input
        tokens = (concatMap tokeniseLineFlat . trimLines) ls

-- attach line number and determine indentation
trimLines :: [String] -> [(LineNo, Indent, HasTabs, String)]
trimLines ls = [ (lineno, indent, hastabs, trimTrailing l')
               | (lineno, l) <- zip [1..] ls
               , let (sps, l') = span isSpace l
                     indent    = length sps
                     hastabs   = '\t' `elem` sps
               , validLine l' ]
  where validLine ('-':'-':_) = False      -- Comment
        validLine []          = False      -- blank line
        validLine _           = True

-- | We parse generically based on indent level and braces '{' '}'. To do that
-- we split into lines and then '{' '}' tokens and other spans within a line.
data Token =
       -- | The 'Line' token is for bits that /start/ a line, eg:
       --
       -- > "\n  blah blah { blah"
       --
       -- tokenises to:
       --
       -- > [Line n 2 False "blah blah", OpenBracket, Span n "blah"]
       --
       -- so lines are the only ones that can have nested layout, since they
       -- have a known indentation level.
       --
       -- eg: we can't have this:
       --
       -- > if ... {
       -- > } else
       -- >     other
       --
       -- because other cannot nest under else, since else doesn't start a line
       -- so cannot have nested layout. It'd have to be:
       --
       -- > if ... {
       -- > }
       -- >   else
       -- >     other
       --
       -- but that's not so common, people would normally use layout or
       -- brackets not both in a single @if else@ construct.
       --
       -- > if ... { foo : bar }
       -- > else
       -- >    other
       --
       -- this is OK
       Line LineNo Indent HasTabs String
     | Span LineNo                String  -- ^ span in a line, following brackets
     | OpenBracket LineNo | CloseBracket LineNo

type Indent = Int
type HasTabs = Bool

-- | Tokenise a single line, splitting on '{' '}' and the spans in between.
-- Also trims leading & trailing space on those spans within the line.
tokeniseLine :: (LineNo, Indent, HasTabs, String) -> [Token]
tokeniseLine (n0, i, t, l) = case split n0 l of
                            (Span _ l':ss) -> Line n0 i t l' :ss
                            cs              -> cs
  where split _ "" = []
        split n s  = case span (\c -> c /='}' && c /= '{') s of
          ("", '{' : s') ->             OpenBracket  n : split n s'
          (w , '{' : s') -> mkspan n w (OpenBracket  n : split n s')
          ("", '}' : s') ->             CloseBracket n : split n s'
          (w , '}' : s') -> mkspan n w (CloseBracket n : split n s')
          (w ,        _) -> mkspan n w []

        mkspan n s ss | null s'   =             ss
                      | otherwise = Span n s' : ss
          where s' = trimTrailing (trimLeading s)

tokeniseLineFlat :: (LineNo, Indent, HasTabs, String) -> [Token]
tokeniseLineFlat (n0, i, t, l)
  | null l'   = []
  | otherwise = [Line n0 i t l']
  where
    l' = trimTrailing (trimLeading l)

trimLeading, trimTrailing :: String -> String
trimLeading  = dropWhile isSpace
trimTrailing = dropWhileEndLE isSpace


type SyntaxTree = Tree (LineNo, HasTabs, String)

-- | Parse the stream of tokens into a tree of them, based on indent \/ layout
mkTree :: [Token] -> ParseResult [SyntaxTree]
mkTree toks =
  layout 0 [] toks >>= \(trees, trailing) -> case trailing of
    []               -> return trees
    OpenBracket  n:_ -> syntaxError n "mismatched brackets, unexpected {"
    CloseBracket n:_ -> syntaxError n "mismatched brackets, unexpected }"
    -- the following two should never happen:
    Span n     l  :_ -> syntaxError n $ "unexpected span: " ++ show l
    Line n _ _ l  :_ -> syntaxError n $ "unexpected line: " ++ show l


-- | Parse the stream of tokens into a tree of them, based on indent
-- This parse state expect to be in a layout context, though possibly
-- nested within a braces context so we may still encounter closing braces.
layout :: Indent       -- ^ indent level of the parent\/previous line
       -> [SyntaxTree] -- ^ accumulating param, trees in this level
       -> [Token]      -- ^ remaining tokens
       -> ParseResult ([SyntaxTree], [Token])
                       -- ^ collected trees on this level and trailing tokens
layout _ a []                               = return (reverse a, [])
layout i a (s@(Line _ i' _ _):ss) | i' < i  = return (reverse a, s:ss)
layout i a (Line n _ t l:OpenBracket n':ss) = do
    (sub, ss') <- braces n' [] ss
    layout i (Node (n,t,l) sub:a) ss'

layout i a (Span n     l:OpenBracket n':ss) = do
    (sub, ss') <- braces n' [] ss
    layout i (Node (n,False,l) sub:a) ss'

-- look ahead to see if following lines are more indented, giving a sub-tree
layout i a (Line n i' t l:ss) = do
    lookahead <- layout (i'+1) [] ss
    case lookahead of
        ([], _)   -> layout i (Node (n,t,l) [] :a) ss
        (ts, ss') -> layout i (Node (n,t,l) ts :a) ss'

layout _ _ (   OpenBracket  n :_)  = syntaxError n "unexpected '{'"
layout _ a (s@(CloseBracket _):ss) = return (reverse a, s:ss)
layout _ _ (   Span n l       : _) = syntaxError n $ "unexpected span: "
                                                  ++ show l

-- | Parse the stream of tokens into a tree of them, based on explicit braces
-- This parse state expects to find a closing bracket.
braces :: LineNo       -- ^ line of the '{', used for error messages
       -> [SyntaxTree] -- ^ accumulating param, trees in this level
       -> [Token]      -- ^ remaining tokens
       -> ParseResult ([SyntaxTree],[Token])
                       -- ^ collected trees on this level and trailing tokens
braces m a (Line n _ t l:OpenBracket n':ss) = do
    (sub, ss') <- braces n' [] ss
    braces m (Node (n,t,l) sub:a) ss'

braces m a (Span n     l:OpenBracket n':ss) = do
    (sub, ss') <- braces n' [] ss
    braces m (Node (n,False,l) sub:a) ss'

braces m a (Line n i t l:ss) = do
    lookahead <- layout (i+1) [] ss
    case lookahead of
        ([], _)   -> braces m (Node (n,t,l) [] :a) ss
        (ts, ss') -> braces m (Node (n,t,l) ts :a) ss'

braces m a (Span n       l:ss) = braces m (Node (n,False,l) []:a) ss
braces _ a (CloseBracket _:ss) = return (reverse a, ss)
braces n _ []                  = syntaxError n $ "opening brace '{'"
                              ++ "has no matching closing brace '}'"
braces _ _ (OpenBracket  n:_)  = syntaxError n "unexpected '{'"

-- | Convert the parse tree into the Field AST
-- Also check for dodgy uses of tabs in indentation.
mkField :: Int -> SyntaxTree -> ParseResult Field
mkField d (Node (n,t,_) _) | d >= 1 && t = tabsError n
mkField d (Node (n,_,l) ts) = case span (\c -> isAlphaNum c || c == '-') l of
  ([], _)       -> syntaxError n $ "unrecognised field or section: " ++ show l
  (name, rest)  -> case trimLeading rest of
    (':':rest') -> do let followingLines = concatMap Tree.flatten ts
                          tabs = not (null [()| (_,True,_) <- followingLines ])
                      if tabs && d >= 1
                        then tabsError n
                        else return $ F n (map toLower name)
                                          (fieldValue rest' followingLines)
    rest'       -> do ts' <- traverse (mkField (d+1)) ts
                      return (Section n (map toLower name) rest' ts')
 where    fieldValue firstLine followingLines =
            let firstLine' = trimLeading firstLine
                followingLines' = map (\(_,_,s) -> stripDot s) followingLines
                allLines | null firstLine' =              followingLines'
                         | otherwise       = firstLine' : followingLines'
             in intercalate "\n" allLines
          stripDot "." = ""
          stripDot s   = s

-- | Convert if/then/else 'Section's to 'IfBlock's
ifelse :: [Field] -> ParseResult [Field]
ifelse [] = return []
ifelse (Section n "if"   cond thenpart
       :Section _ "else" as   elsepart:fs)
       | null cond     = syntaxError n "'if' with missing condition"
       | null thenpart = syntaxError n "'then' branch of 'if' is empty"
       | not (null as) = syntaxError n "'else' takes no arguments"
       | null elsepart = syntaxError n "'else' branch of 'if' is empty"
       | otherwise     = do tp  <- ifelse thenpart
                            ep  <- ifelse elsepart
                            fs' <- ifelse fs
                            return (IfBlock n cond tp ep:fs')
ifelse (Section n "if"   cond thenpart:fs)
       | null cond     = syntaxError n "'if' with missing condition"
       | null thenpart = syntaxError n "'then' branch of 'if' is empty"
       | otherwise     = do tp  <- ifelse thenpart
                            fs' <- ifelse fs
                            return (IfBlock n cond tp []:fs')
ifelse (Section n "else" _ _:_) = syntaxError n
                                  "stray 'else' with no preceding 'if'"
ifelse (Section n s a fs':fs) = do fs''  <- ifelse fs'
                                   fs''' <- ifelse fs
                                   return (Section n s a fs'' : fs''')
ifelse (f:fs) = do fs' <- ifelse fs
                   return (f : fs')

------------------------------------------------------------------------------

-- |parse a module name
parseModuleNameQ :: ReadP r ModuleName
parseModuleNameQ = parseMaybeQuoted parse

parseFilePathQ :: ReadP r FilePath
parseFilePathQ = parseTokenQ
  -- removed until normalise is no longer broken, was:
  --   liftM normalise parseTokenQ

betweenSpaces :: ReadP r a -> ReadP r a
betweenSpaces act = do skipSpaces
                       res <- act
                       skipSpaces
                       return res

parsePackageName :: ReadP r String
parsePackageName = do
  ns <- sepBy1 component (char '-')
  return $ intercalate "-" ns
  where
    component = do
      cs <- munch1 isAlphaNum
      if all isDigit cs then pfail else return cs
      -- each component must contain an alphabetic character, to avoid
      -- ambiguity in identifiers like foo-1 (the 1 is the version number).

parseOptVersion :: ReadP r Version
parseOptVersion = parseMaybeQuoted ver
  where ver :: ReadP r Version
        ver = parse <++ return nullVersion

parseTestedWithQ :: ReadP r (CompilerFlavor,VersionRange)
parseTestedWithQ = parseMaybeQuoted tw
  where
    tw :: ReadP r (CompilerFlavor,VersionRange)
    tw = do compiler <- parseCompilerFlavorCompat
            version <- betweenSpaces $ parse <++ return anyVersion
            return (compiler,version)

parseLicenseQ :: ReadP r License
parseLicenseQ = parseMaybeQuoted parse

-- urgh, we can't define optQuotes :: ReadP r a -> ReadP r a
-- because the "compat" version of ReadP isn't quite powerful enough.  In
-- particular, the type of <++ is ReadP r r -> ReadP r a -> ReadP r a
-- Hence the trick above to make 'lic' polymorphic.

parseLanguageQ :: ReadP r Language
parseLanguageQ = parseMaybeQuoted parse

parseExtensionQ :: ReadP r Extension
parseExtensionQ = parseMaybeQuoted parse

parseHaskellString :: ReadP r String
parseHaskellString = readS_to_P reads

parseTokenQ :: ReadP r String
parseTokenQ = parseHaskellString <++ munch1 (\x -> not (isSpace x) && x /= ',')

parseTokenQ' :: ReadP r String
parseTokenQ' = parseHaskellString <++ munch1 (not . isSpace)

parseSepList :: ReadP r b
             -> ReadP r a -- ^The parser for the stuff between commas
             -> ReadP r [a]
parseSepList sepr p = sepBy p separator
    where separator = betweenSpaces sepr

parseSpaceList :: ReadP r a -- ^The parser for the stuff between commas
               -> ReadP r [a]
parseSpaceList p = sepBy p skipSpaces

parseCommaList :: ReadP r a -- ^The parser for the stuff between commas
               -> ReadP r [a]
parseCommaList = parseSepList (ReadP.char ',')

parseOptCommaList :: ReadP r a -- ^The parser for the stuff between commas
                  -> ReadP r [a]
parseOptCommaList = parseSepList (optional (ReadP.char ','))

parseQuoted :: ReadP r a -> ReadP r a
parseQuoted = between (ReadP.char '"') (ReadP.char '"')

parseMaybeQuoted :: (forall r. ReadP r a) -> ReadP r' a
parseMaybeQuoted p = parseQuoted p <++ p

parseFreeText :: ReadP.ReadP s String
parseFreeText = ReadP.munch (const True)

readPToMaybe :: ReadP a a -> String -> Maybe a
readPToMaybe p str = listToMaybe [ r | (r,s) <- readP_to_S p str
                                     , all isSpace s ]

-------------------------------------------------------------------------------
-- Internal
-------------------------------------------------------------------------------

showTestedWith :: (CompilerFlavor, VersionRange) -> Doc
showTestedWith = pretty . pack' TestedWith
