{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-name-shadowing #-}
module Macro.Load (readPkgIndex) where

import Macro.Types
import Macro.ReadShow ()

import Text.ParserCombinators.ReadP as ReadP hiding (get)
import qualified Text.ParserCombinators.ReadP as Parse
import qualified Text.PrettyPrint          as Disp
import Text.PrettyPrint hiding (braces, (<>))

import Data.List
import Data.Function (on)
import Data.Char as Char (chr, ord, isSpace, isUpper, toLower, isAlphaNum, isDigit)
import Data.Maybe
import Data.Tree as Tree (Tree(..), flatten)
import Data.Array (Array, accumArray, bounds, Ix(inRange), (!))
import Data.Bits
import Control.Monad
import qualified Control.Monad.Fail as Fail
import Control.Exception
import qualified Data.ByteString.Lazy.Char8 as BS
import System.FilePath (normalise, splitDirectories, takeExtension)
import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Tar.Entry as Tar

#if MIN_VERSION_base(4,11,0)
import Prelude hiding ((<>))
#endif
import Data.Semigroup hiding (option)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative(..))
import Data.Monoid hiding ((<>))
#endif

#if !MIN_VERSION_base(4,9,0)
instance Semigroup Doc where
  a <> b = mappend a b
#endif

readPkgIndex :: BS.ByteString -> Either String [GenericPackageDescription]
readPkgIndex = fmap extractCabalFiles . readTarIndex
  where
    extractCabalFiles entries =
      [ pkgDesc
      | entry@Tar.Entry {
          Tar.entryContent = Tar.NormalFile cabalFile _
        } <- entries
      , let filename = Tar.entryPath entry
      , takeExtension filename == ".cabal"
      , let pkgDesc = case parsePackageDescription
                         . ignoreBOM . fromUTF8 . BS.unpack $ cabalFile of
                        ParseOk _   pkg -> pkg
                        ParseFailed err -> error (filename ++ ": " ++ show err)
      ]

    readTarIndex :: BS.ByteString
                 -> Either String [Tar.Entry]
    readTarIndex indexFileContent = collect [] entries
      where
        entries = Tar.read indexFileContent
        collect es' Tar.Done        = Right es'
        collect es' (Tar.Next e es) = case entry e of
                           Just e' -> collect (e':es') es
                           Nothing -> collect     es'  es
        collect _   (Tar.Fail err)  = Left (show err)

        entry e
          | [_pkgname,versionStr,_] <- splitDirectories (normalise (Tar.entryPath e))
          , Just (Version _ []) <- simpleParse versionStr
          = Just e
        entry _ = Nothing

fromUTF8 :: String -> String
fromUTF8 []     = []
fromUTF8 (c:cs)
  | c <= '\x7F' = c : fromUTF8 cs
  | c <= '\xBF' = replacementChar : fromUTF8 cs
  | c <= '\xDF' = twoBytes c cs
  | c <= '\xEF' = moreBytes 3 0x800     cs (ord c .&. 0xF)
  | c <= '\xF7' = moreBytes 4 0x10000   cs (ord c .&. 0x7)
  | c <= '\xFB' = moreBytes 5 0x200000  cs (ord c .&. 0x3)
  | c <= '\xFD' = moreBytes 6 0x4000000 cs (ord c .&. 0x1)
  | otherwise   = replacementChar : fromUTF8 cs
  where
    twoBytes c0 (c1:cs')
      | ord c1 .&. 0xC0 == 0x80
      = let d = ((ord c0 .&. 0x1F) `shiftL` 6)
             .|. (ord c1 .&. 0x3F)
         in if d >= 0x80
               then  chr d           : fromUTF8 cs'
               else  replacementChar : fromUTF8 cs'
    twoBytes _ cs' = replacementChar : fromUTF8 cs'

    moreBytes :: Int -> Int -> [Char] -> Int -> [Char]
    moreBytes 1 overlong cs' acc
      | overlong <= acc && acc <= 0x10FFFF
     && (acc < 0xD800 || 0xDFFF < acc)
     && (acc < 0xFFFE || 0xFFFF < acc)
      = chr acc : fromUTF8 cs'

      | otherwise
      = replacementChar : fromUTF8 cs'

    moreBytes byteCount overlong (cn:cs') acc
      | ord cn .&. 0xC0 == 0x80
      = moreBytes (byteCount-1) overlong cs'
          ((acc `shiftL` 6) .|. ord cn .&. 0x3F)

    moreBytes _ _ cs' _
      = replacementChar : fromUTF8 cs'

    replacementChar = '\xfffd'

ignoreBOM :: String -> String
ignoreBOM ('\xFEFF':string) = string
ignoreBOM string            = string

------------------------------------------------------------------------------


type LineNo = Int

data PError = AmbiguousParse String LineNo
            | NoParse String LineNo
            | TabsError LineNo
            | FromString String (Maybe LineNo)
        deriving (Eq, Show)

data PWarning = PWarning String
              | UTFWarning LineNo String
        deriving (Eq, Show)

data ParseResult a = ParseFailed PError | ParseOk [PWarning] a
        deriving Show

instance Functor ParseResult where
        fmap _ (ParseFailed err) = ParseFailed err
        fmap f (ParseOk ws x) = ParseOk ws $ f x

instance Applicative ParseResult where
        pure = return
        (<*>) = ap


instance Monad ParseResult where
        return = ParseOk []
        ParseFailed err >>= _ = ParseFailed err
        ParseOk ws x >>= f = case f x of
                               ParseFailed err -> ParseFailed err
                               ParseOk ws' x' -> ParseOk (ws'++ws) x'

instance Fail.MonadFail ParseResult where
        fail s = ParseFailed (FromString s Nothing)

catchParseError :: ParseResult a -> (PError -> ParseResult a)
                -> ParseResult a
p@(ParseOk _ _) `catchParseError` _ = p
ParseFailed e `catchParseError` k   = k e

parseFail :: PError -> ParseResult a
parseFail = ParseFailed

runP :: LineNo -> String -> ReadP a -> String -> ParseResult a
runP line fieldname p s =
  case [ x | (x,"") <- results ] of
    [a] -> ParseOk (utf8Warnings line fieldname s) a
    -- TODO FIXME: what is this double parse thing all about? Can't we
    -- just do the all isSpace test the first time?
    []  -> case [ x | (x,ys) <- results, all isSpace ys ] of
             [a] -> ParseOk (utf8Warnings line fieldname s) a
             []  -> ParseFailed (NoParse fieldname line)
             _   -> ParseFailed (AmbiguousParse fieldname line)
    _   -> ParseFailed (AmbiguousParse fieldname line)
  where results = readP_to_S p s


-- | Parser with simple error reporting
newtype ReadE a = ReadE {_runReadE :: String -> Either ErrorMsg a}
type ErrorMsg   = String

instance Functor ReadE where
  fmap f (ReadE p) = ReadE $ \txt -> case p txt of
                                       Right a  -> Right (f a)
                                       Left err -> Left err

utf8Warnings :: LineNo -> String -> String -> [PWarning]
utf8Warnings line fieldname s =
  take 1 [ UTFWarning n fieldname
         | (n,l) <- zip [line..] (lines s)
         , '\xfffd' `elem` l ]

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
      , _fieldGet      :: a -> Doc
      , _fieldSet      :: LineNo -> String -> a -> ParseResult a
        -- ^ @fieldSet n str x@ Parses the field value from the given input
        -- string @str@ and stores the result in @x@ if the parse was
        -- successful.  Otherwise, reports an error on line number @n@.
      }

field :: String -> (a -> Doc) -> ReadP a -> FieldDescr a
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
simpleField :: String -> (a -> Doc) -> ReadP a
            -> (b -> a) -> (a -> b -> b) -> FieldDescr b
simpleField name showF readF get set
  = liftField get set $ field name showF readF

commaListField :: String -> (a -> Doc) -> ReadP a
                 -> (b -> [a]) -> ([a] -> b -> b) -> FieldDescr b
commaListField name showF readF get set =
  liftField get set' $
    field name (fsep . punctuate comma . map showF) (parseCommaList readF)
  where
    set' xs b = set (get b ++ xs) b

spaceListField :: String -> (a -> Doc) -> ReadP a
                 -> (b -> [a]) -> ([a] -> b -> b) -> FieldDescr b
spaceListField name showF readF get set =
  liftField get set' $
    field name (fsep . map showF) (parseSpaceList readF)
  where
    set' xs b = set (get b ++ xs) b

listField :: String -> (a -> Doc) -> ReadP a
                 -> (b -> [a]) -> ([a] -> b -> b) -> FieldDescr b
listField name showF readF get set =
  liftField get set' $
    field name (fsep . map showF) (parseOptCommaList readF)
  where
    set' xs b = set (get b ++ xs) b

optsField :: String -> CompilerFlavor -> (b -> [(CompilerFlavor,[String])])
             -> ([(CompilerFlavor,[String])] -> b -> b) -> FieldDescr b
optsField name flavor get set =
   liftField (fromMaybe [] . lookup flavor . get)
             (\opts b -> set (reorder (update flavor opts (get b))) b) $
        field name (hsep . map text)
                   (sepBy parseTokenQ' (munch1 isSpace))
  where
        update _ opts l | all null opts = l  --empty opts as if no opts
        update f opts [] = [(f,opts)]
        update f opts ((f',opts'):rest)
           | f == f'   = (f, opts' ++ opts) : rest
           | otherwise = (f',opts') : update f opts rest
        reorder = sortBy (compare `on` fst)

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

type UnrecFieldParser a = (String,String) -> a -> Maybe a

-- | A default unrecognized field parser which simply returns Nothing,
--   i.e. ignores all unrecognized fields, so warnings will be generated.
warnUnrec :: UnrecFieldParser a
warnUnrec _ _ = Nothing

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
               =<< mapM (mkField 0)
               =<< mkTree tokens

  where ls = (lines . normaliseLineEndings) input
        tokens = (concatMap tokeniseLine . trimLines) ls

normaliseLineEndings :: String -> String
normaliseLineEndings [] = []
normaliseLineEndings ('\r':'\n':s) = '\n' : normaliseLineEndings s -- windows
normaliseLineEndings ('\r':s)      = '\n' : normaliseLineEndings s -- old osx
normaliseLineEndings (  c :s)      =   c  : normaliseLineEndings s

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
       -- this is ok
       Line LineNo Indent HasTabs String
     | Span LineNo                String  -- ^ span in a line, following brackets
     | OpenBracket LineNo | CloseBracket LineNo

type Indent = Int
type HasTabs = Bool

-- | Tokenise a single line, splitting on '{' '}' and the spans inbetween.
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

trimLeading, trimTrailing :: String -> String
trimLeading  = dropWhile isSpace
trimTrailing = reverse . dropWhile isSpace . reverse


type SyntaxTree = Tree (LineNo, HasTabs, String)

-- | Parse the stream of tokens into a tree of them, based on indent \/ layout
mkTree :: [Token] -> ParseResult [SyntaxTree]
mkTree toks =
  layout 0 [] toks >>= \(trees, trailing) -> case trailing of
    []               -> return trees
    OpenBracket  n:_ -> syntaxError n "mismatched backets, unexpected {"
    CloseBracket n:_ -> syntaxError n "mismatched backets, unexpected }"
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
    rest'       -> do ts' <- mapM (mkField (d+1)) ts
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

-- |parse a module Macro.name
parseModuleNameQ :: ReadP ModuleName
parseModuleNameQ = parseQuoted parse <++ parse

parseFilePathQ :: ReadP FilePath
parseFilePathQ = parseTokenQ
  -- removed until normalise is no longer broken, was:
  --   liftM normalise parseTokenQ

betweenSpaces :: ReadP a -> ReadP a
betweenSpaces act = do skipSpaces
                       res <- act
                       skipSpaces
                       return res

parseBuildTool :: ReadP Dependency
parseBuildTool = do name <- parseBuildToolNameQ
                    ver <- betweenSpaces $
                           parseVersionRangeQ <++ return AnyVersion
                    return $ Dependency name ver

parseBuildToolNameQ :: ReadP PackageName
parseBuildToolNameQ = parseQuoted parseBuildToolName <++ parseBuildToolName

-- like parsePackageName but accepts symbols in components
parseBuildToolName :: ReadP PackageName
parseBuildToolName = do ns <- sepBy1 component (ReadP.char '-')
                        return (PackageName (intercalate "-" ns))
  where component = do
          cs <- munch1 (\c -> isAlphaNum c || c == '+' || c == '_')
          if all isDigit cs then pfail else return cs

-- pkg-config allows versions and other letters in package names,
-- eg "gtk+-2.0" is a valid pkg-config package _name_.
-- It then has a package version number like 2.10.13
parsePkgconfigDependency :: ReadP Dependency
parsePkgconfigDependency = do name <- munch1
                                      (\c -> isAlphaNum c || c `elem` "+-._")
                              ver <- betweenSpaces $
                                     parseVersionRangeQ <++ return AnyVersion
                              return $ Dependency (PackageName name) ver

parseVersionRangeQ :: ReadP VersionRange
parseVersionRangeQ = parseQuoted parse <++ parse

parseTestedWithQ :: ReadP (CompilerFlavor,VersionRange)
parseTestedWithQ = parseQuoted tw <++ tw
  where
    tw :: ReadP (CompilerFlavor,VersionRange)
    tw = do compiler <- parseCompilerFlavorCompat
            version <- betweenSpaces $ parse <++ return AnyVersion
            return (compiler,version)

parseCompilerFlavorCompat :: Parse.ReadP CompilerFlavor
parseCompilerFlavorCompat = do
  comp <- Parse.munch1 Char.isAlphaNum
  when (all Char.isDigit comp) Parse.pfail
  case lookup comp compilerMap of
    Just compiler -> return compiler
    Nothing       -> return (OtherCompiler comp)
  where
    compilerMap = [ (show compiler, compiler)
                  | compiler <- knownCompilerFlavors
                  , compiler /= YHC ]

    knownCompilerFlavors :: [CompilerFlavor]
    knownCompilerFlavors = [GHC, NHC, YHC, Hugs, HBC, Helium, JHC, LHC, UHC]


parseLicenseQ :: ReadP License
parseLicenseQ = parseQuoted parse <++ parse

parseLanguageQ :: ReadP Language
parseLanguageQ = parseQuoted parse <++ parse

parseExtensionQ :: ReadP Extension
parseExtensionQ = parseQuoted parse <++ parse

parseHaskellString :: ReadP String
parseHaskellString = readS_to_P reads

parseTokenQ :: ReadP String
parseTokenQ = parseHaskellString <++ munch1 (\x -> not (isSpace x) && x /= ',')

parseTokenQ' :: ReadP String
parseTokenQ' = parseHaskellString <++ munch1 (not . isSpace)

parseSepList :: ReadP b
             -> ReadP a -- ^The parser for the stuff between commas
             -> ReadP [a]
parseSepList sepr p = sepBy p separator
    where separator = betweenSpaces sepr

parseSpaceList :: ReadP a -- ^The parser for the stuff between commas
               -> ReadP [a]
parseSpaceList p = sepBy p skipSpaces

parseCommaList :: ReadP a -- ^The parser for the stuff between commas
               -> ReadP [a]
parseCommaList = parseSepList (ReadP.char ',')

parseOptCommaList :: ReadP a -- ^The parser for the stuff between commas
                  -> ReadP [a]
parseOptCommaList = parseSepList (optional (ReadP.char ','))

parseQuoted :: ReadP a -> ReadP a
parseQuoted = between (ReadP.char '"') (ReadP.char '"')

parseFreeText :: ReadP.ReadP String
parseFreeText = ReadP.munch (const True)

ident :: Parse.ReadP String
ident = Parse.munch1 (\c -> Char.isAlphaNum c || c == '_' || c == '-')

lowercase :: String -> String
lowercase = map Char.toLower


-- --------------------------------------------
-- ** Pretty printing

showFilePath :: FilePath -> Doc
showFilePath = showToken

showToken :: String -> Doc
showToken str
 | not (any dodgy str) &&
   not (null str)       = text str
 | otherwise            = text (show str)
  where dodgy c = isSpace c || c == ','

showTestedWith :: (CompilerFlavor,VersionRange) -> Doc
showTestedWith (compiler, version) = text (show compiler) <+> disp version

-- | Pretty-print free-format text, ensuring that it is vertically aligned,
-- and with blank lines replaced by dots for correct re-parsing.
showFreeText :: String -> Doc
showFreeText "" = empty
showFreeText ('\n' :r)  = text " " $+$ text "." $+$ showFreeText r
showFreeText s  = vcat [text (if null l then "." else l) | l <- lines_ s]

-- | 'lines_' breaks a string up into a list of strings at newline
-- characters.  The resulting strings do not contain newlines.
lines_                   :: String -> [String]
lines_ []                =  [""]
lines_ s                 =  let (l, s') = break (== '\n') s
                            in  l : case s' of
                                        []    -> []
                                        (_:s'') -> lines_ s''

class Text a where
  disp  :: a -> Disp.Doc
  parse :: Parse.ReadP a

display :: Text a => a -> String
display = Disp.renderStyle style . disp
  where style = Disp.Style {
          Disp.mode            = Disp.PageMode,
          Disp.lineLength      = 79,
          Disp.ribbonsPerLine  = 1.0
        }

simpleParse :: Text a => String -> Maybe a
simpleParse str = case [ p | (p, s) <- Parse.readP_to_S parse str
                       , all Char.isSpace s ] of
  []    -> Nothing
  (p:_) -> Just p

-- -----------------------------------------------------------------------------
-- Instances for types from the base package

instance Text Bool where
  disp  = Disp.text . show
  parse = Parse.choice [ (Parse.string "True" Parse.+++
                          Parse.string "true") >> return True
                       , (Parse.string "False" Parse.+++
                          Parse.string "false") >> return False ]

instance Text Version where
  disp (Version branch _tags)     -- Death to version tags!!
    = Disp.hcat (Disp.punctuate (Disp.char '.') (map Disp.int branch))

  parse = do
      branch <- Parse.sepBy1 digits (Parse.char '.')
      tags   <- Parse.many (Parse.char '-' >> Parse.munch1 Char.isAlphaNum)
      return (Version branch tags)  -- TODO FIXME: should we ignore the tags?
    where
      digits = do
        first <- Parse.satisfy Char.isDigit
        if first == '0'
          then return 0
          else do rest <- Parse.munch Char.isDigit
                  return (read (first : rest))


-- -----------------------------------------------------------------------------

instance Text InstalledPackageId where
  disp (InstalledPackageId str) = text str

  parse = InstalledPackageId `fmap` Parse.munch1 abi_char
   where abi_char c = Char.isAlphaNum c || c `elem` ":-_."

instance Text ModuleName where
  disp (ModuleName ms) =
    Disp.hcat (intersperse (Disp.char '.') (map Disp.text ms))

  parse = do
    ms <- Parse.sepBy1 component (Parse.char '.')
    return (ModuleName ms)

    where
      component = do
        c  <- Parse.satisfy Char.isUpper
        cs <- Parse.munch validModuleChar
        return (c:cs)

validModuleChar :: Char -> Bool
validModuleChar c = Char.isAlphaNum c || c == '_' || c == '\''

instance Text PackageName where
  disp (PackageName n) = Disp.text n
  parse = do
    ns <- Parse.sepBy1 component (Parse.char '-')
    return (PackageName (intercalate "-" ns))
    where
      component = do
        cs <- Parse.munch1 Char.isAlphaNum
        if all Char.isDigit cs then Parse.pfail else return cs
        -- each component must contain an alphabetic character, to avoid
        -- ambiguity in identifiers like foo-1 (the 1 is the version number).

instance Text PackageId where
  disp (PackageId n v) = case v of
    Version [] _ -> disp n -- if no version, don't show version.
    _            -> disp n <> Disp.char '-' <> disp v

  parse = do
    n <- parse
    v <- (Parse.char '-' >> parse) <++ return (Version [] [])
    return (PackageId n v)

instance Text VersionRange where
  disp = fst
       . foldVersionRange'                         -- precedence:
           (         Disp.text "-any"                           , 0 :: Int)
           (\v   -> (Disp.text "==" <> disp v                   , 0))
           (\v   -> (Disp.char '>'  <> disp v                   , 0))
           (\v   -> (Disp.char '<'  <> disp v                   , 0))
           (\v   -> (Disp.text ">=" <> disp v                   , 0))
           (\v   -> (Disp.text "<=" <> disp v                   , 0))
           (\v _ -> (Disp.text "==" <> dispWild v               , 0))
           (\(r1, p1) (r2, p2) -> (punct 2 p1 r1 <+> Disp.text "||" <+> punct 2 p2 r2 , 2))
           (\(r1, p1) (r2, p2) -> (punct 1 p1 r1 <+> Disp.text "&&" <+> punct 1 p2 r2 , 1))
           (\(r, p)   -> (Disp.parens r, p))

    where dispWild (Version b _) =
               Disp.hcat (Disp.punctuate (Disp.char '.') (map Disp.int b))
            <> Disp.text ".*"
          punct p p' | p < p'    = Disp.parens
                     | otherwise = id

  parse = expr
   where
        expr   = do Parse.skipSpaces
                    t <- term
                    Parse.skipSpaces
                    (do _  <- Parse.string "||"
                        Parse.skipSpaces
                        e <- expr
                        return (UnionVersionRanges t e)
                     +++
                     return t)
        term   = do f <- factor
                    Parse.skipSpaces
                    (do _  <- Parse.string "&&"
                        Parse.skipSpaces
                        t <- term
                        return (IntersectVersionRanges f t)
                     +++
                     return f)
        factor = Parse.choice $ parens expr
                              : parseAnyVersion
                              : parseWildcardRange
                              : map parseRangeOp rangeOps
        parseAnyVersion    = Parse.string "-any" >> return AnyVersion

        parseWildcardRange = do
          _ <- Parse.string "=="
          Parse.skipSpaces
          branch <- Parse.sepBy1 digits (Parse.char '.')
          _ <- Parse.char '.'
          _ <- Parse.char '*'
          return (WildcardVersion (Version branch []))

        parens p = Parse.between (Parse.char '(' >> Parse.skipSpaces)
                                 (Parse.char ')' >> Parse.skipSpaces)
                                 (do a <- p
                                     Parse.skipSpaces
                                     return (VersionRangeParens a))

        digits = do
          first <- Parse.satisfy Char.isDigit
          if first == '0'
            then return 0
            else do rest <- Parse.munch Char.isDigit
                    return (read (first : rest))

        parseRangeOp (s,f) = Parse.string s >> Parse.skipSpaces >> fmap f parse
        rangeOps = [ ("<",  EarlierVersion),
                     ("<=", orEarlierVersion),
                     (">",  LaterVersion),
                     (">=", orLaterVersion),
                     ("==", ThisVersion) ]

orLaterVersion :: Version -> VersionRange
orLaterVersion   v = UnionVersionRanges (ThisVersion v) (LaterVersion v)

orEarlierVersion :: Version -> VersionRange
orEarlierVersion v = UnionVersionRanges (ThisVersion v) (EarlierVersion v)

foldVersionRange :: a                         -- ^ @\"-any\"@ version
                 -> (Version -> a)            -- ^ @\"== v\"@
                 -> (Version -> a)            -- ^ @\"> v\"@
                 -> (Version -> a)            -- ^ @\"< v\"@
                 -> (a -> a -> a)             -- ^ @\"_ || _\"@ union
                 -> (a -> a -> a)             -- ^ @\"_ && _\"@ intersection
                 -> VersionRange -> a
foldVersionRange anyv this later earlier union intersect = fold
  where
    fold AnyVersion                     = anyv
    fold (ThisVersion v)                = this v
    fold (LaterVersion v)               = later v
    fold (EarlierVersion v)             = earlier v
    fold (WildcardVersion v)            = fold (wildcard v)
    fold (UnionVersionRanges v1 v2)     = union (fold v1) (fold v2)
    fold (IntersectVersionRanges v1 v2) = intersect (fold v1) (fold v2)
    fold (VersionRangeParens v)         = fold v

    wildcard v = IntersectVersionRanges
                   (orLaterVersion v)
                   (EarlierVersion (wildcardUpperBound v))

foldVersionRange' :: a                         -- ^ @\"-any\"@ version
                  -> (Version -> a)            -- ^ @\"== v\"@
                  -> (Version -> a)            -- ^ @\"> v\"@
                  -> (Version -> a)            -- ^ @\"< v\"@
                  -> (Version -> a)            -- ^ @\">= v\"@
                  -> (Version -> a)            -- ^ @\"<= v\"@
                  -> (Version -> Version -> a) -- ^ @\"== v.*\"@ wildcard. The
                                               -- function is passed the
                                               -- inclusive lower bound and the
                                               -- exclusive upper bounds of the
                                               -- range defined by the wildcard.
                  -> (a -> a -> a)             -- ^ @\"_ || _\"@ union
                  -> (a -> a -> a)             -- ^ @\"_ && _\"@ intersection
                  -> (a -> a)                  -- ^ @\"(_)\"@ parentheses
                  -> VersionRange -> a
foldVersionRange' anyv this later earlier orLater orEarlier
                  wildcard union intersect parens = fold
  where
    fold AnyVersion                     = anyv
    fold (ThisVersion v)                = this v
    fold (LaterVersion v)               = later v
    fold (EarlierVersion v)             = earlier v

    fold (UnionVersionRanges (ThisVersion    v)
                             (LaterVersion   v')) | v==v' = orLater v
    fold (UnionVersionRanges (LaterVersion   v)
                             (ThisVersion    v')) | v==v' = orLater v
    fold (UnionVersionRanges (ThisVersion    v)
                             (EarlierVersion v')) | v==v' = orEarlier v
    fold (UnionVersionRanges (EarlierVersion v)
                             (ThisVersion    v')) | v==v' = orEarlier v

    fold (WildcardVersion v)            = wildcard v (wildcardUpperBound v)
    fold (UnionVersionRanges v1 v2)     = union (fold v1) (fold v2)
    fold (IntersectVersionRanges v1 v2) = intersect (fold v1) (fold v2)
    fold (VersionRangeParens v)         = parens (fold v)

wildcardUpperBound :: Version -> Version
wildcardUpperBound (Version lowerBound ts) = Version upperBound ts
  where
    upperBound = init lowerBound ++ [last lowerBound + 1]

asVersionIntervals :: VersionRange -> [VersionInterval]
asVersionIntervals = versionIntervals . toVersionIntervals

newtype VersionIntervals = VersionIntervals [VersionInterval]
  deriving (Eq, Show)

-- | Inspect the list of version intervals.
--
versionIntervals :: VersionIntervals -> [VersionInterval]
versionIntervals (VersionIntervals is) = is

type VersionInterval = (LowerBound, UpperBound)
data LowerBound =                LowerBound Version !Bound deriving (Eq, Show)
data UpperBound = NoUpperBound | UpperBound Version !Bound deriving (Eq, Show)
data Bound      = ExclusiveBound | InclusiveBound          deriving (Eq, Show)

minLowerBound :: LowerBound
minLowerBound = LowerBound (Version [0] []) InclusiveBound

isVersion0 :: Version -> Bool
isVersion0 (Version [0] _) = True
isVersion0 _               = False

instance Ord LowerBound where
  LowerBound ver bound <= LowerBound ver' bound' = case compare ver ver' of
    LT -> True
    EQ -> not (bound == ExclusiveBound && bound' == InclusiveBound)
    GT -> False

instance Ord UpperBound where
  _            <= NoUpperBound   = True
  NoUpperBound <= UpperBound _ _ = False
  UpperBound ver bound <= UpperBound ver' bound' = case compare ver ver' of
    LT -> True
    EQ -> not (bound == InclusiveBound && bound' == ExclusiveBound)
    GT -> False

invariant :: VersionIntervals -> Bool
invariant (VersionIntervals intervals) = all validInterval intervals
                                      && all doesNotTouch' adjacentIntervals
  where
    doesNotTouch' :: (VersionInterval, VersionInterval) -> Bool
    doesNotTouch' ((_,u), (l',_)) = doesNotTouch u l'

    adjacentIntervals :: [(VersionInterval, VersionInterval)]
    adjacentIntervals
      | null intervals = []
      | otherwise      = zip intervals (tail intervals)

checkInvariant :: VersionIntervals -> VersionIntervals
checkInvariant is = assert (invariant is) is

validVersion :: Version -> Bool
validVersion (Version [] _) = False
validVersion (Version vs _) = all (>=0) vs

validInterval :: (LowerBound, UpperBound) -> Bool
validInterval i@(l, u) = validLower l && validUpper u && nonEmpty i
  where
    validLower (LowerBound v _) = validVersion v
    validUpper NoUpperBound     = True
    validUpper (UpperBound v _) = validVersion v

-- Check an interval is non-empty
--
nonEmpty :: VersionInterval -> Bool
nonEmpty (_,               NoUpperBound   ) = True
nonEmpty (LowerBound l lb, UpperBound u ub) =
  (l < u) || (l == u && lb == InclusiveBound && ub == InclusiveBound)

-- Check an upper bound does not intersect, or even touch a lower bound:
--
--   ---|      or  ---)     but not  ---]     or  ---)     or  ---]
--       |---         (---              (---         [---         [---
--
doesNotTouch :: UpperBound -> LowerBound -> Bool
doesNotTouch NoUpperBound _ = False
doesNotTouch (UpperBound u ub) (LowerBound l lb) =
      u <  l
  || (u == l && ub == ExclusiveBound && lb == ExclusiveBound)

-- | Check an upper bound does not intersect a lower bound:
--
--   ---|      or  ---)     or  ---]     or  ---)     but not  ---]
--       |---         (---         (---         [---              [---
--
doesNotIntersect :: UpperBound -> LowerBound -> Bool
doesNotIntersect NoUpperBound _ = False
doesNotIntersect (UpperBound u ub) (LowerBound l lb) =
      u <  l
  || (u == l && not (ub == InclusiveBound && lb == InclusiveBound))

toVersionIntervals :: VersionRange -> VersionIntervals
toVersionIntervals = foldVersionRange
  (         chkIvl (minLowerBound,               NoUpperBound))
  (\v    -> chkIvl (LowerBound v InclusiveBound, UpperBound v InclusiveBound))
  (\v    -> chkIvl (LowerBound v ExclusiveBound, NoUpperBound))
  (\v    -> if isVersion0 v then VersionIntervals [] else
            chkIvl (minLowerBound,               UpperBound v ExclusiveBound))
  unionVersionIntervals
  intersectVersionIntervals
  where
    chkIvl interval = checkInvariant (VersionIntervals [interval])

unionVersionIntervals :: VersionIntervals -> VersionIntervals
                      -> VersionIntervals
unionVersionIntervals (VersionIntervals is0) (VersionIntervals is'0) =
  checkInvariant (VersionIntervals (union is0 is'0))
  where
    union is []  = is
    union [] is' = is'
    union (i:is) (i':is') = case unionInterval i i' of
      Left  Nothing    -> i  : union      is  (i' :is')
      Left  (Just i'') ->      union      is  (i'':is')
      Right Nothing    -> i' : union (i  :is)      is'
      Right (Just i'') ->      union (i'':is)      is'

unionInterval :: VersionInterval -> VersionInterval
              -> Either (Maybe VersionInterval) (Maybe VersionInterval)
unionInterval (lower , upper ) (lower', upper')

  -- Non-intersecting intervals with the left interval ending first
  | upper `doesNotTouch` lower' = Left Nothing

  -- Non-intersecting intervals with the right interval first
  | upper' `doesNotTouch` lower = Right Nothing

  -- Complete or partial overlap, with the left interval ending first
  | upper <= upper' = lowerBound `seq`
                      Left (Just (lowerBound, upper'))

  -- Complete or partial overlap, with the left interval ending first
  | otherwise = lowerBound `seq`
                Right (Just (lowerBound, upper))
  where
    lowerBound = min lower lower'

intersectVersionIntervals :: VersionIntervals -> VersionIntervals
                          -> VersionIntervals
intersectVersionIntervals (VersionIntervals is0) (VersionIntervals is'0) =
  checkInvariant (VersionIntervals (intersect is0 is'0))
  where
    intersect _  [] = []
    intersect [] _  = []
    intersect (i:is) (i':is') = case intersectInterval i i' of
      Left  Nothing    ->       intersect is (i':is')
      Left  (Just i'') -> i'' : intersect is (i':is')
      Right Nothing    ->       intersect (i:is) is'
      Right (Just i'') -> i'' : intersect (i:is) is'

intersectInterval :: VersionInterval -> VersionInterval
                  -> Either (Maybe VersionInterval) (Maybe VersionInterval)
intersectInterval (lower , upper ) (lower', upper')

  -- Non-intersecting intervals with the left interval ending first
  | upper `doesNotIntersect` lower' = Left Nothing

  -- Non-intersecting intervals with the right interval first
  | upper' `doesNotIntersect` lower = Right Nothing

  -- Complete or partial overlap, with the left interval ending first
  | upper <= upper' = lowerBound `seq`
                      Left (Just (lowerBound, upper))

  -- Complete or partial overlap, with the right interval ending first
  | otherwise = lowerBound `seq`
                Right (Just (lowerBound, upper'))
  where
    lowerBound = max lower lower'


instance Text Dependency where
  disp (Dependency name ver) =
    disp name <+> disp ver

  parse = do name <- parse
             Parse.skipSpaces
             ver <- parse <++ return AnyVersion
             Parse.skipSpaces
             return (Dependency name ver)

instance Text License where
  disp (GPL  version)         = Disp.text "GPL"  <> dispOptVersion version
  disp (LGPL version)         = Disp.text "LGPL" <> dispOptVersion version
  disp (AGPL version)         = Disp.text "AGPL" <> dispOptVersion version
  disp (Apache version)       = Disp.text "Apache" <> dispOptVersion version
  disp (UnknownLicense other) = Disp.text other
  disp other                  = Disp.text (show other)

  parse = do
    name    <- Parse.munch1 (\c -> Char.isAlphaNum c && c /= '-')
    version <- Parse.option Nothing (Parse.char '-' >> fmap Just parse)
    return $! case (name, version :: Maybe Version) of
      ("GPL",               _      ) -> GPL  version
      ("LGPL",              _      ) -> LGPL version
      ("AGPL",              _      ) -> AGPL version
      ("BSD3",              Nothing) -> BSD3
      ("BSD4",              Nothing) -> BSD4
      ("MIT",               Nothing) -> MIT
      ("Apache",            _      ) -> Apache version
      ("PublicDomain",      Nothing) -> PublicDomain
      ("AllRightsReserved", Nothing) -> AllRightsReserved
      ("OtherLicense",      Nothing) -> OtherLicense
      _                              -> UnknownLicense $ name
                                     ++ maybe "" (('-':) . display) version

dispOptVersion :: Maybe Version -> Disp.Doc
dispOptVersion Nothing  = Disp.empty
dispOptVersion (Just v) = Disp.char '-' <> disp v

instance Text CompilerFlavor where
  disp (OtherCompiler name) = Disp.text name
  disp (HaskellSuite name)  = Disp.text name
  disp NHC                  = Disp.text "nhc98"
  disp other                = Disp.text (lowercase (show other))

  parse = do
    comp <- Parse.munch1 Char.isAlphaNum
    when (all Char.isDigit comp) Parse.pfail
    return (classifyCompilerFlavor comp)

classifyCompilerFlavor :: String -> CompilerFlavor
classifyCompilerFlavor s =
  fromMaybe (OtherCompiler s) $ lookup (lowercase s) compilerMap
  where
    compilerMap = [ (display compiler, compiler)
                  | compiler <- knownCompilerFlavors ]

knownCompilerFlavors :: [CompilerFlavor]
knownCompilerFlavors = [GHC, NHC, YHC, Hugs, HBC, Helium, JHC, LHC, UHC]

instance Text Language where
  disp (UnknownLanguage other) = Disp.text other
  disp other                   = Disp.text (show other)

  parse = do
    lang <- Parse.munch1 Char.isAlphaNum
    return (classifyLanguage lang)

classifyLanguage :: String -> Language
classifyLanguage = \str -> case lookup str langTable of
    Just lang -> lang
    Nothing   -> UnknownLanguage str
  where
    langTable = [ (show lang, lang)
                | lang <- knownLanguages ]

knownLanguages :: [Language]
knownLanguages = [Haskell98, Haskell2010]

instance Text Extension where
  disp (UnknownExtension other) = Disp.text other
  disp (EnableExtension ke)     = Disp.text (show ke)
  disp (DisableExtension ke)    = Disp.text ("No" ++ show ke)

  parse = do
    extension <- Parse.munch1 Char.isAlphaNum
    return (classifyExtension extension)

instance Text KnownExtension where
  disp ke = Disp.text (show ke)

  parse = do
    extension <- Parse.munch1 Char.isAlphaNum
    case classifyKnownExtension extension of
        Just ke ->
            return ke
        Nothing ->
            Fail.fail ("Can't parse " ++ show extension ++ " as KnownExtension")

classifyExtension :: String -> Extension
classifyExtension string
  = case classifyKnownExtension string of
    Just ext -> EnableExtension ext
    Nothing ->
        case string of
        'N':'o':string' ->
            case classifyKnownExtension string' of
            Just ext -> DisableExtension ext
            Nothing -> UnknownExtension string
        _ -> UnknownExtension string

-- | 'read' for 'KnownExtension's is really really slow so for the Text
-- instance
-- what we do is make a simple table indexed off the first letter in the
-- extension name. The extension names actually cover the range @'A'-'Z'@
-- pretty densely and the biggest bucket is 7 so it's not too bad. We just do
-- a linear search within each bucket.
--
-- This gives an order of magnitude improvement in parsing speed, and it'll
-- also allow us to do case insensitive matches in future if we prefer.
--
classifyKnownExtension :: String -> Maybe KnownExtension
classifyKnownExtension "" = Nothing
classifyKnownExtension string@(c : _)
  | inRange (bounds knownExtensionTable) c
  = lookup string (knownExtensionTable ! c)
  | otherwise = Nothing

knownExtensionTable :: Array Char [(String, KnownExtension)]
knownExtensionTable =
  accumArray (flip (:)) [] ('A', 'Z')
    [ (head str, (str, extension))
    | extension <- [toEnum 0 ..]
    , let str = show extension ]

instance Text BuildType where
  disp (UnknownBuildType other) = Disp.text other
  disp other                    = Disp.text (show other)

  parse = do
    name <- Parse.munch1 Char.isAlphaNum
    return $ case name of
      "Simple"    -> Simple
      "Configure" -> Configure
      "Custom"    -> Custom
      "Make"      -> Make
      _           -> UnknownBuildType name

instance Text BenchmarkType where
  disp (BenchmarkTypeExe ver)          = text "exitcode-stdio-" <> disp ver
  disp (BenchmarkTypeUnknown name ver) = text name <> Disp.char '-' <> disp ver

  parse = stdParse $ \ver name -> case name of
    "exitcode-stdio" -> BenchmarkTypeExe ver
    _                -> BenchmarkTypeUnknown name ver

stdParse :: Text ver => (ver -> String -> res) -> Parse.ReadP res
stdParse f = do
  cs   <- Parse.sepBy1 component (Parse.char '-')
  _    <- Parse.char '-'
  ver  <- parse
  let name = intercalate "-" cs
  return $! f ver (lowercase name)
  where
    component = do
      cs <- Parse.munch1 Char.isAlphaNum
      if all Char.isDigit cs then Parse.pfail else return cs
      -- each component must contain an alphabetic character, to avoid
      -- ambiguity in identifiers like foo-1 (the 1 is the version number).

instance Text RepoKind where
  disp RepoHead                = Disp.text "head"
  disp RepoThis                = Disp.text "this"
  disp (RepoKindUnknown other) = Disp.text other

  parse = do
    name <- ident
    return $ case lowercase name of
      "head" -> RepoHead
      "this" -> RepoThis
      _      -> RepoKindUnknown name

instance Text RepoType where
  disp (OtherRepoType other) = Disp.text other
  disp other                 = Disp.text (lowercase (show other))
  parse = fmap classifyRepoType ident

classifyRepoType :: String -> RepoType
classifyRepoType s =
  fromMaybe (OtherRepoType s) $ lookup (lowercase s) repoTypeMap
  where
    repoTypeMap = [ (name, repoType')
                  | repoType' <- knownRepoTypes
                  , name <- display repoType' : repoTypeAliases repoType' ]

knownRepoTypes :: [RepoType]
knownRepoTypes = [Darcs, Git, SVN, CVS
                 ,Mercurial, GnuArch, Bazaar, Monotone]

repoTypeAliases :: RepoType -> [String]
repoTypeAliases Bazaar    = ["bzr"]
repoTypeAliases Mercurial = ["hg"]
repoTypeAliases GnuArch   = ["arch"]
repoTypeAliases _         = []

instance Text Arch where
  disp (OtherArch name) = Disp.text name
  disp other            = Disp.text (lowercase (show other))

  parse = fmap classifyArch ident

classifyArch :: String -> Arch
classifyArch s =
  fromMaybe (OtherArch s) $ lookup (lowercase s) archMap
  where
    archMap = [ (display arch, arch)
              | arch <- knownArches ]

knownArches :: [Arch]
knownArches = [I386, X86_64, PPC, PPC64, Sparc
              ,Arm, Mips, SH
              ,IA64, S390
              ,Alpha, Hppa, Rs6000
              ,M68k, Vax]

instance Text OS where
  disp (OtherOS name) = Disp.text name
  disp other          = Disp.text (lowercase (show other))

  parse = fmap classifyOS ident

classifyOS :: String -> OS
classifyOS s =
  fromMaybe (OtherOS s) $ lookup (lowercase s) osMap
  where
    osMap = [ (name, os)
            | os <- knownOSs
            , name <- display os : osAliases os ]

knownOSs :: [OS]
knownOSs = [Linux, Windows, OSX
           ,FreeBSD, OpenBSD, NetBSD
           ,Solaris, AIX, HPUX, IRIX
           ,HaLVM
           ,IOS]

osAliases :: OS -> [String]
osAliases Windows = ["mingw32", "win32"]
osAliases _       = []


-- -----------------------------------------------------------------------------
-- The PackageDescription type

pkgDescrFieldDescrs :: [FieldDescr PackageDescription]
pkgDescrFieldDescrs =
    [ simpleField "name"
           disp                   parse
           (pkgName.package)      (\name pkg -> pkg{package=(package pkg){pkgName=name}})
 , simpleField "version"
           disp                   parse
           (pkgVersion.package)   (\ver pkg -> pkg{package=(package pkg){pkgVersion=ver}})
 , simpleField "cabal-version"
           (either disp disp)     (liftM Left parse +++ liftM Right parse)
           specVersionRaw         (\v pkg -> pkg{specVersionRaw=v})
 , simpleField "build-type"
           (maybe empty disp)     (fmap Just parse)
           buildType              (\t pkg -> pkg{buildType=t})
 , simpleField "license"
           disp                   parseLicenseQ
           license                (\l pkg -> pkg{license=l})
 , simpleField "license-file"
           showFilePath           parseFilePathQ
           licenseFile            (\l pkg -> pkg{licenseFile=l})
 , simpleField "copyright"
           showFreeText           parseFreeText
           copyright              (\val pkg -> pkg{copyright=val})
 , simpleField "maintainer"
           showFreeText           parseFreeText
           maintainer             (\val pkg -> pkg{maintainer=val})
 , commaListField  "build-depends"
           disp                   parse
           buildDepends           (\xs    pkg -> pkg{buildDepends=xs})
 , simpleField "stability"
           showFreeText           parseFreeText
           stability              (\val pkg -> pkg{stability=val})
 , simpleField "homepage"
           showFreeText           parseFreeText
           homepage               (\val pkg -> pkg{homepage=val})
 , simpleField "package-url"
           showFreeText           parseFreeText
           pkgUrl                 (\val pkg -> pkg{pkgUrl=val})
 , simpleField "bug-reports"
           showFreeText           parseFreeText
           bugReports             (\val pkg -> pkg{bugReports=val})
 , simpleField "synopsis"
           showFreeText           parseFreeText
           synopsis               (\val pkg -> pkg{synopsis=val})
 , simpleField "description"
           showFreeText           parseFreeText
           description            (\val pkg -> pkg{description=val})
 , simpleField "category"
           showFreeText           parseFreeText
           category               (\val pkg -> pkg{category=val})
 , simpleField "author"
           showFreeText           parseFreeText
           author                 (\val pkg -> pkg{author=val})
 , listField "tested-with"
           showTestedWith         parseTestedWithQ
           testedWith             (\val pkg -> pkg{testedWith=val})
 , listField "data-files"
           showFilePath           parseFilePathQ
           dataFiles              (\val pkg -> pkg{dataFiles=val})
 , simpleField "data-dir"
           showFilePath           parseFilePathQ
           dataDir                (\val pkg -> pkg{dataDir=val})
 , listField "extra-source-files"
           showFilePath    parseFilePathQ
           extraSrcFiles          (\val pkg -> pkg{extraSrcFiles=val})
 , listField "extra-tmp-files"
           showFilePath       parseFilePathQ
           extraTmpFiles          (\val pkg -> pkg{extraTmpFiles=val})
 , listField "extra-doc-files"
           showFilePath    parseFilePathQ
           extraDocFiles          (\val pkg -> pkg{extraDocFiles=val})
 ]

-- | Store any fields beginning with "x-" in the customFields field of
--   a PackageDescription.  All other fields will generate a warning.
storeXFieldsPD :: UnrecFieldParser PackageDescription
storeXFieldsPD (f@('x':'-':_),val) pkg =
  Just pkg{ customFieldsPD =
               customFieldsPD pkg ++ [(f,val)]}
storeXFieldsPD _ _ = Nothing

-- ---------------------------------------------------------------------------
-- The Library type

libFieldDescrs :: [FieldDescr Library]
libFieldDescrs =
  [ listField "exposed-modules" disp parseModuleNameQ
      exposedModules (\mods lib -> lib{exposedModules=mods})

  , boolField "exposed"
      libExposed     (\val lib -> lib{libExposed=val})
  ] ++ map biToLib binfoFieldDescrs
  where biToLib = liftField libBuildInfo (\bi lib -> lib{libBuildInfo=bi})

storeXFieldsLib :: UnrecFieldParser Library
storeXFieldsLib (f@('x':'-':_), val) l@(Library { libBuildInfo = bi }) =
    Just $ l {libBuildInfo =
                 bi{ customFieldsBI = customFieldsBI bi ++ [(f,val)]}}
storeXFieldsLib _ _ = Nothing

-- ---------------------------------------------------------------------------
-- The Executable type


executableFieldDescrs :: [FieldDescr Executable]
executableFieldDescrs =
  [ -- note ordering: configuration must come first, for
    -- showPackageDescription.
    simpleField "executable"
                           showToken          parseTokenQ
                           exeName            (\xs    exe -> exe{exeName=xs})
  , simpleField "main-is"
                           showFilePath       parseFilePathQ
                           modulePath         (\xs    exe -> exe{modulePath=xs})
  ]
  ++ map biToExe binfoFieldDescrs
  where biToExe = liftField buildInfo (\bi exe -> exe{buildInfo=bi})

storeXFieldsExe :: UnrecFieldParser Executable
storeXFieldsExe (f@('x':'-':_), val) e@(Executable { buildInfo = bi }) =
    Just $ e {buildInfo = bi{ customFieldsBI = (f,val):customFieldsBI bi}}
storeXFieldsExe _ _ = Nothing

-- ---------------------------------------------------------------------------
-- The TestSuite type

-- | An intermediate type just used for parsing the test-suite stanza.
-- After validation it is converted into the proper 'TestSuite' type.
data TestSuiteStanza = TestSuiteStanza {
       testStanzaTestType   :: Maybe TestType,
       testStanzaMainIs     :: Maybe FilePath,
       testStanzaTestModule :: Maybe ModuleName,
       testStanzaBuildInfo  :: BuildInfo
     }

emptyTestStanza :: TestSuiteStanza
emptyTestStanza = TestSuiteStanza Nothing Nothing Nothing mempty

testSuiteFieldDescrs :: [FieldDescr TestSuiteStanza]
testSuiteFieldDescrs =
    [ simpleField "type"
        (maybe empty disp)    (fmap Just parse)
        testStanzaTestType    (\x suite -> suite { testStanzaTestType = x })
    , simpleField "main-is"
        (maybe empty showFilePath)  (fmap Just parseFilePathQ)
        testStanzaMainIs      (\x suite -> suite { testStanzaMainIs = x })
    , simpleField "test-module"
        (maybe empty disp)    (fmap Just parseModuleNameQ)
        testStanzaTestModule  (\x suite -> suite { testStanzaTestModule = x })
    ]
    ++ map biToTest binfoFieldDescrs
  where
    biToTest = liftField testStanzaBuildInfo
                         (\bi suite -> suite { testStanzaBuildInfo = bi })

storeXFieldsTest :: UnrecFieldParser TestSuiteStanza
storeXFieldsTest (f@('x':'-':_), val) t@(TestSuiteStanza { testStanzaBuildInfo = bi }) =
    Just $ t {testStanzaBuildInfo = bi{ customFieldsBI = (f,val):customFieldsBI bi}}
storeXFieldsTest _ _ = Nothing

validateTestSuite :: LineNo -> TestSuiteStanza -> ParseResult TestSuite
validateTestSuite line stanza =
    case testStanzaTestType stanza of
      Nothing -> return $
        emptyTestSuite { testBuildInfo = testStanzaBuildInfo stanza }

      Just tt@(TestTypeUnknown _ _) ->
        return emptyTestSuite {
          testInterface = TestSuiteUnsupported tt,
          testBuildInfo = testStanzaBuildInfo stanza
        }

      Just tt | tt `notElem` knownTestTypes ->
        return emptyTestSuite {
          testInterface = TestSuiteUnsupported tt,
          testBuildInfo = testStanzaBuildInfo stanza
        }

      Just tt@(TestTypeExe ver) ->
        case testStanzaMainIs stanza of
          Nothing   -> syntaxError line (missingField "main-is" tt)
          Just file -> do
            when (isJust (testStanzaTestModule stanza)) $
              warning (extraField "test-module" tt)
            return emptyTestSuite {
              testInterface = TestSuiteExeV10 ver file,
              testBuildInfo = testStanzaBuildInfo stanza
            }

      Just tt@(TestTypeLib ver) ->
        case testStanzaTestModule stanza of
          Nothing      -> syntaxError line (missingField "test-module" tt)
          Just module_ -> do
            when (isJust (testStanzaMainIs stanza)) $
              warning (extraField "main-is" tt)
            return emptyTestSuite {
              testInterface = TestSuiteLibV09 ver module_,
              testBuildInfo = testStanzaBuildInfo stanza
            }

  where
    missingField name tt = "The '" ++ name ++ "' field is required for the "
                        ++ display tt ++ " test suite type."

    extraField   name tt = "The '" ++ name ++ "' field is not used for the '"
                        ++ display tt ++ "' test suite type."

instance Text TestType where
  disp (TestTypeExe ver)          = text "exitcode-stdio-" <> disp ver
  disp (TestTypeLib ver)          = text "detailed-"       <> disp ver
  disp (TestTypeUnknown name ver) = text name <> Disp.char '-' <> disp ver

  parse = stdParse $ \ver name -> case name of
    "exitcode-stdio" -> TestTypeExe ver
    "detailed"       -> TestTypeLib ver
    _                -> TestTypeUnknown name ver

knownTestTypes :: [TestType]
knownTestTypes = [ TestTypeExe (Version [1,0] [])
                 , TestTypeLib (Version [0,9] []) ]

-- ---------------------------------------------------------------------------
-- The Benchmark type

-- | An intermediate type just used for parsing the benchmark stanza.
-- After validation it is converted into the proper 'Benchmark' type.
data BenchmarkStanza = BenchmarkStanza {
       benchmarkStanzaBenchmarkType   :: Maybe BenchmarkType,
       benchmarkStanzaMainIs          :: Maybe FilePath,
       benchmarkStanzaBenchmarkModule :: Maybe ModuleName,
       benchmarkStanzaBuildInfo       :: BuildInfo
     }

emptyBenchmarkStanza :: BenchmarkStanza
emptyBenchmarkStanza = BenchmarkStanza Nothing Nothing Nothing mempty

benchmarkFieldDescrs :: [FieldDescr BenchmarkStanza]
benchmarkFieldDescrs =
    [ simpleField "type"
        (maybe empty disp)    (fmap Just parse)
        benchmarkStanzaBenchmarkType
        (\x suite -> suite { benchmarkStanzaBenchmarkType = x })
    , simpleField "main-is"
        (maybe empty showFilePath)  (fmap Just parseFilePathQ)
        benchmarkStanzaMainIs
        (\x suite -> suite { benchmarkStanzaMainIs = x })
    ]
    ++ map biToBenchmark binfoFieldDescrs
  where
    biToBenchmark = liftField benchmarkStanzaBuildInfo
                    (\bi suite -> suite { benchmarkStanzaBuildInfo = bi })

storeXFieldsBenchmark :: UnrecFieldParser BenchmarkStanza
storeXFieldsBenchmark (f@('x':'-':_), val)
    t@(BenchmarkStanza { benchmarkStanzaBuildInfo = bi }) =
        Just $ t {benchmarkStanzaBuildInfo =
                       bi{ customFieldsBI = (f,val):customFieldsBI bi}}
storeXFieldsBenchmark _ _ = Nothing

validateBenchmark :: LineNo -> BenchmarkStanza -> ParseResult Benchmark
validateBenchmark line stanza =
    case benchmarkStanzaBenchmarkType stanza of
      Nothing -> return $
        emptyBenchmark { benchmarkBuildInfo = benchmarkStanzaBuildInfo stanza }

      Just tt@(BenchmarkTypeUnknown _ _) ->
        return emptyBenchmark {
          benchmarkInterface = BenchmarkUnsupported tt,
          benchmarkBuildInfo = benchmarkStanzaBuildInfo stanza
        }

      Just tt | tt `notElem` knownBenchmarkTypes ->
        return emptyBenchmark {
          benchmarkInterface = BenchmarkUnsupported tt,
          benchmarkBuildInfo = benchmarkStanzaBuildInfo stanza
        }

      Just tt@(BenchmarkTypeExe ver) ->
        case benchmarkStanzaMainIs stanza of
          Nothing   -> syntaxError line (missingField "main-is" tt)
          Just file -> do
            when (isJust (benchmarkStanzaBenchmarkModule stanza)) $
              warning (extraField "benchmark-module" tt)
            return emptyBenchmark {
              benchmarkInterface = BenchmarkExeV10 ver file,
              benchmarkBuildInfo = benchmarkStanzaBuildInfo stanza
            }

  where
    missingField name tt = "The '" ++ name ++ "' field is required for the "
                        ++ display tt ++ " benchmark type."

    extraField   name tt = "The '" ++ name ++ "' field is not used for the '"
                        ++ display tt ++ "' benchmark type."

knownBenchmarkTypes :: [BenchmarkType]
knownBenchmarkTypes = [ BenchmarkTypeExe (Version [1,0] []) ]


-- ---------------------------------------------------------------------------
-- The BuildInfo type


binfoFieldDescrs :: [FieldDescr BuildInfo]
binfoFieldDescrs =
 [ boolField "buildable"
           buildable          (\val binfo -> binfo{buildable=val})
 , commaListField  "build-tools"
           disp               parseBuildTool
           buildTools         (\xs  binfo -> binfo{buildTools=xs})
 , spaceListField "cpp-options"
           showToken          parseTokenQ'
           cppOptions          (\val binfo -> binfo{cppOptions=val})
 , spaceListField "cc-options"
           showToken          parseTokenQ'
           ccOptions          (\val binfo -> binfo{ccOptions=val})
 , spaceListField "ld-options"
           showToken          parseTokenQ'
           ldOptions          (\val binfo -> binfo{ldOptions=val})
 , commaListField  "pkgconfig-depends"
           disp               parsePkgconfigDependency
           pkgconfigDepends   (\xs  binfo -> binfo{pkgconfigDepends=xs})
 , listField "frameworks"
           showToken          parseTokenQ
           frameworks         (\val binfo -> binfo{frameworks=val})
 , listField   "c-sources"
           showFilePath       parseFilePathQ
           cSources           (\paths binfo -> binfo{cSources=paths})

 , simpleField "default-language"
           (maybe empty disp) (option Nothing (fmap Just parseLanguageQ))
           defaultLanguage    (\lang  binfo -> binfo{defaultLanguage=lang})
 , listField   "other-languages"
           disp               parseLanguageQ
           otherLanguages     (\langs binfo -> binfo{otherLanguages=langs})
 , listField   "default-extensions"
           disp               parseExtensionQ
           defaultExtensions  (\exts  binfo -> binfo{defaultExtensions=exts})
 , listField   "other-extensions"
           disp               parseExtensionQ
           otherExtensions    (\exts  binfo -> binfo{otherExtensions=exts})
 , listField   "extensions"
           disp               parseExtensionQ
           oldExtensions      (\exts  binfo -> binfo{oldExtensions=exts})

 , listField   "extra-libraries"
           showToken          parseTokenQ
           extraLibs          (\xs    binfo -> binfo{extraLibs=xs})
 , listField   "extra-lib-dirs"
           showFilePath       parseFilePathQ
           extraLibDirs       (\xs    binfo -> binfo{extraLibDirs=xs})
 , listField   "includes"
           showFilePath       parseFilePathQ
           includes           (\paths binfo -> binfo{includes=paths})
 , listField   "install-includes"
           showFilePath       parseFilePathQ
           installIncludes    (\paths binfo -> binfo{installIncludes=paths})
 , listField   "include-dirs"
           showFilePath       parseFilePathQ
           includeDirs        (\paths binfo -> binfo{includeDirs=paths})
 , listField   "hs-source-dirs"
           showFilePath       parseFilePathQ
           hsSourceDirs       (\paths binfo -> binfo{hsSourceDirs=paths})
 , listField   "other-modules"
           disp               parseModuleNameQ
           otherModules       (\val binfo -> binfo{otherModules=val})
 , listField   "ghc-prof-options"
           text               parseTokenQ
           ghcProfOptions        (\val binfo -> binfo{ghcProfOptions=val})
 , listField   "ghc-shared-options"
           text               parseTokenQ
           ghcSharedOptions      (\val binfo -> binfo{ghcSharedOptions=val})
 , optsField   "ghc-options"  GHC
           options            (\path  binfo -> binfo{options=path})
 , optsField   "hugs-options" Hugs
           options            (\path  binfo -> binfo{options=path})
 , optsField   "nhc98-options"  NHC
           options            (\path  binfo -> binfo{options=path})
 , optsField   "jhc-options"  JHC
           options            (\path  binfo -> binfo{options=path})
 ]

------------------------------------------------------------------------------

flagFieldDescrs :: [FieldDescr Flag]
flagFieldDescrs =
    [ simpleField "description"
        showFreeText     parseFreeText
        flagDescription  (\val fl -> fl{ flagDescription = val })
    , boolField "default"
        flagDefault      (\val fl -> fl{ flagDefault = val })
    , boolField "manual"
        flagManual       (\val fl -> fl{ flagManual = val })
    ]

------------------------------------------------------------------------------

sourceRepoFieldDescrs :: [FieldDescr SourceRepo]
sourceRepoFieldDescrs =
    [ simpleField "type"
        (maybe empty disp)         (fmap Just parse)
        repoType                   (\val repo -> repo { repoType = val })
    , simpleField "location"
        (maybe empty showFreeText) (fmap Just parseFreeText)
        repoLocation               (\val repo -> repo { repoLocation = val })
    , simpleField "module"
        (maybe empty showToken)    (fmap Just parseTokenQ)
        repoModule                 (\val repo -> repo { repoModule = val })
    , simpleField "branch"
        (maybe empty showToken)    (fmap Just parseTokenQ)
        repoBranch                 (\val repo -> repo { repoBranch = val })
    , simpleField "tag"
        (maybe empty showToken)    (fmap Just parseTokenQ)
        repoTag                    (\val repo -> repo { repoTag = val })
    , simpleField "subdir"
        (maybe empty showFilePath) (fmap Just parseFilePathQ)
        repoSubdir                 (\val repo -> repo { repoSubdir = val })
    ]

-- ---------------------------------------------------------------
-- Parsing


mapSimpleFields :: (Field -> ParseResult Field) -> [Field]
                -> ParseResult [Field]
mapSimpleFields f = mapM walk
  where
    walk fld@F{} = f fld
    walk (IfBlock l c fs1 fs2) = do
      fs1' <- mapM walk fs1
      fs2' <- mapM walk fs2
      return (IfBlock l c fs1' fs2')
    walk (Section ln n l fs1) = do
      fs1' <-  mapM walk fs1
      return (Section ln n l fs1')

-- prop_isMapM fs = mapSimpleFields return fs == return fs


-- names of fields that represents dependencies, thus consrca
constraintFieldNames :: [String]
constraintFieldNames = ["build-depends"]

-- Possible refactoring would be to have modifiers be explicit about what
-- they add and define an accessor that specifies what the dependencies
-- are.  This way we would completely reuse the parsing knowledge from the
-- field descriptor.
parseConstraint :: Field -> ParseResult [Dependency]
parseConstraint (F l n v)
    | n == "build-depends" = runP l n (parseCommaList parse) v
parseConstraint f = userBug $ "Constraint was expected (got: " ++ show f ++ ")"

{-
headerFieldNames :: [String]
headerFieldNames = filter (\n -> not (n `elem` constraintFieldNames))
                 . map fieldName $ pkgDescrFieldDescrs
-}

libFieldNames :: [String]
libFieldNames = map fieldName libFieldDescrs
                ++ buildInfoNames ++ constraintFieldNames

-- exeFieldNames :: [String]
-- exeFieldNames = map fieldName executableFieldDescrs
--                 ++ buildInfoNames

buildInfoNames :: [String]
buildInfoNames = map fieldName binfoFieldDescrs
                ++ map fst deprecatedFieldsBuildInfo

-- A minimal implementation of the StateT monad transformer to avoid depending
-- on the 'mtl' package.
newtype StT s m a = StT { runStT :: s -> m (a,s) }

instance Functor f => Functor (StT s f) where
    fmap g (StT f) = StT $ fmap (\(x, s) -> (g x, s)) . f

instance (Monad m, Functor m) => Applicative (StT s m) where
    pure  = return
    (<*>) = ap

instance Monad m => Monad (StT s m) where
    return a = StT (\s -> return (a,s))
    StT f >>= g = StT $ \s -> do
                        (a,s') <- f s
                        runStT (g a) s'

instance Fail.MonadFail m => Fail.MonadFail (StT s m) where
    fail s = StT (\_ -> Fail.fail s)

get :: Monad m => StT s m s
get = StT $ \s -> return (s, s)

modify :: Monad m => (s -> s) -> StT s m ()
modify f = StT $ \s -> return ((),f s)

lift :: Monad m => m a -> StT s m a
lift m = StT $ \s -> m >>= \a -> return (a,s)

evalStT :: Monad m => StT s m a -> s -> m a
evalStT st s = liftM fst $ runStT st s

-- Our monad for parsing a list/tree of fields.
--
-- The state represents the remaining fields to be processed.
type PM a = StT [Field] ParseResult a



-- return look-ahead field or nothing if we're at the end of the file
peekField :: PM (Maybe Field)
peekField = liftM listToMaybe get

-- Unconditionally discard the first field in our state.  Will error when it
-- reaches end of file.  (Yes, that's evil.)
skipField :: PM ()
skipField = modify tail

parsePackageDescription :: String -> ParseResult GenericPackageDescription
parsePackageDescription file = do

    -- This function is quite complex because it needs to be able to parse
    -- both pre-Cabal-1.2 and post-Cabal-1.2 files.  Additionally, it contains
    -- a lot of parser-related noise since we do not want to depend on Parsec.
    --
    -- If we detect an pre-1.2 file we implicitly convert it to post-1.2
    -- style.  See 'sectionizeFields' below for details about the conversion.

    fields0 <- readFields file `catchParseError` \err ->
                 let tabs = findIndentTabs file in
                 case err of
                   -- In case of a TabsError report them all at once.
                   TabsError tabLineNo -> reportTabsError
                   -- but only report the ones including and following
                   -- the one that caused the actual error
                                            [ t | t@(lineNo',_) <- tabs
                                                , lineNo' >= tabLineNo ]
                   _ -> parseFail err

    let cabalVersionNeeded =
          head $ [ minVersionBound versionRange
                 | Just versionRange <- [ simpleParse v
                                        | F _ "cabal-version" v <- fields0 ] ]
              ++ [Version [0] []]
        minVersionBound versionRange =
          case asVersionIntervals versionRange of
            []                            -> Version [0] []
            ((LowerBound version _, _):_) -> version

    handleFutureVersionParseFailure cabalVersionNeeded $ do

      let sf = sectionizeFields fields0  -- ensure 1.2 format

        -- figure out and warn about deprecated stuff (warnings are collected
        -- inside our parsing monad)
      fields <- mapSimpleFields deprecField sf

        -- Our parsing monad takes the not-yet-parsed fields as its state.
        -- After each successful parse we remove the field from the state
        -- ('skipField') and move on to the next one.
        --
        -- Things are complicated a bit, because fields take a tree-like
        -- structure -- they can be sections or "if"/"else" conditionals.

      flip evalStT fields $ do

          -- The header consists of all simple fields up to the first section
          -- (flag, library, executable).
        header_fields <- getHeader []

          -- Parses just the header fields and stores them in a
          -- 'PackageDescription'.  Note that our final result is a
          -- 'GenericPackageDescription'; for pragmatic reasons we just store
          -- the partially filled-out 'PackageDescription' inside the
          -- 'GenericPackageDescription'.
        pkg <- lift $ parseFields pkgDescrFieldDescrs
                                  storeXFieldsPD
                                  emptyPackageDescription
                                  header_fields

          -- 'getBody' assumes that the remaining fields only consist of
          -- flags, lib and exe sections.
        (repos, flags, mlib, exes, tests, bms) <- getBody
        warnIfRest  -- warn if getBody did not parse up to the last field.
          -- warn about using old/new syntax with wrong cabal-version:
        maybeWarnCabalVersion (not $ oldSyntax fields0) pkg
        checkForUndefinedFlags flags mlib exes tests
        return $ GenericPackageDescription
                   pkg { sourceRepos = repos }
                   flags mlib exes tests bms

  where
    oldSyntax = all isSimpleField
    reportTabsError tabs =
        syntaxError (fst (head tabs)) $
          "Do not use tabs for indentation (use spaces instead)\n"
          ++ "  Tabs were used at (line,column): " ++ show tabs

    maybeWarnCabalVersion newsyntax pkg
      | newsyntax && specVersion pkg < Version [1,2] []
      = lift $ warning $
             "A package using section syntax must specify at least\n"
          ++ "'cabal-version: >= 1.2'."

    maybeWarnCabalVersion newsyntax pkg
      | not newsyntax && specVersion pkg >= Version [1,2] []
      = lift $ warning $
             "A package using 'cabal-version: "
          ++ displaySpecVersion (specVersionRaw pkg)
          ++ "' must use section syntax. See the Cabal user guide for details."
      where
        displaySpecVersion (Left version)       = display version
        displaySpecVersion (Right versionRange) =
          case asVersionIntervals versionRange of
            [] {- impossible -}           -> display versionRange
            ((LowerBound version _, _):_) -> display (orLaterVersion version)

    maybeWarnCabalVersion _ _ = return ()

    specVersion :: PackageDescription -> Version
    specVersion pkg = case specVersionRaw pkg of
      Left  version      -> version
      Right versionRange -> case asVersionIntervals versionRange of
                              []                            -> Version [0] []
                              ((LowerBound version _, _):_) -> version


    handleFutureVersionParseFailure cabalVersionNeeded parseBody =
      (unless versionOk (warning message) >> parseBody)
        `catchParseError` \parseError -> case parseError of
        TabsError _   -> parseFail parseError
        _ | versionOk -> parseFail parseError
          | otherwise -> Fail.fail message
      where versionOk = cabalVersionNeeded <= cabalVersion
            message   = "This package requires at least Cabal version "
                     ++ display cabalVersionNeeded

            cabalVersion :: Version
            cabalVersion = Version [1,16] []

    -- "Sectionize" an old-style Cabal file.  A sectionized file has:
    --
    --  * all global fields at the beginning, followed by
    --
    --  * all flag declarations, followed by
    --
    --  * an optional library section, and an arbitrary number of executable
    --    sections (in any order).
    --
    -- The current implementatition just gathers all library-specific fields
    -- in a library section and wraps all executable stanzas in an executable
    -- section.
    sectionizeFields :: [Field] -> [Field]
    sectionizeFields fs
      | oldSyntax fs =
          let
            -- "build-depends" is a local field now.  To be backwards
            -- compatible, we still allow it as a global field in old-style
            -- package description files and translate it to a local field by
            -- adding it to every non-empty section
            (hdr0, exes0) = break ((=="executable") . fName) fs
            (hdr, libfs0) = partition (not . (`elem` libFieldNames) . fName) hdr0

            (deps, libfs) = partition ((== "build-depends") . fName)
                                       libfs0

            exes = unfoldr toExe exes0
            toExe [] = Nothing
            toExe (F l e n : r)
              | e == "executable" =
                  let (efs, r') = break ((=="executable") . fName) r
                  in Just (Section l "executable" n (deps ++ efs), r')
            toExe _ = cabalBug "unexpected input to 'toExe'"
          in
            hdr ++
           (if null libfs then []
            else [Section (lineNo (head libfs)) "library" "" (deps ++ libfs)])
            ++ exes
      | otherwise = fs

    isSimpleField F{} = True
    isSimpleField _ = False

    -- warn if there's something at the end of the file
    warnIfRest :: PM ()
    warnIfRest = do
      s <- get
      case s of
        [] -> return ()
        _ -> lift $ warning "Ignoring trailing declarations."  -- add line no.

    -- all simple fields at the beginning of the file are (considered) header
    -- fields
    getHeader :: [Field] -> PM [Field]
    getHeader acc = peekField >>= \mf -> case mf of
        Just f@F{} -> skipField >> getHeader (f:acc)
        _ -> return (reverse acc)

    --
    -- body ::= { repo | flag | library | executable | test }+   -- at most one lib
    --
    -- The body consists of an optional sequence of declarations of flags and
    -- an arbitrary number of executables and at most one library.
    getBody :: PM ([SourceRepo], [Flag]
                  ,Maybe (CondTree ConfVar [Dependency] Library)
                  ,[(String, CondTree ConfVar [Dependency] Executable)]
                  ,[(String, CondTree ConfVar [Dependency] TestSuite)]
                  ,[(String, CondTree ConfVar [Dependency] Benchmark)])
    getBody = peekField >>= \mf -> case mf of
      Just (Section line_no sec_type sec_label sec_fields)
        | sec_type == "executable" -> do
            when (null sec_label) $ lift $ syntaxError line_no
              "'executable' needs one argument (the executable's name)"
            exename <- lift $ runP line_no "executable" parseTokenQ sec_label
            flds <- collectFields parseExeFields sec_fields
            skipField
            (repos, flags, lib, exes, tests, bms) <- getBody
            return (repos, flags, lib, (exename, flds): exes, tests, bms)

        | sec_type == "test-suite" -> do
            when (null sec_label) $ lift $ syntaxError line_no
                "'test-suite' needs one argument (the test suite's name)"
            testname <- lift $ runP line_no "test" parseTokenQ sec_label
            flds <- collectFields (parseTestFields line_no) sec_fields

            -- Check that a valid test suite type has been chosen. A type
            -- field may be given inside a conditional block, so we must
            -- check for that before complaining that a type field has not
            -- been given. The test suite must always have a valid type, so
            -- we need to check both the 'then' and 'else' blocks, though
            -- the blocks need not have the same type.
            let checkTestType ts ct =
                    let ts' = mappend ts $ condTreeData ct
                        -- If a conditional has only a 'then' block and no
                        -- 'else' block, then it cannot have a valid type
                        -- in every branch, unless the type is specified at
                        -- a higher level in the tree.
                        checkComponent (_, _, Nothing) = False
                        -- If a conditional has a 'then' block and an 'else'
                        -- block, both must specify a test type, unless the
                        -- type is specified higher in the tree.
                        checkComponent (_, t, Just e) =
                            checkTestType ts' t && checkTestType ts' e
                        -- Does the current node specify a test type?
                        hasTestType = testInterface ts'
                            /= testInterface emptyTestSuite
                        components = condTreeComponents ct
                    -- If the current level of the tree specifies a type,
                    -- then we are done. If not, then one of the conditional
                    -- branches below the current node must specify a type.
                    -- Each node may have multiple immediate children; we
                    -- only one need one to specify a type because the
                    -- configure step uses 'mappend' to join together the
                    -- results of flag resolution.
                    in hasTestType || any checkComponent components
            if checkTestType emptyTestSuite flds
                then do
                    skipField
                    (repos, flags, lib, exes, tests, bms) <- getBody
                    return (repos, flags, lib, exes, (testname, flds) : tests, bms)
                else lift $ syntaxError line_no $
                         "Test suite \"" ++ testname
                      ++ "\" is missing required field \"type\" or the field "
                      ++ "is not present in all conditional branches. The "
                      ++ "available test types are: "
                      ++ intercalate ", " (map display knownTestTypes)

        | sec_type == "benchmark" -> do
            when (null sec_label) $ lift $ syntaxError line_no
                "'benchmark' needs one argument (the benchmark's name)"
            benchname <- lift $ runP line_no "benchmark" parseTokenQ sec_label
            flds <- collectFields (parseBenchmarkFields line_no) sec_fields

            -- Check that a valid benchmark type has been chosen. A type
            -- field may be given inside a conditional block, so we must
            -- check for that before complaining that a type field has not
            -- been given. The benchmark must always have a valid type, so
            -- we need to check both the 'then' and 'else' blocks, though
            -- the blocks need not have the same type.
            let checkBenchmarkType ts ct =
                    let ts' = mappend ts $ condTreeData ct
                        -- If a conditional has only a 'then' block and no
                        -- 'else' block, then it cannot have a valid type
                        -- in every branch, unless the type is specified at
                        -- a higher level in the tree.
                        checkComponent (_, _, Nothing) = False
                        -- If a conditional has a 'then' block and an 'else'
                        -- block, both must specify a benchmark type, unless the
                        -- type is specified higher in the tree.
                        checkComponent (_, t, Just e) =
                            checkBenchmarkType ts' t && checkBenchmarkType ts' e
                        -- Does the current node specify a benchmark type?
                        hasBenchmarkType = benchmarkInterface ts'
                            /= benchmarkInterface emptyBenchmark
                        components = condTreeComponents ct
                    -- If the current level of the tree specifies a type,
                    -- then we are done. If not, then one of the conditional
                    -- branches below the current node must specify a type.
                    -- Each node may have multiple immediate children; we
                    -- only one need one to specify a type because the
                    -- configure step uses 'mappend' to join together the
                    -- results of flag resolution.
                    in hasBenchmarkType || any checkComponent components
            if checkBenchmarkType emptyBenchmark flds
                then do
                    skipField
                    (repos, flags, lib, exes, tests, bms) <- getBody
                    return (repos, flags, lib, exes, tests, (benchname, flds) : bms)
                else lift $ syntaxError line_no $
                         "Benchmark \"" ++ benchname
                      ++ "\" is missing required field \"type\" or the field "
                      ++ "is not present in all conditional branches. The "
                      ++ "available benchmark types are: "
                      ++ intercalate ", " (map display knownBenchmarkTypes)

        | sec_type == "library" -> do
            unless (null sec_label) $ lift $
              syntaxError line_no "'library' expects no argument"
            flds <- collectFields parseLibFields sec_fields
            skipField
            (repos, flags, lib, exes, tests, bms) <- getBody
            when (isJust lib) $ lift $ syntaxError line_no
              "There can only be one library section in a package description."
            return (repos, flags, Just flds, exes, tests, bms)

        | sec_type == "flag" -> do
            when (null sec_label) $ lift $
              syntaxError line_no "'flag' needs one argument (the flag's name)"
            flag <- lift $ parseFields
                    flagFieldDescrs
                    warnUnrec
                    (MkFlag (FlagName (lowercase sec_label)) "" True False)
                    sec_fields
            skipField
            (repos, flags, lib, exes, tests, bms) <- getBody
            return (repos, flag:flags, lib, exes, tests, bms)

        | sec_type == "source-repository" -> do
            when (null sec_label) $ lift $ syntaxError line_no $
                 "'source-repository' needs one argument, "
              ++ "the repo kind which is usually 'head' or 'this'"
            kind <- case simpleParse sec_label of
              Just kind -> return kind
              Nothing   -> lift $ syntaxError line_no $
                             "could not parse repo kind: " ++ sec_label
            repo <- lift $ parseFields
                    sourceRepoFieldDescrs
                    warnUnrec
                    SourceRepo {
                      repoKind     = kind,
                      repoType     = Nothing,
                      repoLocation = Nothing,
                      repoModule   = Nothing,
                      repoBranch   = Nothing,
                      repoTag      = Nothing,
                      repoSubdir   = Nothing
                    }
                    sec_fields
            skipField
            (repos, flags, lib, exes, tests, bms) <- getBody
            return (repo:repos, flags, lib, exes, tests, bms)

        | otherwise -> do
            lift $ warning $ "Ignoring unknown section type: " ++ sec_type
            skipField
            getBody
      Just f -> do
            _ <- lift $ syntaxError (lineNo f) $
              "Construct not supported at this position: " ++ show f
            skipField
            getBody
      Nothing -> return ([], [], Nothing, [], [], [])

    -- Extracts all fields in a block and returns a 'CondTree'.
    --
    -- We have to recurse down into conditionals and we treat fields that
    -- describe dependencies specially.
    collectFields :: ([Field] -> PM a) -> [Field]
                  -> PM (CondTree ConfVar [Dependency] a)
    collectFields parser allflds = do

        let simplFlds = [ F l n v | F l n v <- allflds ]
            condFlds = [ f | f@IfBlock{} <- allflds ]

        let (depFlds, dataFlds) = partition isConstraint simplFlds

        a <- parser dataFlds
        deps <- liftM concat . mapM (lift . parseConstraint) $ depFlds

        ifs <- mapM processIfs condFlds

        return (CondNode a deps ifs)
      where
        isConstraint (F _ n _) = n `elem` constraintFieldNames
        isConstraint _ = False

        processIfs (IfBlock l c t e) = do
            cnd <- lift $ runP l "if" parseCondition c
            t' <- collectFields parser t
            e' <- case e of
                   [] -> return Nothing
                   es -> do fs <- collectFields parser es
                            return (Just fs)
            return (cnd, t', e')
        processIfs _ = cabalBug "processIfs called with wrong field type"

    parseLibFields :: [Field] -> PM Library
    parseLibFields = lift . parseFields libFieldDescrs storeXFieldsLib emptyLibrary

    -- Note: we don't parse the "executable" field here, hence the tail hack.
    parseExeFields :: [Field] -> PM Executable
    parseExeFields = lift . parseFields (tail executableFieldDescrs)
                                        storeXFieldsExe emptyExecutable

    parseTestFields :: LineNo -> [Field] -> PM TestSuite
    parseTestFields line fields = do
        x <- lift $ parseFields testSuiteFieldDescrs storeXFieldsTest
                                emptyTestStanza fields
        lift $ validateTestSuite line x

    parseBenchmarkFields :: LineNo -> [Field] -> PM Benchmark
    parseBenchmarkFields line fields = do
        x <- lift $ parseFields benchmarkFieldDescrs storeXFieldsBenchmark
                                emptyBenchmarkStanza fields
        lift $ validateBenchmark line x

    checkForUndefinedFlags ::
        [Flag] ->
        Maybe (CondTree ConfVar [Dependency] Library) ->
        [(String, CondTree ConfVar [Dependency] Executable)] ->
        [(String, CondTree ConfVar [Dependency] TestSuite)] ->
        PM ()
    checkForUndefinedFlags flags mlib exes tests = do
        let definedFlags = map flagName flags
        maybe (return ()) (checkCondTreeFlags definedFlags) mlib
        mapM_ (checkCondTreeFlags definedFlags . snd) exes
        mapM_ (checkCondTreeFlags definedFlags . snd) tests

    checkCondTreeFlags :: [FlagName] -> CondTree ConfVar c a -> PM ()
    checkCondTreeFlags definedFlags ct = do
        let fv = nub $ freeVars ct
        unless (all (`elem` definedFlags) fv) $
            Fail.fail $ "These flags are used without having been defined: "
                ++ intercalate ", " [ n | FlagName n <- fv \\ definedFlags ]

    findIndentTabs :: String -> [(Int,Int)]
    findIndentTabs = concatMap checkLine
                   . zip [1..]
                   . lines
        where
          checkLine (lineno, l) =
              let (indent, _content) = span isSpace l
                  tabCols = map fst . filter ((== '\t') . snd) . zip [0..]
                  addLineNo = map (\col -> (lineno,col))
              in addLineNo (tabCols indent)

parseCondition :: ReadP (Condition ConfVar)
parseCondition = condOr
  where
    condOr   = sepBy1 condAnd (oper "||") >>= return . foldl1 COr
    condAnd  = sepBy1 cond (oper "&&")>>= return . foldl1 CAnd
    cond     = sp >> (boolLiteral +++ inparens condOr +++ notCond +++ osCond
                      +++ archCond +++ flagCond +++ implCond )
    inparens   = between (ReadP.char '(' >> sp) (sp >> ReadP.char ')' >> sp)
    notCond  = ReadP.char '!' >> sp >> cond >>= return . CNot
    osCond   = string "os" >> sp >> inparens osIdent >>= return . Var
    archCond = string "arch" >> sp >> inparens archIdent >>= return . Var
    flagCond = string "flag" >> sp >> inparens flagIdent >>= return . Var
    implCond = string "impl" >> sp >> inparens implIdent >>= return . Var
    boolLiteral   = fmap Lit  parse
    archIdent     = fmap Arch parse
    osIdent       = fmap OS   parse
    flagIdent     = fmap (Flag . FlagName . lowercase) (munch1 isIdentChar)
    isIdentChar c = isAlphaNum c || c == '_' || c == '-'
    oper s        = sp >> string s >> sp
    sp            = skipSpaces
    implIdent     = do i <- parse
                       vr <- sp >> option AnyVersion parse
                       return $ Impl i vr

freeVars :: CondTree ConfVar c a  -> [FlagName]
freeVars t = [ f | Flag f <- freeVars' t ]
  where
    freeVars' (CondNode _ _ ifs) = concatMap compfv ifs
    compfv (c, ct, mct) = condfv c ++ freeVars' ct ++ maybe [] freeVars' mct
    condfv c = case c of
      Var v      -> [v]
      Lit _      -> []
      CNot c'    -> condfv c'
      COr c1 c2  -> condfv c1 ++ condfv c2
      CAnd c1 c2 -> condfv c1 ++ condfv c2


emptyPackageDescription :: PackageDescription
emptyPackageDescription
    =  PackageDescription {
                      package      = PackageId (PackageName "")
                                               (Version [] []),
                      license      = AllRightsReserved,
                      licenseFile  = "",
                      specVersionRaw = Right AnyVersion,
                      buildType    = Nothing,
                      copyright    = "",
                      maintainer   = "",
                      author       = "",
                      stability    = "",
                      testedWith   = [],
                      buildDepends = [],
                      homepage     = "",
                      pkgUrl       = "",
                      bugReports   = "",
                      sourceRepos  = [],
                      synopsis     = "",
                      description  = "",
                      category     = "",
                      customFieldsPD = [],
                      library      = Nothing,
                      executables  = [],
                      testSuites   = [],
                      benchmarks   = [],
                      dataFiles    = [],
                      dataDir      = "",
                      extraSrcFiles = [],
                      extraTmpFiles = [],
                      extraDocFiles = []
                     }


instance Monoid Library where
  mempty = Library {
    exposedModules = mempty,
    libExposed     = True,
    libBuildInfo   = mempty
  }
  mappend = mappendLibrary

instance Semigroup Library where
  a <> b = mappendLibrary a b

mappendLibrary :: Library -> Library -> Library
mappendLibrary a b = Library {
    exposedModules = combine exposedModules,
    libExposed     = libExposed a && libExposed b, -- so False propagates
    libBuildInfo   = combine libBuildInfo
  }
    where combine field = field a `mappend` field b

emptyLibrary :: Library
emptyLibrary = mempty

instance Monoid Executable where
  mempty = Executable {
    exeName    = mempty,
    modulePath = mempty,
    buildInfo  = mempty
  }
  mappend = mappendExecutable

instance Semigroup Executable where
  a <> b = mappendExecutable a b

mappendExecutable :: Executable -> Executable -> Executable
mappendExecutable a b = Executable {
    exeName    = combine' exeName,
    modulePath = combine modulePath,
    buildInfo  = combine buildInfo
  }
    where combine field = field a `mappend` field b
          combine' field = case (field a, field b) of
                      ("","") -> ""
                      ("", x) -> x
                      (x, "") -> x
                      (x, y) -> error $ "Ambiguous values for executable field: '"
                                  ++ x ++ "' and '" ++ y ++ "'"

emptyExecutable :: Executable
emptyExecutable = mempty

instance Monoid TestSuite where
    mempty = TestSuite {
        testName      = mempty,
        testInterface = mempty,
        testBuildInfo = mempty,
        testEnabled   = False
    }
    mappend = mappendTestSuite

instance Semigroup TestSuite where
    a <> b = mappendTestSuite a b

mappendTestSuite :: TestSuite -> TestSuite -> TestSuite
mappendTestSuite a b = TestSuite {
        testName      = combine' testName,
        testInterface = combine  testInterface,
        testBuildInfo = combine  testBuildInfo,
        testEnabled   = testEnabled a || testEnabled b
    }
        where combine   field = field a `mappend` field b
              combine' f = case (f a, f b) of
                        ("", x) -> x
                        (x, "") -> x
                        (x, y) -> error "Ambiguous values for test field: '"
                            ++ x ++ "' and '" ++ y ++ "'"

instance Monoid TestSuiteInterface where
    mempty  =  TestSuiteUnsupported (TestTypeUnknown mempty (Version [] []))
    mappend = mappendTestSuiteInterface

instance Semigroup TestSuiteInterface where
    a <> b = mappendTestSuiteInterface a b

mappendTestSuiteInterface :: TestSuiteInterface -> TestSuiteInterface -> TestSuiteInterface
mappendTestSuiteInterface a (TestSuiteUnsupported _) = a
mappendTestSuiteInterface _ b                        = b

emptyTestSuite :: TestSuite
emptyTestSuite = mempty

instance Monoid Benchmark where
    mempty = Benchmark {
        benchmarkName      = mempty,
        benchmarkInterface = mempty,
        benchmarkBuildInfo = mempty,
        benchmarkEnabled   = False
    }
    mappend = mappendBenchmark

mappendBenchmark :: Benchmark -> Benchmark -> Benchmark
mappendBenchmark a b = Benchmark {
        benchmarkName      = combine' benchmarkName,
        benchmarkInterface = combine  benchmarkInterface,
        benchmarkBuildInfo = combine  benchmarkBuildInfo,
        benchmarkEnabled   = benchmarkEnabled a || benchmarkEnabled b
    }
        where combine   field = field a `mappend` field b
              combine' f = case (f a, f b) of
                        ("", x) -> x
                        (x, "") -> x
                        (x, y) -> error "Ambiguous values for benchmark field: '"
                            ++ x ++ "' and '" ++ y ++ "'"

instance Monoid BenchmarkInterface where
    mempty  =  BenchmarkUnsupported (BenchmarkTypeUnknown mempty (Version [] []))
    mappend = mappendBenchmarkInterface

mappendBenchmarkInterface :: BenchmarkInterface -> BenchmarkInterface -> BenchmarkInterface
mappendBenchmarkInterface a (BenchmarkUnsupported _) = a
mappendBenchmarkInterface _ b                        = b



emptyBenchmark :: Benchmark
emptyBenchmark = mempty

instance Monoid BuildInfo where
  mempty = BuildInfo {
    buildable         = True,
    buildTools        = [],
    cppOptions        = [],
    ccOptions         = [],
    ldOptions         = [],
    pkgconfigDepends  = [],
    frameworks        = [],
    cSources          = [],
    hsSourceDirs      = [],
    otherModules      = [],
    defaultLanguage   = Nothing,
    otherLanguages    = [],
    defaultExtensions = [],
    otherExtensions   = [],
    oldExtensions     = [],
    extraLibs         = [],
    extraLibDirs      = [],
    includeDirs       = [],
    includes          = [],
    installIncludes   = [],
    options           = [],
    ghcProfOptions    = [],
    ghcSharedOptions  = [],
    customFieldsBI    = [],
    targetBuildDepends = []
  }
  mappend = mappendBuildInfo

mappendBuildInfo :: BuildInfo -> BuildInfo -> BuildInfo
mappendBuildInfo a b = BuildInfo {
    buildable         = buildable a && buildable b,
    buildTools        = combine    buildTools,
    cppOptions        = combine    cppOptions,
    ccOptions         = combine    ccOptions,
    ldOptions         = combine    ldOptions,
    pkgconfigDepends  = combine    pkgconfigDepends,
    frameworks        = combineNub frameworks,
    cSources          = combineNub cSources,
    hsSourceDirs      = combineNub hsSourceDirs,
    otherModules      = combineNub otherModules,
    defaultLanguage   = combineMby defaultLanguage,
    otherLanguages    = combineNub otherLanguages,
    defaultExtensions = combineNub defaultExtensions,
    otherExtensions   = combineNub otherExtensions,
    oldExtensions     = combineNub oldExtensions,
    extraLibs         = combine    extraLibs,
    extraLibDirs      = combineNub extraLibDirs,
    includeDirs       = combineNub includeDirs,
    includes          = combineNub includes,
    installIncludes   = combineNub installIncludes,
    options           = combine    options,
    ghcProfOptions    = combine    ghcProfOptions,
    ghcSharedOptions  = combine    ghcSharedOptions,
    customFieldsBI    = combine    customFieldsBI,
    targetBuildDepends = combineNub targetBuildDepends
  }
    where
      combine    field = field a `mappend` field b
      combineNub field = nub (combine field)
      combineMby field = field b `mplus` field a


instance Semigroup Benchmark where
  a <> b = mappendBenchmark a b

instance Semigroup BenchmarkInterface where
  a <> b = mappendBenchmarkInterface a b

instance Semigroup BuildInfo where
  a <> b = mappendBuildInfo a b

-- | Parse a list of fields, given a list of field descriptions,
--   a structure to accumulate the parsed fields, and a function
--   that can decide what to do with fields which don't match any
--   of the field descriptions.
parseFields :: [FieldDescr a]      -- ^ descriptions of fields we know how to
                                   --   parse
            -> UnrecFieldParser a  -- ^ possibly do something with
                                   --   unrecognized fields
            -> a                   -- ^ accumulator
            -> [Field]             -- ^ fields to be parsed
            -> ParseResult a
parseFields descrs unrec ini fields =
    do (a, unknowns) <- foldM (parseField descrs unrec) (ini, []) fields
       unless (null unknowns) $ warning $ render $
         text "Unknown fields:" <+>
              commaSep (map (\(l,u) -> u ++ " (line " ++ show l ++ ")")
                            (reverse unknowns))
         $+$
         text "Fields allowed in this section:" $$
           nest 4 (commaSep $ map fieldName descrs)
       return a
  where
    commaSep = fsep . punctuate comma . map text

parseField :: [FieldDescr a]     -- ^ list of parseable fields
           -> UnrecFieldParser a -- ^ possibly do something with
                                 --   unrecognized fields
           -> (a,[(Int,String)]) -- ^ accumulated result and warnings
           -> Field              -- ^ the field to be parsed
           -> ParseResult (a, [(Int,String)])
parseField (FieldDescr name _ parser : fields) unrec (a, us) (F line f val)
  | name == f = parser line val a >>= \a' -> return (a',us)
  | otherwise = parseField fields unrec (a,us) (F line f val)
parseField [] unrec (a,us) (F l f val) = return $
  case unrec (f,val) a of        -- no fields matched, see if the 'unrec'
    Just a' -> (a',us)           -- function wants to do anything with it
    Nothing -> (a, (l,f):us)
parseField _ _ _ _ = cabalBug "'parseField' called on a non-field"

deprecatedFields :: [(String,String)]
deprecatedFields =
    deprecatedFieldsPkgDescr ++ deprecatedFieldsBuildInfo

deprecatedFieldsPkgDescr :: [(String,String)]
deprecatedFieldsPkgDescr = [ ("other-files", "extra-source-files") ]

deprecatedFieldsBuildInfo :: [(String,String)]
deprecatedFieldsBuildInfo = [ ("hs-source-dir","hs-source-dirs") ]

-- Handle deprecated fields
deprecField :: Field -> ParseResult Field
deprecField (F line fld val) = do
  fld' <- case lookup fld deprecatedFields of
            Nothing -> return fld
            Just newName -> do
              warning $ "The field \"" ++ fld
                      ++ "\" is deprecated, please use \"" ++ newName ++ "\""
              return newName
  return (F line fld' val)
deprecField _ = cabalBug "'deprecField' called on a non-field"

userBug :: String -> a
userBug msg = error $ msg ++ ". This is a bug in your .cabal file."

cabalBug :: String -> a
cabalBug msg = error $ msg ++ ". This is possibly a bug in Cabal.\n"
               ++ "Please report it to the developers: "
               ++ "https://github.com/haskell/cabal/issues/new"

