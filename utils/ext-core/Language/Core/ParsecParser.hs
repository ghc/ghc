{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module Language.Core.ParsecParser (parseCore, coreModuleName, coreTcon, 
  coreQualifiedGen, upperName, identifier, coreType, coreKind,
  coreTbinds, parens, braces, topVbind) where

import Language.Core.Core
import Language.Core.Check
import Language.Core.Encoding
import Language.Core.PrimCoercions

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Data.Char
import Data.List
import Data.Ratio

parseCore :: FilePath -> IO (Either ParseError Module)
parseCore = parseFromFile coreModule

coreModule :: Parser Module
coreModule = do
   whiteSpace
   reserved "module"
   mName      <- coreModuleName
   whiteSpace
   tdefs      <- option [] coreTdefs
   vdefGroups <- coreVdefGroups
   eof
   return $ Module mName tdefs vdefGroups

coreModuleName :: Parser AnMname
coreModuleName = do
   pkgName      <- corePackageName
   char ':'
   (modHierarchy,baseName) <- coreHierModuleNames
   return $ M (pkgName, modHierarchy, baseName)

corePackageName :: Parser Pname
-- Package names can be lowercase or uppercase!
corePackageName = (identifier <|> upperName) >>= (return . P)

coreHierModuleNames :: Parser ([Id], Id)
coreHierModuleNames = do
   parentName <- upperName
   return $ splitModuleName parentName

upperName :: Parser Id
upperName = do
   firstChar <- upper
   rest <- many (identLetter extCoreDef)
   return $ firstChar:rest

coreTdefs :: Parser [Tdef]
coreTdefs = many coreTdef 

coreTdef :: Parser Tdef
coreTdef = withSemi (try (try coreDataDecl <|> try coreNewtypeDecl))
            

withSemi p = try p `withTerminator` ";"

withTerminator p term = do
   x <- try p
   try $ symbol term
   return x

coreDataDecl :: Parser Tdef
coreDataDecl = do
  reserved "data"
  tyCon  <- coreQualifiedCon
  whiteSpace -- important
  tBinds <- coreTbinds
  whiteSpace
  symbol "="
  whiteSpace
  cDefs  <- braces coreCdefs
  return $ Data tyCon tBinds cDefs

coreNewtypeDecl :: Parser Tdef
coreNewtypeDecl = do
  reserved "newtype"
  tyCon  <- coreQualifiedCon
  whiteSpace
  coercionName <- coreQualifiedCon
  whiteSpace
  tBinds <- coreTbinds
  tyRep  <- try coreTRep
  return $ Newtype tyCon coercionName tBinds tyRep

coreQualifiedCon :: Parser (Mname, Id)
coreQualifiedCon = coreQualifiedGen upperName
 
coreQualifiedName = coreQualifiedGen identifier

coreQualifiedGen :: Parser String -> Parser (Mname, Id) 
coreQualifiedGen p = (try (do
  packageIdOrVarName <- corePackageName
  maybeRest <- optionMaybe (char ':' >> coreHierModuleNames)
  case maybeRest of
               -- unqualified id, so backtrack
    Nothing -> pzero
               -- qualified name, so look for the id part
    Just (modHierarchy, baseName) -> do
               char '.'
               theId <- p
               return
                 (Just $ M (packageIdOrVarName, modHierarchy, baseName),
                  theId))) <|> 
   -- unqualified name
   (p >>= (\ res -> return (Nothing, res)))

coreTbinds :: Parser [Tbind]
coreTbinds = many coreTbind 

coreTbindsGen :: CharParser () String -> Parser [Tbind]
-- The "try" here is important. Otherwise, when parsing:
-- "Node (^base:DataziTuple.Z3T)" (a cdef), we commit to
-- parsing (^base...) as a tbind rather than a type.
coreTbindsGen separator = many (try $ coreTbindGen separator)

coreTbind :: Parser Tbind
coreTbind = coreTbindGen whiteSpace

coreTbindGen :: CharParser () a -> Parser Tbind
coreTbindGen sep = (parens (do
                     sep
                     tyVar <- identifier
                     kind <- symbol "::" >> coreKind
                     return (tyVar, kind))) <|>
                    (sep >> identifier >>= (return . (\ tv -> (tv,Klifted))))

coreCdefs :: Parser [Cdef]
coreCdefs = sepBy coreCdef (symbol ";")

coreCdef :: Parser Cdef
coreCdef = do
  dataConName <- coreQualifiedCon
  whiteSpace -- important!
  tBinds      <- try $ coreTbindsGen (symbol "@")
  -- This should be equivalent to (many coreAty)
  -- But it isn't. WHY??
  tys         <- sepBy coreAtySaturated whiteSpace
  return $ Constr dataConName tBinds tys

coreTRep :: Parser Ty
-- note that the "=" is inside here since if there's
-- no rhs for the newtype, there's no "="
coreTRep = symbol "=" >> try coreType

coreType :: Parser Ty
coreType = coreForallTy <|> (do
             hd <- coreBty
             -- whiteSpace is important!
             whiteSpace
             -- This says: If there is at least one ("-> ty"..) thing,
             -- use it. If not, don't consume any input.
             maybeRest <- option [] (many1 (symbol "->" >> coreType))
             return $ case maybeRest of
                         [] -> hd
                         stuff -> foldl Tapp (Tcon tcArrow) (hd:stuff))

coreBty :: Parser Ty
coreBty = do
  hd <- coreAty
                         -- The "try" is necessary:
                         -- otherwise, parsing "T " fails rather
                         -- than returning "T".
  maybeRest <- option [] (many1 (try (whiteSpace >> coreAtySaturated)))
  return $ (case hd of
             -- so I'm not sure I like this... it's basically doing
             -- typechecking (kind-checking?) in the parser.
             -- However, the type syntax as defined in Core.hs sort of
             -- forces it.
             ATy t     -> foldl Tapp t maybeRest
             Trans k   -> app k 2 maybeRest "trans"
             Sym k     -> app k 1 maybeRest "sym"
             Unsafe k  -> app k 2 maybeRest "unsafe"
             LeftCo k  -> app k 1 maybeRest "left"
             RightCo k -> app k 1 maybeRest "right"
             InstCo k  -> app k 2 maybeRest "inst")
                 where app k arity args _ | length args == arity = k args
                       app _ _ args err = 
                           primCoercionError (err ++ 
                             ("Args were: " ++ show args))

coreAtySaturated :: Parser Ty
coreAtySaturated = do
   t <- coreAty
   case t of
     ATy ty -> return ty
     _     -> unexpected "coercion ty"

coreAty :: Parser ATyOp
coreAty = try coreTcon <|> ((try coreTvar <|> parens coreType)
                             >>= return . ATy)
coreTvar :: Parser Ty
coreTvar = try identifier >>= (return . Tvar)

coreTcon :: Parser ATyOp
-- TODO: Change the grammar
-- A Tcon can be an uppercase type constructor
-- or a lowercase (always qualified) coercion variable
coreTcon =  
         -- Special case is first so that (CoUnsafe .. ..) gets parsed as
         -- a prim. coercion app and not a Tcon app.
         -- But the whole thing is so bogus.
        try (do
                                    -- the "try"s are crucial; they force
                                    -- backtracking
           maybeCoercion <- choice [try symCo, try transCo, try unsafeCo,
                                    try instCo, try leftCo, rightCo]
           return $ case maybeCoercion of
              TransC  -> Trans (\ [x,y] -> TransCoercion x y)
              SymC    -> Sym (\ [x] -> SymCoercion x)
              UnsafeC -> Unsafe (\ [x,y] -> UnsafeCoercion x y)
              LeftC   -> LeftCo (\ [x] -> LeftCoercion x)
              RightC  -> RightCo (\ [x] -> RightCoercion x)
              InstC   -> InstCo (\ [x,y] -> InstCoercion x y))
    <|> (coreQualifiedCon >>= (return . ATy . Tcon))

data CoercionTy = TransC | InstC | SymC | UnsafeC | LeftC | RightC

symCo, transCo, unsafeCo, instCo, leftCo, rightCo :: Parser CoercionTy
symCo    = string "%sym"    >> return SymC
transCo  = string "%trans"  >> return TransC
unsafeCo = string "%unsafe" >> return UnsafeC
leftCo   = string "%left"   >> return LeftC
rightCo  = string "%right"  >> return RightC
instCo   = string "%inst"   >> return InstC

coreForallTy :: Parser Ty
coreForallTy = do
  reserved "forall"
  tBinds <- many1 coreTbind
  symbol "."
  bodyTy <- coreType
  return $ foldr Tforall bodyTy tBinds

-- TODO: similar to coreType. should refactor
coreKind :: Parser Kind
coreKind = do
  hd <- coreAtomicKind 
  maybeRest <- option [] (many1 (symbol "->" >> coreKind))
  return $ foldl Karrow hd maybeRest

coreAtomicKind = try liftedKind <|> try unliftedKind 
       <|> try openKind <|> try (do
                    (from,to) <- parens equalityKind
                    return $ Keq from to)
       <|> try (parens coreKind)

liftedKind = do
  symbol "*"
  return Klifted

unliftedKind = do
  symbol "#"
  return Kunlifted

openKind = do
  symbol "?"
  return Kopen

equalityKind = do
  ty1 <- coreBty
  symbol ":=:"
  ty2 <- coreBty
  return (ty1, ty2)

-- Only used internally within the parser:
-- represents either a Tcon, or a continuation
-- for a primitive coercion
data ATyOp = 
   ATy Ty
 | Trans ([Ty] -> Ty)
 | Sym ([Ty] -> Ty)
 | Unsafe ([Ty] -> Ty)
 | LeftCo ([Ty] -> Ty)
 | RightCo ([Ty] -> Ty)
 | InstCo ([Ty] -> Ty)

coreVdefGroups :: Parser [Vdefg]
coreVdefGroups = option [] (do
  theFirstVdef <- coreVdefg
  symbol ";"
  others <- coreVdefGroups
  return $ theFirstVdef:others)

coreVdefg :: Parser Vdefg
coreVdefg = coreRecVdef <|> coreNonrecVdef

coreRecVdef = do
  reserved "rec"
  braces (sepBy1 coreVdef (symbol ";")) >>= (return . Rec)

coreNonrecVdef = coreVdef >>= (return . Nonrec)

coreVdef = do
  (vdefLhs, vdefTy) <- try topVbind <|> (do
                        (v, ty) <- lambdaBind
                        return (unqual v, ty))
  whiteSpace
  symbol "="
  whiteSpace
  vdefRhs  <- coreFullExp
  return $ Vdef (vdefLhs, vdefTy, vdefRhs) 

coreAtomicExp :: Parser Exp
coreAtomicExp = do
-- For stupid reasons, the whiteSpace is necessary.
-- Without it, (pt coreAppExp "w a:B.C ") doesn't work.
  whiteSpace
  res <- choice [try coreDconOrVar,
                    try coreLit,
                    parens coreFullExp ]
  whiteSpace
  return res

coreFullExp = (choice [coreLam, coreLet,
  coreCase, coreCast, coreNote, coreExternal, coreLabel]) <|> (try coreAppExp)
-- The "try" is necessary so that we backtrack
-- when we see a var (that is not an app)
    <|> coreAtomicExp

coreAppExp = do
-- notes:
-- it's important to have a separate coreAtomicExp (that any app exp
-- begins with) and to define the args in terms of many1.
-- previously, coreAppExp could parse either an atomic exp (an app with
-- 0 arguments) or an app with >= 1 arguments, but that led to ambiguity.
    oper <- try coreAtomicExp
    whiteSpace
    args <- many1 (whiteSpace >> ((coreAtomicExp >>= (return . Left)) <|>
             -- note this MUST be coreAty, not coreType, because otherwise:
             -- "A @ B c" gets parsed as "A @ (B c)"
             ((symbol "@" >> coreAtySaturated) >>= (return . Right))))
    return $ foldl (\ op ->
                     either (App op) (Appt op)) oper args

coreDconOrVar = do
  theThing <- coreQualifiedGen (try upperName <|> identifier)
  return $ case theThing of
    -- note that data constructors must be qualified
    (Just _, idItself) | isUpper (head idItself)
      -> Dcon theThing
    _ -> Var theThing

coreLit :: Parser Exp
coreLit = parens (coreLiteral >>= (return . Lit))

coreLiteral :: Parser Lit
coreLiteral = do
  l <- try aLit
  symbol "::"
  t <- coreType
  return $ Literal l t

coreLam = do
  symbol "\\"
  binds <- coreLambdaBinds
  symbol "->"
  body <- coreFullExp
  return $ foldr Lam body binds
coreLet = do
  reserved "let"
  vdefg <- coreVdefg
  whiteSpace
  reserved "in"
  body <- coreFullExp
  return $ Let vdefg body 
coreCase = do
  reserved "case"
  ty <- coreAtySaturated
  scrut <- coreAtomicExp
  reserved "of"
  vBind <- parens lambdaBind
  alts <- coreAlts
  return $ Case scrut vBind ty alts
coreCast = do
  reserved "cast"
  whiteSpace
-- The parens are CRUCIAL, o/w it's ambiguous
  body <- try (parens coreFullExp)
  whiteSpace
  ty <- try coreAtySaturated
  return $ Cast body ty
coreNote = do
  reserved "note"
  s <- stringLiteral
  e <- coreFullExp
  return $ Note s e
coreExternal = (do
  reserved "external"
  -- TODO: This isn't in the grammar, but GHC
  -- always prints "external ccall". investigate...
  symbol "ccall"
  s <- stringLiteral
  t <- coreAtySaturated
  return $ External s t) <|>
    -- TODO: I don't really understand what this does
                (do
    reserved "dynexternal"
    symbol "ccall"
    t <- coreAtySaturated
    return $ External "[dynamic]" t)
coreLabel = do
-- TODO: Totally punting this, but it needs to go in the grammar
-- or not at all
  reserved "label"
  s <- stringLiteral
  return $ External s tAddrzh

coreLambdaBinds = many1 coreBind

coreBind = coreTbinding <|> coreVbind

coreTbinding = try coreAtTbind >>= (return . Tb)
coreVbind = parens (lambdaBind >>= (return . Vb))

coreAtTbind = (symbol "@") >> coreTbind

topVbind :: Parser (Qual Var, Ty)
topVbind   = aCoreVbind coreQualifiedName
lambdaBind :: Parser (Var, Ty)
lambdaBind = aCoreVbind identifier

aCoreVbind idP =  do
  nm <- idP
  symbol "::"
  t <- coreType
  return (nm, t)


aLit :: Parser CoreLit
aLit = intOrRatLit <|> charLit <|> stringLit

intOrRatLit :: Parser CoreLit
intOrRatLit = do
 -- Int and lit combined into one to avoid ambiguity.
 -- Argh....
  lhs <- intLit
  maybeRhs <- optionMaybe (symbol "%" >> anIntLit)
  case maybeRhs of
    Nothing  -> return $ Lint lhs
    Just rhs -> return $ Lrational (lhs % rhs)

intLit :: Parser Integer
intLit = anIntLit <|> parens anIntLit

anIntLit :: Parser Integer
anIntLit = do
  sign <- option 1 (symbol "-" >> return (-1)) 
  n <- natural
  return (sign * n)

charLit :: Parser CoreLit
charLit = charLiteral >>= (return . Lchar)
 -- make sure this is right
   
stringLit :: Parser CoreLit
stringLit = stringLiteral >>= (return . Lstring)
 -- make sure this is right

coreAlts :: Parser [Alt]
coreAlts = braces $ sepBy1 coreAlt (symbol ";")

coreAlt :: Parser Alt
coreAlt = conAlt <|> litAlt <|> defaultAlt

conAlt :: Parser Alt
conAlt = do
  conName <- coreQualifiedCon
  whiteSpace
  (tBinds, vBinds) <- caseVarBinds
  try (symbol "->")
  rhs     <- try coreFullExp
  return $ Acon conName tBinds vBinds rhs

caseVarBinds :: Parser ([Tbind], [Vbind])
caseVarBinds = do
     maybeFirstTbind <- optionMaybe coreAtTbind
     case maybeFirstTbind of
        Just tb -> do
           (tbs,vbs) <- caseVarBinds
           return (tb:tbs, vbs)
        Nothing -> do
           vbs <- many (parens lambdaBind)
           return ([], vbs)

litAlt :: Parser Alt
litAlt = do
  l <- parens coreLiteral
  symbol "->"
  rhs <- coreFullExp
  return $ Alit l rhs

defaultAlt :: Parser Alt
defaultAlt = do
  reserved "_"
  symbol "->"
  rhs <- coreFullExp
  return $ Adefault rhs
----------------
-- ugh
splitModuleName mn = 
   let decoded = zDecodeString mn
       -- Triple ugh.
       -- We re-encode the individual parts so that:
       -- main:Foo_Bar.Quux.baz
       -- prints as:
       -- main:FoozuBarziQuux.baz
       -- and not:
       -- main:Foo_BarziQuux.baz
       parts   = map zEncodeString $ filter (notElem '.') $ groupBy 
                   (\ c1 c2 -> c1 /= '.' && c2 /= '.') 
                 decoded in
     (take (length parts - 1) parts, last parts)
----------------
extCore = P.makeTokenParser extCoreDef

parens          = P.parens extCore    
braces          = P.braces extCore    
-- newlines are allowed anywhere
whiteSpace      = P.whiteSpace extCore <|> (newline >> return ())
symbol          = P.symbol extCore    
identifier      = P.identifier extCore    
-- Keywords all begin with '%'
reserved  s     = P.reserved extCore ('%':s) 
natural         = P.natural extCore    
charLiteral     = P.charLiteral extCore    
stringLiteral   = P.stringLiteral extCore    

-- dodgy since Core doesn't really allow comments,
-- but we'll pretend...
extCoreDef = LanguageDef { 
      commentStart    = "{-"
    , commentEnd      = "-}"
    , commentLine     = "--"
    , nestedComments  = True
    , identStart      = lower
    , identLetter     = lower <|> upper <|> digit <|> (char '\'')
    , opStart         = opLetter extCoreDef
    , opLetter        = oneOf ";=@:\\%_.*#?%"
    , reservedNames   = map ('%' :)
                          ["module", "data", "newtype", "rec",
                           "let", "in", "case", "of", "cast",
                           "note", "external", "forall"]
    , reservedOpNames = [";", "=", "@", "::", "\\", "%_",
                          ".", "*", "#", "?"]
    , caseSensitive   = True
    }       

{-
-- Stuff to help with testing in ghci.
pTest (Left a) = error (show a)
pTest (Right t) = print t

pTest1 :: Show a => CharParser () a -> String -> IO ()
pTest1 pr s = do
  let res = parse pr "" s
  pTest res

pt :: Show a => CharParser () a -> String -> IO ()
pt pr s = do
  x <- parseTest pr s
  print x

try_ = try
many_ = many
option_ = option
many1_ = many1
il = identLetter

andThenSym a b = do
  p <- a
  symbol b
  return p
-}