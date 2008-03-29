module ParsecParser where

import Core
import ParseGlue

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
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
   return (pkgName, modHierarchy, baseName)

corePackageName :: Parser Pname
corePackageName = identifier

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
  tBinds <- coreTbinds
  symbol "^"
  axiom <- coreAxiom
  tyRep  <- try coreTRep
  return $ Newtype tyCon tBinds axiom tyRep

coreQualifiedCon :: Parser (Mname, Id)
coreQualifiedCon = coreQualifiedGen upperName

coreQualifiedName = coreQualifiedGen identifier

coreQualifiedGen p = do
  maybeMname <- coreMaybeMname
  theId      <- p
  return (maybeMname, theId)

coreMaybeMname = optionMaybe coreMname

coreRequiredQualifiedName = do
  mname <- coreMname
  theId <- identifier
  return (Just mname, theId)

coreMname = do
-- Notice the '^' goes here:
-- it's part of a variable *occurrence*, not a module name.
  char '^'
  nm <- try coreModuleName
  symbol "."
  return nm

coreAxiom :: Parser Axiom
coreAxiom = parens (do
              coercionName <- coreQualifiedCon
              whiteSpace
              symbol "::"
              whiteSpace
              coercionKind <- coreKind
              return (coercionName, coercionKind))

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
coreCdefs = sepBy1 coreCdef (symbol ";")

coreCdef :: Parser Cdef
coreCdef = do
  dataConName <- coreQualifiedCon
  whiteSpace -- important!
  tBinds      <- try $ coreTbindsGen (symbol "@")
  -- This should be equivalent to (many coreAty)
  -- But it isn't. WHY??
  tys         <- sepBy coreAty whiteSpace
  return $ Constr dataConName tBinds tys

coreTRep :: Parser (Maybe Ty)
-- note that the "=" is inside here since if there's
-- no rhs for the newtype, there's no "="
coreTRep = optionMaybe (do
              symbol "=" 
              try coreType)

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
coreBty = arrowThing coreAty coreAty whiteSpace Tapp

arrowThing :: Parser a -> Parser a -> Parser b -> (a -> a -> a) -> Parser a
arrowThing hdThing restThing sep op = do
  hd <- hdThing
                         -- The "try" is necessary:
                         -- otherwise, parsing "T " fails rather
                         -- than returning "T".
  maybeRest <- option [] (many1 (try (sep >> restThing)))
  return $ case maybeRest of 
             [] -> hd
             stuff -> foldl op hd maybeRest

coreAppTy :: Parser Ty
coreAppTy = do 
  bTy <- try coreBty
  whiteSpace
  aTy <- try coreAty
  return $ Tapp bTy aTy

coreAty = try coreTcon <|> try coreTvar <|> parens coreType

coreTvar :: Parser Ty
coreTvar = try identifier >>= (return . Tvar)

coreTcon :: Parser Ty
-- TODO: Change the grammar
-- A Tcon can be an uppercase type constructor
-- or a lowercase (always qualified) coercion variable
coreTcon = (try coreQualifiedCon <|> coreRequiredQualifiedName) 
             >>= (return . Tcon)

coreTyApp :: Parser Ty
coreTyApp = do
  operTy <- coreType
  randTy <- coreType
  return $ Tapp operTy randTy

coreFunTy :: Parser Ty
coreFunTy = do
  argTy <- coreBty
  whiteSpace
  symbol "->"
  whiteSpace
  resTy <- coreType
  return $ tArrow argTy resTy

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
  return $ case maybeRest of
             [] -> hd
             stuff -> foldl Karrow hd maybeRest

coreAtomicKind = try liftedKind <|> try unliftedKind 
       <|> try openKind <|> try (parens equalityKind) 
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
  return $ Keq ty1 ty2
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
  (vdefLhs, vdefTy) <- topVbind
  whiteSpace
  symbol "="
  whiteSpace
  vdefRhs  <- coreFullExp
  return $ Vdef (vdefLhs, vdefTy, vdefRhs) 

coreAtomicExp :: Parser Exp
coreAtomicExp = do
-- For stupid reasons, the whiteSpace is necessary.
-- Without it, (pt coreAppExp "w ^a:B.C ") doesn't work.
  whiteSpace
  res <- choice [ try coreVar,
                    coreDcon,
                    try coreLit,
                    parens coreFullExp ]
  whiteSpace
  return res

coreFullExp = (choice [coreLam, coreLet,
  coreCase, coreCast, coreNote, coreExternal]) <|> (try coreAppExp)
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
             ((symbol "@" >> coreAty) >>= (return . Right))))
    return $ foldl (\ op ->
                     either (App op) (Appt op)) oper args

coreVar = ((try coreQualifiedName) <|> (identifier >>= (return . unqual)))
             >>= (return . Var)
coreDcon = coreQualifiedCon >>= (return . Dcon)

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
  ty <- coreAty
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
  ty <- try coreAty
  return $ Cast body ty
coreNote = do
  reserved "note"
  s <- stringLiteral
  e <- coreFullExp
  return $ Note s e
coreExternal = do
  reserved "external"
  -- TODO: This isn't in the grammar, but GHC
  -- always prints "external ccall". investigate...
  symbol "ccall"
  s <- stringLiteral
  t <- coreAty
  return $ External s t

coreLambdaBinds = many1 coreBind

coreBind = coreTbinding <|> coreVbind

coreTbinding = try coreAtTbind >>= (return . Tb)
coreVbind = parens (lambdaBind >>= (return . Vb))

coreAtTbind = (symbol "@") >> coreTbind

topVbind   = aCoreVbind coreQualifiedName
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
  lhs <- anIntLit
  maybeRhs <- optionMaybe (symbol "%" >> anIntLit)
  case maybeRhs of
    Nothing  -> return $ Lint lhs
    Just rhs -> return $ Lrational (lhs % rhs)

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
  tBinds  <- many (parens coreAtTbind)
  whiteSpace -- necessary b/c otherwise we parse the next list as empty
  vBinds  <- many (parens lambdaBind)
  whiteSpace
  try (symbol "->")
  rhs     <- try coreFullExp
  return $ Acon conName tBinds vBinds rhs

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
extCore = P.makeTokenParser extCoreDef

parens          = P.parens extCore    
braces          = P.braces extCore    
semiSep1        = P.semiSep1 extCore    
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
