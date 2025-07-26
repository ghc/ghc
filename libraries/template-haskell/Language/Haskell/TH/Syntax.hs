{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE UnboxedTuples #-}

module Language.Haskell.TH.Syntax (
    Quote (..),
    Exp (..),
    Match (..),
    Clause (..),
    Q (..),
    Pat (..),
    Stmt (..),
    Con (..),
    Type (..),
    Dec (..),
    BangType,
    VarBangType,
    FieldExp,
    FieldPat,
    Name (..),
    FunDep (..),
    Pred,
    RuleBndr (..),
    TySynEqn (..),
    InjectivityAnn (..),
    Kind,
    Overlap (..),
    DerivClause (..),
    DerivStrategy (..),
    Code (..),
    ModName (..),
    addCorePlugin,
    addDependentFile,
    addForeignFile,
    addForeignFilePath,
    addForeignSource,
    addModFinalizer,
    addTempFile,
    addTopDecls,
    badIO,
    bindCode,
    bindCode_,
    cmpEq,
    compareBytes,
    counter,
    defaultFixity,
    eqBytes,
    extsEnabled,
    getDoc,
    getPackageRoot,
    getQ,
    get_cons_names,
    hoistCode,
    isExtEnabled,
    isInstance,
    joinCode,
    liftCode,
    location,
    lookupName,
    lookupTypeName,
    lookupValueName,
    manyName,
    maxPrecedence,
    memcmp,
    mkNameG,
    mkNameU,
    mkOccName,
    mkPkgName,
    mk_tup_name,
    mkName,
    mkNameG_v,
    mkNameG_d,
    mkNameG_tc,
    mkNameL,
    mkNameS,
    unTypeCode,
    mkModName,
    unsafeCodeCoerce,
    mkNameQ,
    mkNameG_fld,
    modString,
    nameBase,
    nameModule,
    namePackage,
    nameSpace,
    newDeclarationGroup,
    newNameIO,
    occString,
    oneName,
    pkgString,
    putDoc,
    putQ,
    recover,
    reify,
    reifyAnnotations,
    reifyConStrictness,
    reifyFixity,
    reifyInstances,
    reifyModule,
    reifyRoles,
    reifyType,
    report,
    reportError,
    reportWarning,
    runIO,
    sequenceQ,
    runQ,
    showName,
    showName',
    thenCmp,
    tupleDataName,
    tupleTypeName,
    unTypeQ,
    unboxedSumDataName,
    unboxedSumTypeName,
    unboxedTupleDataName,
    unboxedTupleTypeName,
    unsafeTExpCoerce,
    ForeignSrcLang (..),
    Extension (..),
    AnnLookup (..),
    AnnTarget (..),
    Arity,
    Bang (..),
    BndrVis (..),
    Body (..),
    Bytes (..),
    Callconv (..),
    CharPos,
    Cxt,
    DecidedStrictness (..),
    DocLoc (..),
    FamilyResultSig (..),
    Fixity (..),
    FixityDirection (..),
    Foreign (..),
    Guard (..),
    Info (..),
    Inline (..),
    InstanceDec,
    Lit (..),
    Loc (..),
    Module (..),
    ModuleInfo (..),
    NameFlavour (..),
    NameIs (..),
    NameSpace (..),
    NamespaceSpecifier (..),
    OccName (..),
    ParentName,
    PatSynArgs (..),
    PatSynDir (..),
    PatSynType,
    Phases (..),
    PkgName (..),
    Pragma (..),
    Quasi (..),
    Range (..),
    Role (..),
    RuleMatch (..),
    Safety (..),
    SourceStrictness (..),
    SourceUnpackedness (..),
    Specificity (..),
    Strict,
    StrictType,
    SumAlt,
    SumArity,
    TExp (..),
    TyLit (..),
    TyVarBndr (..),
    TypeFamilyHead (..),
    Uniq,
    Unlifted,
    VarStrictType,
    makeRelativeToProject,
    liftString,
    Lift (..),
    dataToCodeQ,
    dataToExpQ,
    dataToPatQ,
    dataToQa,
    falseName,
    justName,
    leftName,
    liftData,
    liftDataTyped,
    nonemptyName,
    nothingName,
    rightName,
    trueName,
    -- * Notes
    -- ** Unresolved Infix
    -- $infix
)
where

import GHC.Boot.TH.Lift
import GHC.Boot.TH.Syntax
import System.FilePath
import Data.Data hiding (Fixity(..))
import Data.List.NonEmpty (NonEmpty(..))
import GHC.Lexeme ( startsVarSym, startsVarId )

-- This module completely re-exports 'GHC.Boot.TH.Syntax',
-- and exports additionally functions that depend on filepath.

-- |
addForeignFile :: ForeignSrcLang -> String -> Q ()
addForeignFile = addForeignSource
{-# DEPRECATED addForeignFile
               "Use 'Language.Haskell.TH.Syntax.addForeignSource' instead"
  #-} -- deprecated in 8.6

-- | The input is a filepath, which if relative is offset by the package root.
makeRelativeToProject :: FilePath -> Q FilePath
makeRelativeToProject fp | isRelative fp = do
  root <- getPackageRoot
  return (root </> fp)
makeRelativeToProject fp = return fp

trueName, falseName :: Name
trueName  = 'True
falseName = 'False

nothingName, justName :: Name
nothingName = 'Nothing
justName    = 'Just

leftName, rightName :: Name
leftName  = 'Left
rightName = 'Right

nonemptyName :: Name
nonemptyName = '(:|)

-----------------------------------------------------
--
--              Generic Lift implementations
--
-----------------------------------------------------

-- | 'dataToQa' is an internal utility function for constructing generic
-- conversion functions from types with 'Data' instances to various
-- quasi-quoting representations.  See the source of 'dataToExpQ' and
-- 'dataToPatQ' for two example usages: @mkCon@, @mkLit@
-- and @appQ@ are overloadable to account for different syntax for
-- expressions and patterns; @antiQ@ allows you to override type-specific
-- cases, a common usage is just @const Nothing@, which results in
-- no overloading.
dataToQa  ::  forall m a k q. (Quote m, Data a)
          =>  (Name -> k)
          ->  (Lit -> m q)
          ->  (k -> [m q] -> m q)
          ->  (forall b . Data b => b -> Maybe (m q))
          ->  a
          ->  m q
dataToQa mkCon mkLit appCon antiQ t =
    case antiQ t of
      Nothing ->
          case constrRep constr of
            AlgConstr _ ->
                appCon (mkCon funOrConName) conArgs
              where
                funOrConName :: Name
                funOrConName =
                    case showConstr constr of
                      "(:)"       -> Name (mkOccName ":")
                                          (NameG DataName
                                                (mkPkgName "ghc-internal")
                                                (mkModName "GHC.Internal.Types"))
                      con@"[]"    -> Name (mkOccName con)
                                          (NameG DataName
                                                (mkPkgName "ghc-internal")
                                                (mkModName "GHC.Internal.Types"))
                      con@('(':_) -> Name (mkOccName con)
                                          (NameG DataName
                                                (mkPkgName "ghc-internal")
                                                (mkModName "GHC.Internal.Tuple"))

                      -- Tricky case: see Note [Data for non-algebraic types]
                      fun@(x:_)   | startsVarSym x || startsVarId x
                                  -> mkNameG_v tyconPkg tyconMod fun
                      con         -> mkNameG_d tyconPkg tyconMod con

                  where
                    tycon :: TyCon
                    tycon = (typeRepTyCon . typeOf) t

                    tyconPkg, tyconMod :: String
                    tyconPkg = tyConPackage tycon
                    tyconMod = tyConModule  tycon

                conArgs :: [m q]
                conArgs = gmapQ (dataToQa mkCon mkLit appCon antiQ) t
            IntConstr n ->
                mkLit $ IntegerL n
            FloatConstr n ->
                mkLit $ RationalL n
            CharConstr c ->
                mkLit $ CharL c
        where
          constr :: Constr
          constr = toConstr t

      Just y -> y


{- Note [Data for non-algebraic types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Class Data was originally intended for algebraic data types.  But
it is possible to use it for abstract types too.  For example, in
package `text` we find

  instance Data Text where
    ...
    toConstr _ = packConstr

  packConstr :: Constr
  packConstr = mkConstr textDataType "pack" [] Prefix

Here `packConstr` isn't a real data constructor, it's an ordinary
function.  Two complications

* In such a case, we must take care to build the Name using
  mkNameG_v (for values), not mkNameG_d (for data constructors).
  See #10796.

* The pseudo-constructor is named only by its string, here "pack".
  But 'dataToQa' needs the TyCon of its defining module, and has
  to assume it's defined in the same module as the TyCon itself.
  But nothing enforces that; #12596 shows what goes wrong if
  "pack" is defined in a different module than the data type "Text".
  -}

-- | A typed variant of 'dataToExpQ'.
dataToCodeQ :: (Quote m, Data a)
            => (forall b . Data b => b -> Maybe (Code m b))
            ->                       a ->        Code m a
dataToCodeQ f = unsafeCodeCoerce . dataToExpQ (fmap unTypeCode . f)

-- | 'dataToExpQ' converts a value to a 'Exp' representation of the
-- same value, in the SYB style. It is generalized to take a function
-- override type-specific cases; see 'liftData' for a more commonly
-- used variant.
dataToExpQ  ::  (Quote m, Data a)
            =>  (forall b . Data b => b -> Maybe (m Exp))
            ->  a
            ->  m Exp
dataToExpQ = dataToQa varOrConE litE (foldl appE)
    where
          -- Make sure that VarE is used if the Constr value relies on a
          -- function underneath the surface (instead of a constructor).
          -- See #10796.
          varOrConE s =
            case nameSpace s of
                 Just VarName      -> return (VarE s)
                 Just (FldName {}) -> return (VarE s)
                 Just DataName     -> return (ConE s)
                 _ -> error $ "Can't construct an expression from name "
                           ++ showName s
          appE x y = do { a <- x; b <- y; return (AppE a b)}
          litE c = return (LitE c)

-- | A typed variant of 'liftData'.
liftDataTyped :: (Quote m, Data a) => a -> Code m a
liftDataTyped = dataToCodeQ (const Nothing)

-- | 'liftData' is a variant of 'lift' in the 'Lift' type class which
-- works for any type with a 'Data' instance.
liftData :: (Quote m, Data a) => a -> m Exp
liftData = dataToExpQ (const Nothing)

-- | 'dataToPatQ' converts a value to a 'Pat' representation of the same
-- value, in the SYB style. It takes a function to handle type-specific cases,
-- alternatively, pass @const Nothing@ to get default behavior.
dataToPatQ  ::  (Quote m, Data a)
            =>  (forall b . Data b => b -> Maybe (m Pat))
            ->  a
            ->  m Pat
dataToPatQ = dataToQa id litP conP
    where litP l = return (LitP l)
          conP n ps =
            case nameSpace n of
                Just DataName -> do
                    ps' <- sequence ps
                    return (ConP n [] ps')
                _ -> error $ "Can't construct a pattern from name "
                          ++ showName n

{-
Note [Unresolved infix]
~~~~~~~~~~~~~~~~~~~~~~~
-}
{- $infix #infix#

When implementing antiquotation for quasiquoters, one often wants
to parse strings into expressions:

> parse :: String -> Maybe Exp

But how should we parse @a + b * c@? If we don't know the fixities of
@+@ and @*@, we don't know whether to parse it as @a + (b * c)@ or @(a
+ b) * c@.

In cases like this, use 'UInfixE', 'UInfixP', 'UInfixT', or 'PromotedUInfixT',
which stand for \"unresolved infix expression / pattern / type / promoted
constructor\", respectively. When the compiler is given a splice containing a
tree of @UInfixE@ applications such as

> UInfixE
>   (UInfixE e1 op1 e2)
>   op2
>   (UInfixE e3 op3 e4)

it will look up and the fixities of the relevant operators and
reassociate the tree as necessary.

  * trees will not be reassociated across 'ParensE', 'ParensP', or 'ParensT',
    which are of use for parsing expressions like

    > (a + b * c) + d * e

  * 'InfixE', 'InfixP', 'InfixT', and 'PromotedInfixT' expressions are never
    reassociated.

  * The 'UInfixE' constructor doesn't support sections. Sections
    such as @(a *)@ have no ambiguity, so 'InfixE' suffices. For longer
    sections such as @(a + b * c -)@, use an 'InfixE' constructor for the
    outer-most section, and use 'UInfixE' constructors for all
    other operators:

    > InfixE
    >   Just (UInfixE ...a + b * c...)
    >   op
    >   Nothing

    Sections such as @(a + b +)@ and @((a + b) +)@ should be rendered
    into 'Exp's differently:

    > (+ a + b)   ---> InfixE Nothing + (Just $ UInfixE a + b)
    >                    -- will result in a fixity error if (+) is left-infix
    > (+ (a + b)) ---> InfixE Nothing + (Just $ ParensE $ UInfixE a + b)
    >                    -- no fixity errors

  * Quoted expressions such as

    > [| a * b + c |] :: Q Exp
    > [p| a : b : c |] :: Q Pat
    > [t| T + T |] :: Q Type

    will never contain 'UInfixE', 'UInfixP', 'UInfixT', 'PromotedUInfixT',
    'InfixT', 'PromotedInfixT, 'ParensE', 'ParensP', or 'ParensT' constructors.

-}
