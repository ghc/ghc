{-# LANGUAGE BangPatterns, StandaloneDeriving, FlexibleInstances, ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.GhcUtils
-- Copyright   :  (c) David Waern 2006-2009
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Utils for dealing with types from the GHC API
-----------------------------------------------------------------------------
module Haddock.GhcUtils where


import Control.Arrow
import Data.Char ( isSpace )

import Haddock.Types( DocNameI )

import Exception
import FV
import Outputable ( Outputable, panic, showPpr )
import Name
import NameSet
import Module
import HscTypes
import GHC
import Class
import DynFlags
import SrcLoc    ( advanceSrcLoc )
import Var       ( VarBndr(..), TyVarBinder, tyVarKind, updateTyVarKind,
                   isInvisibleArgFlag )
import VarSet    ( VarSet, emptyVarSet )
import VarEnv    ( TyVarEnv, extendVarEnv, elemVarEnv, emptyVarEnv )
import TyCoRep   ( Type(..), isRuntimeRepVar )
import TysWiredIn( liftedRepDataConTyCon )

import           StringBuffer ( StringBuffer )
import qualified StringBuffer             as S

import           Data.ByteString ( ByteString )
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BS


moduleString :: Module -> String
moduleString = moduleNameString . moduleName

isNameSym :: Name -> Bool
isNameSym = isSymOcc . nameOccName

getMainDeclBinder :: (SrcSpanLess (LPat p) ~ Pat p , HasSrcSpan (LPat p)) =>
                     HsDecl p -> [IdP p]
getMainDeclBinder (TyClD _ d) = [tcdName d]
getMainDeclBinder (ValD _ d) =
  case collectHsBindBinders d of
    []       -> []
    (name:_) -> [name]
getMainDeclBinder (SigD _ d) = sigNameNoLoc d
getMainDeclBinder (ForD _ (ForeignImport _ name _ _)) = [unLoc name]
getMainDeclBinder (ForD _ (ForeignExport _ _ _ _)) = []
getMainDeclBinder _ = []

-- Extract the source location where an instance is defined. This is used
-- to correlate InstDecls with their Instance/CoAxiom Names, via the
-- instanceMap.
getInstLoc :: InstDecl name -> SrcSpan
getInstLoc (ClsInstD _ (ClsInstDecl { cid_poly_ty = ty })) = getLoc (hsSigType ty)
getInstLoc (DataFamInstD _ (DataFamInstDecl
  { dfid_eqn = HsIB { hsib_body = FamEqn { feqn_tycon = L l _ }}})) = l
getInstLoc (TyFamInstD _ (TyFamInstDecl
  -- Since CoAxioms' Names refer to the whole line for type family instances
  -- in particular, we need to dig a bit deeper to pull out the entire
  -- equation. This does not happen for data family instances, for some reason.
  { tfid_eqn = HsIB { hsib_body = FamEqn { feqn_rhs = L l _ }}})) = l
getInstLoc (ClsInstD _ (XClsInstDecl _)) = panic "getInstLoc"
getInstLoc (DataFamInstD _ (DataFamInstDecl (HsIB _ (XFamEqn _)))) = panic "getInstLoc"
getInstLoc (TyFamInstD _ (TyFamInstDecl (HsIB _ (XFamEqn _)))) = panic "getInstLoc"
getInstLoc (XInstDecl _) = panic "getInstLoc"
getInstLoc (DataFamInstD _ (DataFamInstDecl (XHsImplicitBndrs _))) = panic "getInstLoc"
getInstLoc (TyFamInstD _ (TyFamInstDecl (XHsImplicitBndrs _))) = panic "getInstLoc"



-- Useful when there is a signature with multiple names, e.g.
--   foo, bar :: Types..
-- but only one of the names is exported and we have to change the
-- type signature to only include the exported names.
filterLSigNames :: (IdP (GhcPass p) -> Bool) -> LSig (GhcPass p) -> Maybe (LSig (GhcPass p))
filterLSigNames p (L loc sig) = L loc <$> (filterSigNames p sig)

filterSigNames :: (IdP (GhcPass p) -> Bool) -> Sig (GhcPass p) -> Maybe (Sig (GhcPass p))
filterSigNames p orig@(SpecSig _ n _ _)          = ifTrueJust (p $ unLoc n) orig
filterSigNames p orig@(InlineSig _ n _)          = ifTrueJust (p $ unLoc n) orig
filterSigNames p (FixSig _ (FixitySig _ ns ty)) =
  case filter (p . unLoc) ns of
    []       -> Nothing
    filtered -> Just (FixSig noExt (FixitySig noExt filtered ty))
filterSigNames _ orig@(MinimalSig _ _ _)      = Just orig
filterSigNames p (TypeSig _ ns ty) =
  case filter (p . unLoc) ns of
    []       -> Nothing
    filtered -> Just (TypeSig noExt filtered ty)
filterSigNames p (ClassOpSig _ is_default ns ty) =
  case filter (p . unLoc) ns of
    []       -> Nothing
    filtered -> Just (ClassOpSig noExt is_default filtered ty)
filterSigNames p (PatSynSig _ ns ty) =
  case filter (p . unLoc) ns of
    []       -> Nothing
    filtered -> Just (PatSynSig noExt filtered ty)
filterSigNames _ _                             = Nothing

ifTrueJust :: Bool -> name -> Maybe name
ifTrueJust True  = Just
ifTrueJust False = const Nothing

sigName :: LSig name -> [IdP name]
sigName (L _ sig) = sigNameNoLoc sig

sigNameNoLoc :: Sig name -> [IdP name]
sigNameNoLoc (TypeSig    _   ns _)         = map unLoc ns
sigNameNoLoc (ClassOpSig _ _ ns _)         = map unLoc ns
sigNameNoLoc (PatSynSig  _   ns _)         = map unLoc ns
sigNameNoLoc (SpecSig    _   n _ _)        = [unLoc n]
sigNameNoLoc (InlineSig  _   n _)          = [unLoc n]
sigNameNoLoc (FixSig _ (FixitySig _ ns _)) = map unLoc ns
sigNameNoLoc _                             = []

-- | Was this signature given by the user?
isUserLSig :: LSig name -> Bool
isUserLSig (L _(TypeSig {}))    = True
isUserLSig (L _(ClassOpSig {})) = True
isUserLSig (L _(PatSynSig {}))  = True
isUserLSig _                    = False


isClassD :: HsDecl a -> Bool
isClassD (TyClD _ d) = isClassDecl d
isClassD _ = False

isValD :: HsDecl a -> Bool
isValD (ValD _ _) = True
isValD _ = False

pretty :: Outputable a => DynFlags -> a -> String
pretty = showPpr

nubByName :: (a -> Name) -> [a] -> [a]
nubByName f ns = go emptyNameSet ns
  where
    go !_ [] = []
    go !s (x:xs)
      | y `elemNameSet` s = go s xs
      | otherwise         = let !s' = extendNameSet s y
                            in x : go s' xs
      where
        y = f x

-- ---------------------------------------------------------------------

-- This function is duplicated as getGADTConType and getGADTConTypeG,
-- as I can't get the types to line up otherwise. AZ.

getGADTConType :: ConDecl DocNameI -> LHsType DocNameI
-- The full type of a GADT data constructor We really only get this in
-- order to pretty-print it, and currently only in Haddock's code.  So
-- we are cavalier about locations and extensions, hence the
-- 'undefined's
getGADTConType (ConDeclGADT { con_forall = L _ has_forall
                            , con_qvars = qtvs
                            , con_mb_cxt = mcxt, con_args = args
                            , con_res_ty = res_ty })
 | has_forall = noLoc (HsForAllTy { hst_xforall = NoExt
                                  , hst_bndrs = hsQTvExplicit qtvs
                                  , hst_body  = theta_ty })
 | otherwise  = theta_ty
 where
   theta_ty | Just theta <- mcxt
            = noLoc (HsQualTy { hst_xqual = NoExt, hst_ctxt = theta, hst_body = tau_ty })
            | otherwise
            = tau_ty

   tau_ty = case args of
              RecCon flds -> noLoc (HsFunTy noExt (noLoc (HsRecTy noExt (unLoc flds))) res_ty)
              PrefixCon pos_args -> foldr mkFunTy res_ty pos_args
              InfixCon arg1 arg2 -> arg1 `mkFunTy` (arg2 `mkFunTy` res_ty)

   mkFunTy a b = noLoc (HsFunTy noExt a b)

getGADTConType (ConDeclH98 {}) = panic "getGADTConType"
  -- Should only be called on ConDeclGADT
getGADTConType (XConDecl {}) = panic "getGADTConType"

-- -------------------------------------

getGADTConTypeG :: ConDecl (GhcPass p) -> LHsType (GhcPass p)
-- The full type of a GADT data constructor We really only get this in
-- order to pretty-print it, and currently only in Haddock's code.  So
-- we are cavalier about locations and extensions, hence the
-- 'undefined's
getGADTConTypeG (ConDeclGADT { con_forall = L _ has_forall
                            , con_qvars = qtvs
                            , con_mb_cxt = mcxt, con_args = args
                            , con_res_ty = res_ty })
 | has_forall = noLoc (HsForAllTy { hst_xforall = NoExt
                                  , hst_bndrs = hsQTvExplicit qtvs
                                  , hst_body  = theta_ty })
 | otherwise  = theta_ty
 where
   theta_ty | Just theta <- mcxt
            = noLoc (HsQualTy { hst_xqual = NoExt, hst_ctxt = theta, hst_body = tau_ty })
            | otherwise
            = tau_ty

   tau_ty = case args of
              RecCon flds -> noLoc (HsFunTy noExt (noLoc (HsRecTy noExt (unLoc flds))) res_ty)
              PrefixCon pos_args -> foldr mkFunTy res_ty pos_args
              InfixCon arg1 arg2 -> arg1 `mkFunTy` (arg2 `mkFunTy` res_ty)

   mkFunTy a b = noLoc (HsFunTy noExt a b)

getGADTConTypeG (ConDeclH98 {}) = panic "getGADTConTypeG"
  -- Should only be called on ConDeclGADT
getGADTConTypeG (XConDecl {}) = panic "getGADTConTypeG"


-------------------------------------------------------------------------------
-- * Parenthesization
-------------------------------------------------------------------------------

-- | Precedence level (inside the 'HsType' AST).
data Precedence
  = PREC_TOP  -- ^ precedence of 'type' production in GHC's parser

  | PREC_SIG  -- ^ explicit type signature

  | PREC_CTX  -- ^ Used for single contexts, eg. ctx => type
              -- (as opposed to (ctx1, ctx2) => type)

  | PREC_FUN  -- ^ precedence of 'btype' production in GHC's parser
              -- (used for LH arg of (->))

  | PREC_OP   -- ^ arg of any infix operator
              -- (we don't keep have fixity info)

  | PREC_CON  -- ^ arg of type application: always parenthesize unless atomic
  deriving (Eq, Ord)

-- | Add in extra 'HsParTy' where needed to ensure that what would be printed
-- out using 'ppr' has enough parentheses to be re-parsed properly.
--
-- We cannot add parens that may be required by fixities because we do not have
-- any fixity information to work with in the first place :(.
reparenTypePrec :: (XParTy a ~ NoExt) => Precedence -> HsType a -> HsType a
reparenTypePrec = go
  where

  -- Shorter name for 'reparenType'
  go :: (XParTy a ~ NoExt) => Precedence -> HsType a -> HsType a
  go _ (HsBangTy x b ty)     = HsBangTy x b (reparenLType ty)
  go _ (HsTupleTy x con tys) = HsTupleTy x con (map reparenLType tys)
  go _ (HsSumTy x tys)       = HsSumTy x (map reparenLType tys)
  go _ (HsListTy x ty)       = HsListTy x (reparenLType ty)
  go _ (HsRecTy x flds)      = HsRecTy x (map (fmap reparenConDeclField) flds)
  go p (HsDocTy x ty d)      = HsDocTy x (goL p ty) d
  go _ (HsExplicitListTy x p tys) = HsExplicitListTy x p (map reparenLType tys)
  go _ (HsExplicitTupleTy x tys) = HsExplicitTupleTy x (map reparenLType tys)
  go p (HsKindSig x ty kind)
    = paren p PREC_SIG $ HsKindSig x (goL PREC_SIG ty) (goL PREC_SIG kind)
  go p (HsIParamTy x n ty)
    = paren p PREC_SIG $ HsIParamTy x n (reparenLType ty)
  go p (HsForAllTy x tvs ty)
    = paren p PREC_CTX $ HsForAllTy x (map (fmap reparenTyVar) tvs) (reparenLType ty)
  go p (HsQualTy x ctxt ty)
    = let p' [_] = PREC_CTX
          p' _   = PREC_TOP -- parens will get added anyways later...
      in paren p PREC_CTX $ HsQualTy x (fmap (\xs -> map (goL (p' xs)) xs) ctxt) (goL PREC_TOP ty)
  go p (HsFunTy x ty1 ty2)
    = paren p PREC_FUN $ HsFunTy x (goL PREC_FUN ty1) (goL PREC_TOP ty2)
  go p (HsAppTy x fun_ty arg_ty)
    = paren p PREC_CON $ HsAppTy x (goL PREC_FUN fun_ty) (goL PREC_CON arg_ty)
  go p (HsAppKindTy x fun_ty arg_ki)
    = paren p PREC_CON $ HsAppKindTy x (goL PREC_FUN fun_ty) (goL PREC_CON arg_ki)
  go p (HsOpTy x ty1 op ty2)
    = paren p PREC_FUN $ HsOpTy x (goL PREC_OP ty1) op (goL PREC_OP ty2)
  go p (HsParTy _ t) = unLoc $ goL p t -- pretend the paren doesn't exist - it will be added back if needed
  go _ t@HsTyVar{} = t
  go _ t@HsStarTy{} = t
  go _ t@HsSpliceTy{} = t
  go _ t@HsTyLit{} = t
  go _ t@HsWildCardTy{} = t
  go _ t@XHsType{} = t

  -- Located variant of 'go'
  goL :: (XParTy a ~ NoExt) => Precedence -> LHsType a -> LHsType a
  goL ctxt_prec = fmap (go ctxt_prec)

  -- Optionally wrap a type in parens
  paren :: (XParTy a ~ NoExt)
        => Precedence            -- Precedence of context
        -> Precedence            -- Precedence of top-level operator
        -> HsType a -> HsType a  -- Wrap in parens if (ctxt >= op)
  paren ctxt_prec op_prec | ctxt_prec >= op_prec = HsParTy NoExt . noLoc
                          | otherwise            = id


-- | Add parenthesis around the types in a 'HsType' (see 'reparenTypePrec')
reparenType :: (XParTy a ~ NoExt) => HsType a -> HsType a
reparenType = reparenTypePrec PREC_TOP

-- | Add parenthesis around the types in a 'LHsType' (see 'reparenTypePrec')
reparenLType :: (XParTy a ~ NoExt) => LHsType a -> LHsType a
reparenLType = fmap reparenType

-- | Add parenthesis around the types in a 'HsTyVarBndr' (see 'reparenTypePrec')
reparenTyVar :: (XParTy a ~ NoExt) => HsTyVarBndr a -> HsTyVarBndr a
reparenTyVar (UserTyVar x n) = UserTyVar x n
reparenTyVar (KindedTyVar x n kind) = KindedTyVar x n (reparenLType kind)
reparenTyVar v@XTyVarBndr{} = v

-- | Add parenthesis around the types in a 'ConDeclField' (see 'reparenTypePrec')
reparenConDeclField :: (XParTy a ~ NoExt) => ConDeclField a -> ConDeclField a
reparenConDeclField (ConDeclField x n t d) = ConDeclField x n (reparenLType t) d
reparenConDeclField c@XConDeclField{} = c


-------------------------------------------------------------------------------
-- * Located
-------------------------------------------------------------------------------


unL :: Located a -> a
unL (L _ x) = x


reL :: a -> Located a
reL = L undefined

-------------------------------------------------------------------------------
-- * NamedThing instances
-------------------------------------------------------------------------------


instance NamedThing (TyClDecl GhcRn) where
  getName = tcdName

-------------------------------------------------------------------------------
-- * Subordinates
-------------------------------------------------------------------------------


class Parent a where
  children :: a -> [Name]


instance Parent (ConDecl GhcRn) where
  children con =
    case con_args con of
      RecCon fields -> map (extFieldOcc . unL) $
                         concatMap (cd_fld_names . unL) (unL fields)
      _             -> []

instance Parent (TyClDecl GhcRn) where
  children d
    | isDataDecl  d = map unL $ concatMap (getConNames . unL)
                              $ (dd_cons . tcdDataDefn) $ d
    | isClassDecl d =
        map (unL . fdLName . unL) (tcdATs d) ++
        [ unL n | L _ (TypeSig _ ns _) <- tcdSigs d, n <- ns ]
    | otherwise = []


-- | A parent and its children
family :: (NamedThing a, Parent a) => a -> (Name, [Name])
family = getName &&& children


familyConDecl :: ConDecl GHC.GhcRn -> [(Name, [Name])]
familyConDecl d = zip (map unL (getConNames d)) (repeat $ children d)

-- | A mapping from the parent (main-binder) to its children and from each
-- child to its grand-children, recursively.
families :: TyClDecl GhcRn -> [(Name, [Name])]
families d
  | isDataDecl  d = family d : concatMap (familyConDecl . unL) (dd_cons (tcdDataDefn d))
  | isClassDecl d = [family d]
  | otherwise     = []


-- | A mapping from child to parent
parentMap :: TyClDecl GhcRn -> [(Name, Name)]
parentMap d = [ (c, p) | (p, cs) <- families d, c <- cs ]


-- | The parents of a subordinate in a declaration
parents :: Name -> HsDecl GhcRn -> [Name]
parents n (TyClD _ d) = [ p | (c, p) <- parentMap d, c == n ]
parents _ _ = []


-------------------------------------------------------------------------------
-- * Utils that work in monads defined by GHC
-------------------------------------------------------------------------------


modifySessionDynFlags :: (DynFlags -> DynFlags) -> Ghc ()
modifySessionDynFlags f = do
  dflags <- getSessionDynFlags
  _ <- setSessionDynFlags (f dflags)
  return ()


-- | A variant of 'gbracket' where the return value from the first computation
-- is not required.
gbracket_ :: ExceptionMonad m => m a -> m b -> m c -> m c
gbracket_ before_ after thing = gbracket before_ (const after) (const thing)

-- Extract the minimal complete definition of a Name, if one exists
minimalDef :: GhcMonad m => Name -> m (Maybe ClassMinimalDef)
minimalDef n = do
  mty <- lookupGlobalName n
  case mty of
    Just (ATyCon (tyConClass_maybe -> Just c)) -> return . Just $ classMinimalDef c
    _ -> return Nothing

-------------------------------------------------------------------------------
-- * DynFlags
-------------------------------------------------------------------------------


setObjectDir, setHiDir, setHieDir, setStubDir, setOutputDir :: String -> DynFlags -> DynFlags
setObjectDir  f d = d{ objectDir  = Just f}
setHiDir      f d = d{ hiDir      = Just f}
setHieDir     f d = d{ hieDir     = Just f}
setStubDir    f d = d{ stubDir    = Just f
                     , includePaths = addGlobalInclude (includePaths d) [f] }
  -- -stubdir D adds an implicit -I D, so that gcc can find the _stub.h file
  -- \#included from the .hc file when compiling with -fvia-C.
setOutputDir  f = setObjectDir f . setHiDir f . setHieDir f . setStubDir f


-------------------------------------------------------------------------------
-- * 'StringBuffer' and 'ByteString'
-------------------------------------------------------------------------------
-- We get away with a bunch of these functions because 'StringBuffer' and
-- 'ByteString' have almost exactly the same structure.

-- | Convert a UTF-8 encoded 'ByteString' into a 'StringBuffer. This really
-- relies on the internals of both 'ByteString' and 'StringBuffer'.
--
-- /O(n)/ (but optimized into a @memcpy@ by @bytestring@ under the hood)
stringBufferFromByteString :: ByteString -> StringBuffer
stringBufferFromByteString bs =
  let BS.PS fp off len = bs <> BS.pack [0,0,0]
  in S.StringBuffer { S.buf = fp, S.len = len - 3, S.cur = off }

-- | Take the first @n@ /bytes/ of the 'StringBuffer' and put them in a
-- 'ByteString'.
--
-- /O(1)/
takeStringBuffer :: Int -> StringBuffer -> ByteString
takeStringBuffer !n !(S.StringBuffer fp _ cur) = BS.PS fp cur n

-- | Return the prefix of the first 'StringBuffer' that /isn't/ in the second
-- 'StringBuffer'. **The behavior is undefined if the 'StringBuffers' use
-- separate buffers.**
--
-- /O(1)/
splitStringBuffer :: StringBuffer -> StringBuffer -> ByteString
splitStringBuffer buf1 buf2 = takeStringBuffer n buf1
  where n = S.byteDiff buf1 buf2

-- | Split the 'StringBuffer' at the next newline (or the end of the buffer).
-- Also: initial position is passed in and the updated position is returned.
--
-- /O(n)/ (but /O(1)/ space)
spanLine :: RealSrcLoc -> StringBuffer -> (ByteString, RealSrcLoc, StringBuffer)
spanLine !loc !buf = go loc buf
  where

  go !l !b
    | not (S.atEnd b)
    = case S.nextChar b of
        ('\n', b') -> (splitStringBuffer buf b', advanceSrcLoc l '\n', b')
        (c,    b') -> go (advanceSrcLoc l c) b'
    | otherwise
    = (splitStringBuffer buf b, advanceSrcLoc l '\n', b)

-- | Given a start position and a buffer with that start position, split the
-- buffer at an end position.
--
-- /O(n)/ (but /O(1)/ space)
spanPosition :: RealSrcLoc   -- ^ start of buffeer
             -> RealSrcLoc   -- ^ position until which to take
             -> StringBuffer -- ^ buffer from which to take
             -> (ByteString, StringBuffer)
spanPosition !start !end !buf = go start buf
  where

  go !l !b
    | l < end
    , not (S.atEnd b)
    , (c, b') <- S.nextChar b
    = go (advanceSrcLoc l c) b'
    | otherwise
    = (splitStringBuffer buf b, b)

-- | Try to parse a line of CPP from the from of the buffer. A \"line\" of CPP
-- consists of
--
--   * at most 10 whitespace characters, including at least one newline
--   * a @#@ character
--   * keep parsing lines until you find a line not ending in @\\@.
--
-- This is chock full of heuristics about what a line of CPP is.
--
-- /O(n)/ (but /O(1)/ space)
tryCppLine :: RealSrcLoc -> StringBuffer -> Maybe (ByteString, RealSrcLoc, StringBuffer)
tryCppLine !loc !buf = spanSpace (S.prevChar buf '\n' == '\n') loc buf
  where

  -- Keep consuming space characters until we hit either a @#@ or something
  -- else. If we hit a @#@, start parsing CPP
  spanSpace !seenNl !l !b
    | S.atEnd b
    = Nothing
    | otherwise
    = case S.nextChar b of
        ('#' , b') | not (S.atEnd b')
                   , ('-', b'') <- S.nextChar b'
                   , ('}', _) <- S.nextChar b''
                   -> Nothing -- Edge case exception for @#-}@
                   | seenNl
                   -> Just (spanCppLine (advanceSrcLoc l '#') b') -- parse CPP
                   | otherwise
                   -> Nothing -- We didn't see a newline, so this can't be CPP!

        (c   , b') | isSpace c -> spanSpace (seenNl || c == '\n')
                                            (advanceSrcLoc l c) b'
                   | otherwise -> Nothing

  -- Consume a CPP line to its "end" (basically the first line that ends not
  -- with a @\@ character)
  spanCppLine !l !b
    | S.atEnd b
    = (splitStringBuffer buf b, l, b)
    | otherwise
    = case S.nextChar b of
        ('\\', b') | not (S.atEnd b')
                   , ('\n', b'') <- S.nextChar b'
                   -> spanCppLine (advanceSrcLoc (advanceSrcLoc l '\\') '\n') b''

        ('\n', b') -> (splitStringBuffer buf b', advanceSrcLoc l '\n', b')

        (c   , b') -> spanCppLine (advanceSrcLoc l c) b'

-------------------------------------------------------------------------------
-- * Free variables of a 'Type'
-------------------------------------------------------------------------------

-- | Get free type variables in a 'Type' in their order of appearance.
-- See [Ordering of implicit variables].
orderedFVs
  :: VarSet  -- ^ free variables to ignore 
  -> [Type]  -- ^ types to traverse (in order) looking for free variables
  -> [TyVar] -- ^ free type variables, in the order they appear in
orderedFVs vs tys =
  reverse . fst $ tyCoFVsOfTypes' tys (const True) vs ([], emptyVarSet)


-- See the "Free variables of types and coercions" section in 'TyCoRep', or
-- check out Note [Free variables of types]. The functions in this section
-- don't output type variables in the order they first appear in in the 'Type'.
--
-- For example, 'tyCoVarsOfTypeList' reports an incorrect order for the type
-- of 'const :: a -> b -> a':
--
-- >>> import Name 
-- >>> import TyCoRep
-- >>> import TysPrim
-- >>> import Var
-- >>> a = TyVarTy alphaTyVar
-- >>> b = TyVarTy betaTyVar
-- >>> constTy = mkFunTys [a, b] a
-- >>> map (getOccString . tyVarName) (tyCoVarsOfTypeList constTy)
-- ["b","a"]
--
-- However, we want to reuse the very optimized traversal machinery there, so
-- so we make our own `tyCoFVsOfType'`, `tyCoFVsBndr'`, and `tyCoVarsOfTypes'`.
-- All these do differently is traverse in a different order and ignore
-- coercion variables.

-- | Just like 'tyCoFVsOfType', but traverses type variables in reverse order
-- of  appearance.
tyCoFVsOfType' :: Type -> FV
tyCoFVsOfType' (TyVarTy v)        a b c = (FV.unitFV v `unionFV` tyCoFVsOfType' (tyVarKind v)) a b c
tyCoFVsOfType' (TyConApp _ tys)   a b c = tyCoFVsOfTypes' tys a b c
tyCoFVsOfType' (LitTy {})         a b c = emptyFV a b c
tyCoFVsOfType' (AppTy fun arg)    a b c = (tyCoFVsOfType' arg `unionFV` tyCoFVsOfType' fun) a b c
tyCoFVsOfType' (FunTy arg res)    a b c = (tyCoFVsOfType' res `unionFV` tyCoFVsOfType' arg) a b c
tyCoFVsOfType' (ForAllTy bndr ty) a b c = tyCoFVsBndr' bndr (tyCoFVsOfType' ty)  a b c
tyCoFVsOfType' (CastTy ty _)      a b c = (tyCoFVsOfType' ty) a b c
tyCoFVsOfType' (CoercionTy _ )    a b c = emptyFV a b c

-- | Just like 'tyCoFVsOfTypes', but traverses type variables in reverse order
-- of appearance.
tyCoFVsOfTypes' :: [Type] -> FV
tyCoFVsOfTypes' (ty:tys) fv_cand in_scope acc = (tyCoFVsOfTypes' tys `unionFV` tyCoFVsOfType' ty) fv_cand in_scope acc
tyCoFVsOfTypes' []       fv_cand in_scope acc = emptyFV fv_cand in_scope acc

-- | Just like 'tyCoFVsBndr', but traverses type variables in reverse order of
-- appearance.
tyCoFVsBndr' :: TyVarBinder -> FV -> FV
tyCoFVsBndr' (Bndr tv _) fvs = FV.delFV tv fvs `unionFV` tyCoFVsOfType' (tyVarKind tv)


-------------------------------------------------------------------------------
-- * Defaulting RuntimeRep variables
-------------------------------------------------------------------------------

-- | Traverses the type, defaulting type variables of kind 'RuntimeRep' to
-- 'LiftedType'. See 'defaultRuntimeRepVars' in IfaceType.hs the original such
-- function working over `IfaceType`'s.
defaultRuntimeRepVars :: Type -> Type
defaultRuntimeRepVars = go emptyVarEnv
  where
    go :: TyVarEnv () -> Type -> Type
    go subs (ForAllTy (Bndr var flg) ty)
      | isRuntimeRepVar var
      , isInvisibleArgFlag flg
      = let subs' = extendVarEnv subs var ()
        in go subs' ty
      | otherwise
      = ForAllTy (Bndr (updateTyVarKind (go subs) var) flg)
                 (go subs ty)

    go subs (TyVarTy tv)
      | tv `elemVarEnv` subs
      = TyConApp liftedRepDataConTyCon []
      | otherwise
      = TyVarTy (updateTyVarKind (go subs) tv)

    go subs (TyConApp tc tc_args)
      = TyConApp tc (map (go subs) tc_args)

    go subs (FunTy arg res)
      = FunTy (go subs arg) (go subs res)

    go subs (AppTy t u)
      = AppTy (go subs t) (go subs u)

    go subs (CastTy x co)
      = CastTy (go subs x) co

    go _ ty@(LitTy {}) = ty
    go _ ty@(CoercionTy {}) = ty

