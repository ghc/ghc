module Distribution.Solver.Modular.IndexConversion
    ( convPIs
    ) where

import Data.List as L
import Data.Map as M
import Data.Maybe
import Data.Monoid as Mon
import Data.Set as S

import Distribution.Compiler
import Distribution.InstalledPackageInfo as IPI
import Distribution.Package                          -- from Cabal
import Distribution.Simple.BuildToolDepends          -- from Cabal
import Distribution.Types.ExeDependency              -- from Cabal
import Distribution.Types.PkgconfigDependency        -- from Cabal
import Distribution.Types.ComponentName              -- from Cabal
import Distribution.Types.UnqualComponentName        -- from Cabal
import Distribution.Types.CondTree                   -- from Cabal
import Distribution.Types.MungedPackageId            -- from Cabal
import Distribution.Types.MungedPackageName          -- from Cabal
import Distribution.PackageDescription as PD         -- from Cabal
import Distribution.PackageDescription.Configuration as PDC
import qualified Distribution.Simple.PackageIndex as SI
import Distribution.System
import Distribution.Types.ForeignLib

import           Distribution.Solver.Types.ComponentDeps
                   ( Component(..), componentNameToComponent )
import           Distribution.Solver.Types.Flag
import           Distribution.Solver.Types.OptionalStanza
import qualified Distribution.Solver.Types.PackageIndex as CI
import           Distribution.Solver.Types.Settings
import           Distribution.Solver.Types.SourcePackage

import Distribution.Solver.Modular.Dependency as D
import Distribution.Solver.Modular.Flag as F
import Distribution.Solver.Modular.Index
import Distribution.Solver.Modular.Package
import Distribution.Solver.Modular.Tree
import Distribution.Solver.Modular.Version

-- | Convert both the installed package index and the source package
-- index into one uniform solver index.
--
-- We use 'allPackagesBySourcePackageId' for the installed package index
-- because that returns us several instances of the same package and version
-- in order of preference. This allows us in principle to \"shadow\"
-- packages if there are several installed packages of the same version.
-- There are currently some shortcomings in both GHC and Cabal in
-- resolving these situations. However, the right thing to do is to
-- fix the problem there, so for now, shadowing is only activated if
-- explicitly requested.
convPIs :: OS -> Arch -> CompilerInfo -> ShadowPkgs -> StrongFlags -> SolveExecutables ->
           SI.InstalledPackageIndex -> CI.PackageIndex (SourcePackage loc) -> Index
convPIs os arch comp sip strfl solveExes iidx sidx =
  mkIndex (convIPI' sip iidx ++ convSPI' os arch comp strfl solveExes sidx)

-- | Convert a Cabal installed package index to the simpler,
-- more uniform index format of the solver.
convIPI' :: ShadowPkgs -> SI.InstalledPackageIndex -> [(PN, I, PInfo)]
convIPI' (ShadowPkgs sip) idx =
    -- apply shadowing whenever there are multiple installed packages with
    -- the same version
    [ maybeShadow (convIP idx pkg)
    -- IMPORTANT to get internal libraries. See
    -- Note [Index conversion with internal libraries]
    | (_, pkgs) <- SI.allPackagesBySourcePackageIdAndLibName idx
    , (maybeShadow, pkg) <- zip (id : repeat shadow) pkgs ]
  where

    -- shadowing is recorded in the package info
    shadow (pn, i, PInfo fdeps fds _) | sip = (pn, i, PInfo fdeps fds (Just Shadowed))
    shadow x                                = x

-- | Extract/recover the the package ID from an installed package info, and convert it to a solver's I.
convId :: InstalledPackageInfo -> (PN, I)
convId ipi = (pn, I ver $ Inst $ IPI.installedUnitId ipi)
  where MungedPackageId mpn ver = mungedId ipi
        -- HACK. See Note [Index conversion with internal libraries]
        pn = mkPackageName (unMungedPackageName mpn)

-- | Convert a single installed package into the solver-specific format.
convIP :: SI.InstalledPackageIndex -> InstalledPackageInfo -> (PN, I, PInfo)
convIP idx ipi =
  case mapM (convIPId (DependencyReason pn [] []) comp idx) (IPI.depends ipi) of
        Nothing  -> (pn, i, PInfo []  M.empty (Just Broken))
        Just fds -> (pn, i, PInfo fds M.empty Nothing)
 where
  (pn, i) = convId ipi
  -- 'sourceLibName' is unreliable, but for now we only really use this for
  -- primary libs anyways
  comp = componentNameToComponent $ libraryComponentName $ sourceLibName ipi
-- TODO: Installed packages should also store their encapsulations!

-- Note [Index conversion with internal libraries]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Something very interesting happens when we have internal libraries
-- in our index.  In this case, we maybe have p-0.1, which itself
-- depends on the internal library p-internal ALSO from p-0.1.
-- Here's the danger:
--
--      - If we treat both of these packages as having PN "p",
--        then the solver will try to pick one or the other,
--        but never both.
--
--      - If we drop the internal packages, now p-0.1 has a
--        dangling dependency on an "installed" package we know
--        nothing about. Oops.
--
-- An expedient hack is to put p-internal into cabal-install's
-- index as a MUNGED package name, so that it doesn't conflict
-- with anyone else (except other instances of itself).  But
-- yet, we ought NOT to say that PNs in the solver are munged
-- package names, because they're not; for source packages,
-- we really will never see munged package names.
--
-- The tension here is that the installed package index is actually
-- per library, but the solver is per package.  We need to smooth
-- it over, and munging the package names is a pretty good way to
-- do it.

-- | Convert dependencies specified by an installed package id into
-- flagged dependencies of the solver.
--
-- May return Nothing if the package can't be found in the index. That
-- indicates that the original package having this dependency is broken
-- and should be ignored.
convIPId :: DependencyReason PN -> Component -> SI.InstalledPackageIndex -> UnitId -> Maybe (FlaggedDep PN)
convIPId dr comp idx ipid =
  case SI.lookupUnitId idx ipid of
    Nothing  -> Nothing
    Just ipi -> let (pn, i) = convId ipi
                in  Just (D.Simple (LDep dr (Dep (IsExe False) pn (Fixed i))) comp)
                -- NB: something we pick up from the
                -- InstalledPackageIndex is NEVER an executable

-- | Convert a cabal-install source package index to the simpler,
-- more uniform index format of the solver.
convSPI' :: OS -> Arch -> CompilerInfo -> StrongFlags -> SolveExecutables ->
            CI.PackageIndex (SourcePackage loc) -> [(PN, I, PInfo)]
convSPI' os arch cinfo strfl solveExes = L.map (convSP os arch cinfo strfl solveExes) . CI.allPackages

-- | Convert a single source package into the solver-specific format.
convSP :: OS -> Arch -> CompilerInfo -> StrongFlags -> SolveExecutables -> SourcePackage loc -> (PN, I, PInfo)
convSP os arch cinfo strfl solveExes (SourcePackage (PackageIdentifier pn pv) gpd _ _pl) =
  let i = I pv InRepo
  in  (pn, i, convGPD os arch cinfo strfl solveExes pn gpd)

-- We do not use 'flattenPackageDescription' or 'finalizePD'
-- from 'Distribution.PackageDescription.Configuration' here, because we
-- want to keep the condition tree, but simplify much of the test.

-- | Convert a generic package description to a solver-specific 'PInfo'.
convGPD :: OS -> Arch -> CompilerInfo -> StrongFlags -> SolveExecutables ->
           PN -> GenericPackageDescription -> PInfo
convGPD os arch cinfo strfl solveExes pn
        (GenericPackageDescription pkg flags mlib sub_libs flibs exes tests benchs) =
  let
    fds  = flagInfo strfl flags

    -- | We have to be careful to filter out dependencies on
    -- internal libraries, since they don't refer to real packages
    -- and thus cannot actually be solved over.  We'll do this
    -- by creating a set of package names which are "internal"
    -- and dropping them as we convert.

    ipns = S.fromList $ [ unqualComponentNameToPackageName nm
                        | (nm, _) <- sub_libs ]

    conv :: Mon.Monoid a => Component -> (a -> BuildInfo) -> DependencyReason PN ->
            CondTree ConfVar [Dependency] a -> FlaggedDeps PN
    conv comp getInfo dr =
        convCondTree dr pkg os arch cinfo pn fds comp getInfo ipns solveExes .
        PDC.addBuildableCondition getInfo

    initDR = DependencyReason pn [] []

    flagged_deps
        = concatMap (\ds ->       conv ComponentLib         libBuildInfo        initDR ds) (maybeToList mlib)
       ++ concatMap (\(nm, ds) -> conv (ComponentSubLib nm) libBuildInfo        initDR ds) sub_libs
       ++ concatMap (\(nm, ds) -> conv (ComponentFLib nm)   foreignLibBuildInfo initDR ds) flibs
       ++ concatMap (\(nm, ds) -> conv (ComponentExe nm)    buildInfo           initDR ds) exes
       ++ prefix (Stanza (SN pn TestStanzas))
            (L.map  (\(nm, ds) -> conv (ComponentTest nm)   testBuildInfo (addStanza TestStanzas initDR) ds)
                    tests)
       ++ prefix (Stanza (SN pn BenchStanzas))
            (L.map  (\(nm, ds) -> conv (ComponentBench nm)  benchmarkBuildInfo (addStanza BenchStanzas initDR) ds)
                    benchs)
       ++ maybe []  (convSetupBuildInfo pn) (setupBuildInfo pkg)

    addStanza :: Stanza -> DependencyReason pn -> DependencyReason pn
    addStanza s (DependencyReason pn' fs ss) = DependencyReason pn' fs (s : ss)
  in
    PInfo flagged_deps fds Nothing

-- | Create a flagged dependency tree from a list @fds@ of flagged
-- dependencies, using @f@ to form the tree node (@f@ will be
-- something like @Stanza sn@).
prefix :: (FlaggedDeps qpn -> FlaggedDep qpn)
       -> [FlaggedDeps qpn] -> FlaggedDeps qpn
prefix _ []  = []
prefix f fds = [f (concat fds)]

-- | Convert flag information. Automatic flags are now considered weak
-- unless strong flags have been selected explicitly.
flagInfo :: StrongFlags -> [PD.Flag] -> FlagInfo
flagInfo (StrongFlags strfl) =
    M.fromList . L.map (\ (MkFlag fn _ b m) -> (fn, FInfo b (flagType m) (weak m)))
  where
    weak m = WeakOrTrivial $ not (strfl || m)
    flagType m = if m then Manual else Automatic

-- | Internal package names, which should not be interpreted as true
-- dependencies.
type IPNs = Set PN

-- | Convenience function to delete a 'FlaggedDep' if it's
-- for a 'PN' that isn't actually real.
filterIPNs :: IPNs -> Dependency -> FlaggedDep PN -> FlaggedDeps PN
filterIPNs ipns (Dependency pn _) fd
    | S.notMember pn ipns = [fd]
    | otherwise           = []

-- | Convert condition trees to flagged dependencies.  Mutually
-- recursive with 'convBranch'.  See 'convBranch' for an explanation
-- of all arguments preceeding the input 'CondTree'.
convCondTree :: DependencyReason PN -> PackageDescription -> OS -> Arch -> CompilerInfo -> PN -> FlagInfo ->
                Component ->
                (a -> BuildInfo) ->
                IPNs ->
                SolveExecutables ->
                CondTree ConfVar [Dependency] a -> FlaggedDeps PN
convCondTree dr pkg os arch cinfo pn fds comp getInfo ipns solveExes@(SolveExecutables solveExes') (CondNode info ds branches) =
                 concatMap
                    (\d -> filterIPNs ipns d (D.Simple (convLibDep dr d) comp))             ds  -- unconditional package dependencies
              ++ L.map (\e -> D.Simple (LDep dr (Ext  e)) comp) (PD.allExtensions bi) -- unconditional extension dependencies
              ++ L.map (\l -> D.Simple (LDep dr (Lang l)) comp) (PD.allLanguages  bi) -- unconditional language dependencies
              ++ L.map (\(PkgconfigDependency pkn vr) -> D.Simple (LDep dr (Pkg pkn vr)) comp) (PD.pkgconfigDepends bi) -- unconditional pkg-config dependencies
              ++ concatMap (convBranch dr pkg os arch cinfo pn fds comp getInfo ipns solveExes) branches
              -- build-tools dependencies
              -- NB: Only include these dependencies if SolveExecutables
              -- is True.  It might be false in the legacy solver
              -- codepath, in which case there won't be any record of
              -- an executable we need.
              ++ [ D.Simple (convExeDep dr exeDep) comp
                 | solveExes'
                 , exeDep <- getAllToolDependencies pkg bi
                 , not $ isInternal pkg exeDep
                 ]
  where
    bi = getInfo info

-- | Branch interpreter.  Mutually recursive with 'convCondTree'.
--
-- Here, we try to simplify one of Cabal's condition tree branches into the
-- solver's flagged dependency format, which is weaker. Condition trees can
-- contain complex logical expression composed from flag choices and special
-- flags (such as architecture, or compiler flavour). We try to evaluate the
-- special flags and subsequently simplify to a tree that only depends on
-- simple flag choices.
--
-- This function takes a number of arguments:
--
--      1. Some pre dependency-solving known information ('OS', 'Arch',
--         'CompilerInfo') for @os()@, @arch()@ and @impl()@ variables,
--
--      2. The package name @'PN'@ which this condition tree
--         came from, so that we can correctly associate @flag()@
--         variables with the correct package name qualifier,
--
--      3. The flag defaults 'FlagInfo' so that we can populate
--         'Flagged' dependencies with 'FInfo',
--
--      4. The name of the component 'Component' so we can record where
--         the fine-grained information about where the component came
--         from (see 'convCondTree'), and
--
--      5. A selector to extract the 'BuildInfo' from the leaves of
--         the 'CondTree' (which actually contains the needed
--         dependency information.)
--
--      6. The set of package names which should be considered internal
--         dependencies, and thus not handled as dependencies.
convBranch :: DependencyReason PN -> PackageDescription -> OS -> Arch -> CompilerInfo ->
              PN -> FlagInfo ->
              Component ->
              (a -> BuildInfo) ->
              IPNs ->
              SolveExecutables ->
              CondBranch ConfVar [Dependency] a ->
              FlaggedDeps PN
convBranch dr pkg os arch cinfo pn fds comp getInfo ipns solveExes (CondBranch c' t' mf') =
    go c'
       (\dr' ->           convCondTree dr' pkg os arch cinfo pn fds comp getInfo ipns solveExes  t')
       (\dr' -> maybe [] (convCondTree dr' pkg os arch cinfo pn fds comp getInfo ipns solveExes) mf')
       dr
  where
    go :: Condition ConfVar
       -> (DependencyReason PN -> FlaggedDeps PN)
       -> (DependencyReason PN -> FlaggedDeps PN)
       ->  DependencyReason PN -> FlaggedDeps PN
    go (Lit True)  t _ = t
    go (Lit False) _ f = f
    go (CNot c)    t f = go c f t
    go (CAnd c d)  t f = go c (go d t f) f
    go (COr  c d)  t f = go c t (go d t f)
    go (Var (Flag fn)) t f = \dr' ->
         -- Add each flag to the DependencyReason for all dependencies below,
         -- including any extracted dependencies. Extracted dependencies are
         -- introduced by both flag values (FlagBoth). Note that we don't
         -- actually need to add the flag to the extracted dependencies for
         -- correct backjumping; the information only improves log messages by
         -- giving the user the full reason for each dependency.
         let addFlagVal v = addFlag fn v dr'
         in extractCommon (t (addFlagVal FlagBoth))
                          (f (addFlagVal FlagBoth))
             ++ [ Flagged (FN pn fn) (fds ! fn) (t (addFlagVal FlagTrue))
                                                (f (addFlagVal FlagFalse)) ]
    go (Var (OS os')) t f
      | os == os'      = t
      | otherwise      = f
    go (Var (Arch arch')) t f
      | arch == arch'  = t
      | otherwise      = f
    go (Var (Impl cf cvr)) t f
      | matchImpl (compilerInfoId cinfo) ||
            -- fixme: Nothing should be treated as unknown, rather than empty
            --        list. This code should eventually be changed to either
            --        support partial resolution of compiler flags or to
            --        complain about incompletely configured compilers.
        any matchImpl (fromMaybe [] $ compilerInfoCompat cinfo) = t
      | otherwise      = f
      where
        matchImpl (CompilerId cf' cv) = cf == cf' && checkVR cvr cv

    addFlag :: FlagName -> FlagValue -> DependencyReason pn -> DependencyReason pn
    addFlag fn v (DependencyReason pn' flags stanzas) =
        DependencyReason pn' ((fn, v) : flags) stanzas

    -- If both branches contain the same package as a simple dep, we lift it to
    -- the next higher-level, but with the union of version ranges. This
    -- heuristic together with deferring flag choices will then usually first
    -- resolve this package, and try an already installed version before imposing
    -- a default flag choice that might not be what we want.
    --
    -- Note that we make assumptions here on the form of the dependencies that
    -- can occur at this point. In particular, no occurrences of Fixed, as all
    -- dependencies below this point have been generated using 'convLibDep'.
    --
    -- WARNING: This is quadratic!
    extractCommon :: Eq pn => FlaggedDeps pn -> FlaggedDeps pn -> FlaggedDeps pn
    extractCommon ps ps' =
        [ D.Simple (LDep (mergeDRs vs1 vs2) (Dep is_exe1 pn1 (Constrained $ vr1 .||. vr2))) comp
        | D.Simple (LDep vs1                (Dep is_exe1 pn1 (Constrained vr1))) _ <- ps
        , D.Simple (LDep vs2                (Dep is_exe2 pn2 (Constrained vr2))) _ <- ps'
        , pn1 == pn2
        , is_exe1 == is_exe2
        ]
      where
        -- Merge the DependencyReasons, because the extracted dependency can be
        -- avoided by removing the dependency from either side of the
        -- conditional.
        mergeDRs :: DependencyReason pn -> DependencyReason pn -> DependencyReason pn
        mergeDRs (DependencyReason pn' fs1 ss1) (DependencyReason _ fs2 ss2) =
            DependencyReason pn' (nub $ fs1 ++ fs2) (nub $ ss1 ++ ss2)

-- | Convert a Cabal dependency on a library to a solver-specific dependency.
convLibDep :: DependencyReason PN -> Dependency -> LDep PN
convLibDep dr (Dependency pn vr) = LDep dr $ Dep (IsExe False) pn (Constrained vr)

-- | Convert a Cabal dependency on a executable (build-tools) to a solver-specific dependency.
-- TODO do something about the name of the exe component itself
convExeDep :: DependencyReason PN -> ExeDependency -> LDep PN
convExeDep dr (ExeDependency pn _ vr) = LDep dr $ Dep (IsExe True) pn (Constrained vr)

-- | Convert setup dependencies
convSetupBuildInfo :: PN -> SetupBuildInfo -> FlaggedDeps PN
convSetupBuildInfo pn nfo =
    L.map (\d -> D.Simple (convLibDep (DependencyReason pn [] []) d) ComponentSetup) (PD.setupDepends nfo)
