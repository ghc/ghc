{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, ExplicitForAll, KindSignatures, RankNTypes, TypeApplications, TypeFamilies, TypeOperators #-}
module NativeTypeCaseRigid where
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import NativeCase
import qualified NativeNestedCase as N

blocked :: Proxy a -> Same a :~: Int
blocked _ = Refl

shadow :: Proxy x -> Proxy a -> Capture x a :~: '(x, 'False)
shadow _ _ = Refl

nested :: Proxy inner -> N.SameFallback ('Just inner) :~: Int
nested _ = Refl

left :: Proxy first -> N.LeftFirst ('N.Pair first ('Just 'True)) :~: Bool
left _ = Refl

indexed :: forall (tag :: N.Indexed Ordering). Proxy tag -> N.Uniform @Ordering ('Just tag) :~: Int
indexed _ = Refl

type family Unknown (a :: Type) :: Type

typeHead :: Proxy a -> N.UniformTypeHead ('Just a) :~: Int
typeHead _ = Refl

stuckTypeHead :: Proxy a -> N.UniformTypeHead ('Just (Unknown a)) :~: Int
stuckTypeHead _ = Refl

writtenFirst :: Proxy x -> N.WrittenFirst ('Just (Either Char x)) :~: Int
writtenFirst _ = Refl

wrongAliasField :: N.PickFlipped ('Just (Maybe (Either Char Bool))) :~: Proxy Bool
wrongAliasField = Refl

aliasFirst :: Proxy first -> N.AliasFirst ('N.Pair first 'Nothing) :~: Int
aliasFirst _ = Refl

mixedAliasFirst :: Proxy first -> N.MixedAlias ('N.Pair ('Just first) 'Nothing) :~: Int
mixedAliasFirst _ = Refl

knownIndexRigid :: Proxy field -> N.FamilyGatePick ('Just field) :~: Bool
knownIndexRigid _ = Refl

hiddenIndex :: Proxy x -> N.HiddenGatePick ('Just x) :~: Int
hiddenIndex _ = Refl

rigidArrowResult ::
  Proxy x -> N.ArrowResult 'LT ('Just x) :~: (() -> (Proxy 'LT, Proxy 'False))
rigidArrowResult _ = Refl

rigidForall :: N.ForallResult -> Proxy 'LT -> N.CaseFlag ('Just x) -> (Proxy 'LT, Int)
rigidForall select tag flag = select tag flag

rigidIndexedField :: Proxy flag ->
  N.IndexedFieldOrder ('Just ('N.RefinedFieldBox ('Proxy :: Proxy 'False) ('N.FieldOn flag))) :~: Int
rigidIndexedField _ = Refl

rigidOpenHead :: forall b (d :: N.OpenCaseGate (N.OpenCaseIndex b)).
  Proxy b -> Proxy d -> N.OpenIndexPick (N.OpenCaseIndex b) ('Just d) :~: Int
rigidOpenHead _ _ = Refl

rigidOpenField :: Proxy b -> Proxy flag ->
  N.OpenIndexPick (N.OpenCaseIndex b) ('Just ('N.OpenPlain flag)) :~: Int
rigidOpenField _ _ = Refl

type family DeferredOpenGate (k :: Type) :: N.OpenCaseGate k

rigidOpenFamily :: Proxy b ->
  N.OpenIndexPick (N.OpenCaseIndex b) ('Just (DeferredOpenGate (N.OpenCaseIndex b))) :~: Int
rigidOpenFamily _ = Refl

blockedTransport :: Proxy (d :: N.DependentFieldGate Bool 'True 'Proxy)
                 -> N.DependentFieldPick ('Just d) :~: Char
blockedTransport _ = Refl

blockedBranch :: Proxy seed -> Proxy bit ->
  N.RefinedBranchPick seed ('Just ('N.RefinedBranch 'Proxy ('N.RefinedBit bit))) :~: Int
blockedBranch _ _ = Refl
