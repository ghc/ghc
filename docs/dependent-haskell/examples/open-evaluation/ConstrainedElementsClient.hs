{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE DataKinds, ExplicitNamespaces, RequiredTypeArguments, TypeApplications #-}
{-# LANGUAGE GADTs, NoExperimentalDependentTypes, PartialTypeSignatures, QuantifiedConstraints, RankNTypes, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module ConstrainedElementsClient (splitCollection, constrainedElements, annotatedElement, explicitElements, renderField, formatField, renderKnownFields, retainedMaybeDomain, aliasedReflection, runReflection, partialElement) where

import ConstrainedElements (Collection(..), Allowed, Collects(..), FieldKey, Field(..), applyField, field, withField, Reflection, reflectMaybe, MaybeBoolEcho, AtNothing)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))

-- | Use the signature's class constraint to form the family application.
assumed :: Collects c => Proxy c -> Elem c :~: Elem c
assumed _ = Refl

ground :: Elem [Bool] :~: Bool
ground = Refl

-- | Carry the evidence required by this field.
data Witness c where
    Witness :: Collects c => Proxy (Elem c) -> Witness c

fromWitness :: forall c. Witness c -> Bool
fromWitness (Witness _) =
    let local :: Elem c :~: Elem c
        local = Refl
    in case local of Refl -> True

-- | Use the matched dictionary in a written expression type.
annotatedElement :: Bool
annotatedElement = observe (Witness Proxy :: Witness [Bool])
  where
    observe :: forall c. Witness c -> Bool
    observe (Witness _) = case (Refl :: Elem c :~: Elem c) of Refl -> True

-- | Supply the same associated type to specified and required parameters.
explicitElements :: (Bool, Bool)
explicitElements = (id @(Elem [Bool]) True, required (type (Elem [Bool])) False)
  where
    required :: forall a -> a -> a
    required (type a) x = x

-- | Check the family with evidence from a signature, an instance and a pattern.
constrainedElements :: (Bool :~: Bool, Bool :~: Bool, Bool)
constrainedElements =
    (assumed (Proxy :: Proxy [Bool]), ground, fromWitness (Witness Proxy :: Witness [Bool]))

-- | Use the dictionary from the field at the flag supplied by the caller.
renderField :: Bool -> Maybe String
renderField tag = case field tag of
    Nothing -> Nothing
    Just (Field value render) -> Just (applyField tag render value)

-- | Choose the output format without naming the field's element type.
formatField :: Bool -> Maybe String
formatField tag = case field tag of
    Nothing -> Nothing
    Just present -> Just (withField (\_ render value -> "[" ++ render value ++ "]") tag present)

-- | Render an element of the domain supplied as a required type argument.
renderKnown :: forall (c :: Type) -> Collects c => (Elem c -> String) -> Elem c -> String
renderKnown (type _) render value = render value

-- | Use two class instances in a module with ordinary required type arguments.
renderKnownFields :: (String, String)
renderKnownFields = (renderKnown (type [Bool]) show True, renderKnown (type (FieldKey 'True)) (: []) 'x')

-- | Use the polymorphic proof with two runtime constructors and an unused bottom.
retainedMaybeDomain :: IO (Maybe Bool, Maybe Bool, Bool)
retainedMaybeDomain = do
    ref <- newIORef (Just False)
    first <- checked <$> readIORef ref
    writeIORef ref Nothing
    second <- checked <$> readIORef ref
    pure (first, second, quiet (error "forced Maybe index"))
  where
    checked :: Maybe Bool -> Maybe Bool
    checked x = case reflectMaybe x of Refl -> x

    quiet :: Maybe Bool -> Bool
    quiet x = case reflectMaybe x of Refl -> True

-- | Use the partial family and the qualified proof through their imported aliases.
aliasedReflection :: (Maybe Bool, AtNothing MaybeBoolEcho :~: 'Nothing)
aliasedReflection = (runReflection reflectMaybe (Just True), Refl)

-- | Use the supplied polymorphic proof at the runtime input.
runReflection :: (forall a. Reflection a) -> Maybe Bool -> Maybe Bool
runReflection proof x = case proof @Bool x of Refl -> x

-- | Forward a runtime element using the premises of a partial signature.
partialElement :: Bool -> Bool
partialElement = forward (Proxy @[]) (Proxy @Bool)
  where
    forward :: forall f a. (Allowed a, forall x. Allowed x => Collects (f x))
            => Proxy f -> Proxy a -> Elem (f a) -> _
    forward _ _ value = id @(Elem (f a)) value

-- | The collection determines the element type required by Rest.
splitCollection :: Collection c e => c -> Maybe (e, Rest c)
splitCollection = uncons
