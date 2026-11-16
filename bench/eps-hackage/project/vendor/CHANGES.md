# Changes to the vendored packages

These are the Hackage releases with the minimal changes needed to build them
with GHC HEAD (10.1). All other files are unchanged.

* `data-default-class` gets a dependency on `base`. GHC HEAD implicitly loads
  `GHC.Essentials` from `base`, so a package without `base` fails.
* `blaze-markup`, `semialign`, `strict`: canonical `mappend = (<>)` and
  `return = pure`. Noncanonical definitions are errors in GHC HEAD
  (`ErrorWithoutFlag`, so no `-W` flag helps).
* `serialise`: `KindRepTYPE r` was replaced by `KindRepType` and
  `KindRepConstraint`, as in the boot package `binary`.

## blaze-markup-0.8.3.0

```diff
--- a/src/Text/Blaze/Internal.hs
+++ b/src/Text/Blaze/Internal.hs
@@ -144,7 +144,7 @@
     mempty = EmptyChoiceString
     {-# INLINE mempty #-}
 #if !(MIN_VERSION_base(4,11,0))
-    mappend = AppendChoiceString
+    mappend = (<>)
     {-# INLINE mappend #-}
 #endif
 
@@ -187,7 +187,7 @@
     mempty = Empty mempty
     {-# INLINE mempty #-}
 #if !(MIN_VERSION_base(4,11,0))
-    mappend x y = Append x y
+    mappend = (<>)
     {-# INLINE mappend #-}
     mconcat = foldr Append (Empty mempty)
     {-# INLINE mconcat #-}
@@ -220,7 +220,7 @@
     -- {-# INLINE (<*) #-}
 
 instance Monad MarkupM where
-    return x = Empty x
+    return = pure
     {-# INLINE return #-}
     (>>) = Append
     {-# INLINE (>>) #-}
@@ -264,7 +264,7 @@
 instance Monoid Attribute where
     mempty                            = Attribute id
 #if !(MIN_VERSION_base(4,11,0))
-    Attribute f `mappend` Attribute g = Attribute (g . f)
+    mappend = (<>)
 #endif
 
 -- | The type for the value part of an attribute.
```

## data-default-class-0.2.0.0

```diff
--- a/data-default-class.cabal
+++ b/data-default-class.cabal
@@ -19,6 +19,6 @@
     branch: flare
 
 library
-    build-depends:     data-default ^>=0.8
+    build-depends:     base, data-default ^>=0.8
     exposed-modules:   Data.Default.Class
     default-language:  Haskell98
```

## semialign-1.4

```diff
--- a/src/Data/Zip.hs
+++ b/src/Data/Zip.hs
@@ -35,7 +35,7 @@
 
 instance (Repeat f, Monoid a) => Monoid (Zippy f a) where
     mempty                      = Zippy $ repeat mempty
-    mappend (Zippy x) (Zippy y) = Zippy $ zipWith mappend x y
+    mappend = (<>)
 
 #ifdef MIN_VERSION_semigroupoids
 instance Zip f => Apply (Zippy f) where
```

## serialise-0.2.6.1

```diff
--- a/src/Codec/Serialise/Class.hs
+++ b/src/Codec/Serialise/Class.hs
@@ -1269,7 +1269,12 @@
       KindRepVar bndr       -> encodeListLen 2 <> encodeWord 1 <> encode bndr
       KindRepApp a b        -> encodeListLen 3 <> encodeWord 2 <> encode a <> encode b
       KindRepFun a b        -> encodeListLen 3 <> encodeWord 3 <> encode a <> encode b
+#if __GLASGOW_HASKELL__ >= 1001
+      KindRepType           -> encodeListLen 1 <> encodeWord 4
+      KindRepConstraint     -> encodeListLen 1 <> encodeWord 6
+#else
       KindRepTYPE r         -> encodeListLen 2 <> encodeWord 4 <> encode r
+#endif
       KindRepTypeLit sort r -> encodeListLen 3 <> encodeWord 5 <> encode sort <> encode r
 
   decode = do
@@ -1280,7 +1285,12 @@
       1 | len == 2 -> KindRepVar <$> decode
       2 | len == 3 -> KindRepApp <$> decode <*> decode
       3 | len == 3 -> KindRepFun <$> decode <*> decode
+#if __GLASGOW_HASKELL__ >= 1001
+      4 | len == 1 -> pure KindRepType
+      6 | len == 1 -> pure KindRepConstraint
+#else
       4 | len == 2 -> KindRepTYPE <$> decode
+#endif
       5 | len == 3 -> KindRepTypeLit <$> decode <*> decode
       _            -> fail "Data.Serialise.Binary.CBOR.getKindRep: invalid tag"
 
```

## strict-0.5.1

```diff
--- a/src/Data/Strict/Tuple.hs
+++ b/src/Data/Strict/Tuple.hs
@@ -121,7 +121,7 @@
 
 instance (Monoid a, Monoid b) => Monoid (Pair a b) where
   mempty                            = mempty :!: mempty
-  (x1 :!: y1) `mappend` (x2 :!: y2) = (x1 `mappend` x2) :!: (y1 `mappend` y2)
+  mappend = (<>)
 
 -- deepseq
 instance (NFData a, NFData b) => NFData (Pair a b) where
```

