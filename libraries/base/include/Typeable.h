{- --------------------------------------------------------------------------
// Macros to help make Typeable instances.
//
// INSTANCE_TYPEABLEn(tc,tcname,"tc") defines
//
//	instance Typeable/n/ tc
//	instance Typeable a => Typeable/n-1/ (tc a)
//	instance (Typeable a, Typeable b) => Typeable/n-2/ (tc a b)
//	...
//	instance (Typeable a1, ..., Typeable an) => Typeable (tc a1 ... an)
// --------------------------------------------------------------------------
-}

#ifndef TYPEABLE_H
#define TYPEABLE_H

#ifdef __GLASGOW_HASKELL__

--  // For GHC, we can use DeriveDataTypeable + StandaloneDeriving to
--  // generate the instances.

#define INSTANCE_TYPEABLE0(tycon,tcname,str) deriving instance Typeable tycon
#define INSTANCE_TYPEABLE1(tycon,tcname,str) deriving instance Typeable tycon
#define INSTANCE_TYPEABLE2(tycon,tcname,str) deriving instance Typeable tycon
#define INSTANCE_TYPEABLE3(tycon,tcname,str) deriving instance Typeable tycon
#define INSTANCE_TYPEABLE4(tycon,tcname,str) deriving instance Typeable tycon
#define INSTANCE_TYPEABLE5(tycon,tcname,str) deriving instance Typeable tycon
#define INSTANCE_TYPEABLE6(tycon,tcname,str) deriving instance Typeable tycon
#define INSTANCE_TYPEABLE7(tycon,tcname,str) deriving instance Typeable tycon

#else /* !__GLASGOW_HASKELL__ */

#define INSTANCE_TYPEABLE0(tycon,tcname,str) \
tcname :: TyCon; \
tcname = mkTyCon str; \
instance Typeable tycon where { typeOf _ = mkTyConApp tcname [] }

#define INSTANCE_TYPEABLE1(tycon,tcname,str) \
tcname = mkTyCon str; \
instance Typeable1 tycon where { typeOf1 _ = mkTyConApp tcname [] }; \
instance Typeable a => Typeable (tycon a) where { typeOf = typeOfDefault }

#define INSTANCE_TYPEABLE2(tycon,tcname,str) \
tcname = mkTyCon str; \
instance Typeable2 tycon where { typeOf2 _ = mkTyConApp tcname [] }; \
instance Typeable a => Typeable1 (tycon a) where { \
  typeOf1 = typeOf1Default }; \
instance (Typeable a, Typeable b) => Typeable (tycon a b) where { \
  typeOf = typeOfDefault }

#define INSTANCE_TYPEABLE3(tycon,tcname,str) \
tcname = mkTyCon str; \
instance Typeable3 tycon where { typeOf3 _ = mkTyConApp tcname [] }; \
instance Typeable a => Typeable2 (tycon a) where { \
  typeOf2 = typeOf2Default }; \
instance (Typeable a, Typeable b) => Typeable1 (tycon a b) where { \
  typeOf1 = typeOf1Default }; \
instance (Typeable a, Typeable b, Typeable c) => Typeable (tycon a b c) where { \
  typeOf = typeOfDefault }

#define INSTANCE_TYPEABLE4(tycon,tcname,str) \
tcname = mkTyCon str; \
instance Typeable4 tycon where { typeOf4 _ = mkTyConApp tcname [] }; \
instance Typeable a => Typeable3 (tycon a) where { \
  typeOf3 = typeOf3Default }; \
instance (Typeable a, Typeable b) => Typeable2 (tycon a b) where { \
  typeOf2 = typeOf2Default }; \
instance (Typeable a, Typeable b, Typeable c) => Typeable1 (tycon a b c) where { \
  typeOf1 = typeOf1Default }; \
instance (Typeable a, Typeable b, Typeable c, Typeable d) => Typeable (tycon a b c d) where { \
  typeOf = typeOfDefault }

#define INSTANCE_TYPEABLE5(tycon,tcname,str) \
tcname = mkTyCon str; \
instance Typeable5 tycon where { typeOf5 _ = mkTyConApp tcname [] }; \
instance Typeable a => Typeable4 (tycon a) where { \
  typeOf4 = typeOf4Default }; \
instance (Typeable a, Typeable b) => Typeable3 (tycon a b) where { \
  typeOf3 = typeOf3Default }; \
instance (Typeable a, Typeable b, Typeable c) => Typeable2 (tycon a b c) where { \
  typeOf2 = typeOf2Default }; \
instance (Typeable a, Typeable b, Typeable c, Typeable d) => Typeable1 (tycon a b c d) where { \
  typeOf1 = typeOf1Default }; \
instance (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e) => Typeable (tycon a b c d e) where { \
  typeOf = typeOfDefault }

#define INSTANCE_TYPEABLE6(tycon,tcname,str) \
tcname = mkTyCon str; \
instance Typeable6 tycon where { typeOf6 _ = mkTyConApp tcname [] }; \
instance Typeable a => Typeable5 (tycon a) where { \
  typeOf5 = typeOf5Default }; \
instance (Typeable a, Typeable b) => Typeable4 (tycon a b) where { \
  typeOf4 = typeOf4Default }; \
instance (Typeable a, Typeable b, Typeable c) => Typeable3 (tycon a b c) where { \
  typeOf3 = typeOf3Default }; \
instance (Typeable a, Typeable b, Typeable c, Typeable d) => Typeable2 (tycon a b c d) where { \
  typeOf2 = typeOf2Default }; \
instance (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e) => Typeable1 (tycon a b c d e) where { \
  typeOf1 = typeOf1Default }; \
instance (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f) => Typeable (tycon a b c d e f) where { \
  typeOf = typeOfDefault }

#define INSTANCE_TYPEABLE7(tycon,tcname,str) \
tcname = mkTyCon str; \
instance Typeable7 tycon where { typeOf7 _ = mkTyConApp tcname [] }; \
instance Typeable a => Typeable6 (tycon a) where { \
  typeOf6 = typeOf6Default }; \
instance (Typeable a, Typeable b) => Typeable5 (tycon a b) where { \
  typeOf5 = typeOf5Default }; \
instance (Typeable a, Typeable b, Typeable c) => Typeable4 (tycon a b c) where { \
  typeOf4 = typeOf4Default }; \
instance (Typeable a, Typeable b, Typeable c, Typeable d) => Typeable3 (tycon a b c d) where { \
  typeOf3 = typeOf3Default }; \
instance (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e) => Typeable2 (tycon a b c d e) where { \
  typeOf2 = typeOf2Default }; \
instance (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f) => Typeable1 (tycon a b c d e f) where { \
  typeOf1 = typeOf1Default }; \
instance (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable g) => Typeable (tycon a b c d e f g) where { \
  typeOf = typeOfDefault }

#endif /* !__GLASGOW_HASKELL__ */

#endif
