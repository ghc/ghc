/* ----------------------------------------------------------------------------
 * Macros to help make Typeable instances.
 *
 * INSTANCE_TYPEABLEn(tc,tcname,"tc") defines
 *
 *	instance Typeable/n/ tc
 *	instance Typeable a => Typeable/n-1/ (tc a)
 *	instance (Typeable a, Typeable b) => Typeable/n-2/ (tc a b)
 *	...
 *	instance (Typeable a1, ..., Typeable an) => Typeable (tc a1 ... an)
 * -------------------------------------------------------------------------- */

#ifndef TYPEABLE_H
#define TYPEABLE_H

#define INSTANCE_TYPEABLE0(tycon,tcname,str) \
tcname = mkTyCon str; \
instance Typeable tycon where { typeOf _ = mkTyConApp tcname [] }

#ifdef __GLASGOW_HASKELL__

/* For GHC, the extra instances follow from general instance declarations
 * defined in Data.Typeable. */

#define INSTANCE_TYPEABLE1(tycon,tcname,str) \
tcname = mkTyCon str; \
instance Typeable1 tycon where { typeOf1 _ = mkTyConApp tcname [] }

#define INSTANCE_TYPEABLE2(tycon,tcname,str) \
tcname = mkTyCon str; \
instance Typeable2 tycon where { typeOf2 _ = mkTyConApp tcname [] }

#define INSTANCE_TYPEABLE3(tycon,tcname,str) \
tcname = mkTyCon str; \
instance Typeable3 tycon where { typeOf3 _ = mkTyConApp tcname [] }

#else /* !__GLASGOW_HASKELL__ */

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

#endif /* !__GLASGOW_HASKELL__ */

#endif
