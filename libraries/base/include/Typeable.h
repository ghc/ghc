/* ----------------------------------------------------------------------------
 * Macros to help make Typeable instances.
 * -------------------------------------------------------------------------- */

#define INSTANCE_TYPEABLE0(tycon,tcname,str) \
tcname = mkTyCon str; \
instance Typeable tycon where { typeOf _ = mkAppTy tcname [] }

#ifdef __GLASGOW_HASKELL__

#define INSTANCE_TYPEABLE1(tycon,tcname,str) \
tcname = mkTyCon str; \
instance Typeable1 tycon where { typeOf1 _ = mkAppTy tcname [] }

#define INSTANCE_TYPEABLE2(tycon,tcname,str) \
tcname = mkTyCon str; \
instance Typeable2 tycon where { typeOf2 _ = mkAppTy tcname [] }

#define INSTANCE_TYPEABLE3(tycon,tcname,str) \
tcname = mkTyCon str; \
instance Typeable3 tycon where { typeOf3 _ = mkAppTy tcname [] }

#else /* !__GLASGOW_HASKELL__ */

#define INSTANCE_TYPEABLE1(tycon,tcname,str) \
tcname = mkTyCon str; \
instance Typeable1 tycon where { typeOf1 _ = mkAppTy tcname [] }; \
instance Typeable a => Typeable (tycon a) where { typeOf = typeOfDefault }

#define INSTANCE_TYPEABLE2(tycon,tcname,str) \
tcname = mkTyCon str; \
instance Typeable2 tycon where { typeOf2 _ = mkAppTy tcname [] }; \
instance Typeable a => Typeable1 (tycon a) where { \
  typeOf1 = typeOf1Default }; \
instance (Typeable a, Typeable b) => Typeable (tycon a b) where { \
  typeOf = typeOfDefault }

#define INSTANCE_TYPEABLE3(tycon,tcname,str) \
tcname = mkTyCon str; \
instance Typeable3 tycon where { typeOf3 _ = mkAppTy tcname [] }; \
instance Typeable a => Typeable2 (tycon a) where { \
  typeOf2 = typeOf2Default }; \
instance (Typeable a, Typeable b) => Typeable1 (tycon a b) where { \
  typeOf1 = typeOf1Default }; \
instance (Typeable a, Typeable b, Typeable c) => Typeable (tycon a b c) where { \
  typeOf = typeOfDefault }

#endif /* !__GLASGOW_HASKELL__ */
