/* ----------------------------------------------------------------------------
 * Macros to help make Typeable instances.
 * -------------------------------------------------------------------------- */

#define INSTANCE_TYPEABLE0(tycon,tcname,str) \
tcname = mkTyCon str; \
instance Typeable tycon where { typeOf _ = mkAppTy tcname [] }

#define INSTANCE_TYPEABLE1(tycon,tcname,str) \
tcname = mkTyCon str; \
instance Typeable a => Typeable (tycon a) where { \
  typeOf x = mkAppTy tcname [typeOf ((undefined :: tycon a -> a) x) ] }

#define INSTANCE_TYPEABLE2(tycon,tcname,str) \
tcname = mkTyCon str; \
instance (Typeable a, Typeable b) => Typeable (tycon a b) where { \
  typeOf x = mkAppTy tcname [typeOf ((undefined :: tycon a b -> a) x), \
			     typeOf ((undefined :: tycon a b -> b) x)] }

#define INSTANCE_TYPEABLE3(tycon,tcname,str) \
tcname = mkTyCon str; \
instance (Typeable a, Typeable b, Typeable c) => Typeable (tycon a b c) where {\
  typeOf a = mkAppTy tcname [typeOf ((undefined :: tycon a b c -> a) a), \
			     typeOf ((undefined :: tycon a b c -> b) a), \
			     typeOf ((undefined :: tycon a b c -> c) a)] }
