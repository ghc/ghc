
extern void*      getDLLSymbol   Args((String,String));
extern void*      lookupSymbol   Args((ObjectFile file, String symbol));
extern ObjectFile loadLibrary    Args((String fn));
extern Bool       stdcallAllowed Args((void));

