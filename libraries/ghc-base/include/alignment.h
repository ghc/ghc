#if __GLASGOW_HASKELL__ < 711
#define hsc_alignment(t ) hsc_printf ( "%lu", (unsigned long)offsetof(struct {char x__; t(y__); }, y__));
#endif
