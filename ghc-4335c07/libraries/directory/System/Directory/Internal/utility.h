#if !defined alignof && __cplusplus < 201103L
# ifdef STDC_HEADERS
#  include <stddef.h>
# endif
# define alignof(x) offsetof(struct { char c; x m; }, m)
#endif
