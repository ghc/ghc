
/* We combine the C files here.
 *
 * There is actually a good reason for this, really!
 * The alloc file contains a __attribute__((constructor)) function. We must
 * have this function in the same .o file as other stuff that actually gets
 * used otherwise the static linker doesn't bother to pull in the .o file
 * containing the constructor function. While we could just stick them in
 * the same .c file that'd be a bit annoying. So we combine them here.
 * */

#include "alloc.c"
#include "float.c"
#include "longlong.c"
