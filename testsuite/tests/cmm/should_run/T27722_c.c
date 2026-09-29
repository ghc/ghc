#include <stdint.h>

// Common block elimination used to drop the entry block together with
// its info table, so that this symbol went missing.
extern char zork_info[];

uintptr_t zork_info_addr(void) { return (uintptr_t) zork_info; }
