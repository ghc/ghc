#include <stdint.h>
#include <stdalign.h>

int foo = 42;       /* goes to __data, but __data gets page aligned as
                     * the first section within a segment, so we need
                     * another section to check the alignment */

alignas(32) int bar = 0;       /* goes to __common that follows __data
                                * within the same segment */

long isAligned()
{
        return ((uintptr_t)&bar & ~(-32)) == 0;
}
