#include "MachDeps.h"

#if WORD_SIZE_IN_BITS == 64

# define WORD_SIZE_IN_BYTES    8
# define WORD_SIZE_BYTES_SHIFT 3
# define WORD_SIZE_BYTES_MASK  0b111
# define WORD_SIZE_BITS_SHIFT  6
# define WORD_SIZE_BITS_MASK   0b111111
# define WORD_MAXBOUND         0xffffffffffffffff
# define INT_MINBOUND         -0x8000000000000000
# define INT_MAXBOUND          0x7fffffffffffffff
# define ABS_INT_MINBOUND      0x8000000000000000
# define SQRT_INT_MAXBOUND     0xb504f333

#elif WORD_SIZE_IN_BITS == 32

# define WORD_SIZE_IN_BYTES    4
# define WORD_SIZE_BYTES_SHIFT 2
# define WORD_SIZE_BYTES_MASK  0b11
# define WORD_SIZE_BITS_SHIFT  5
# define WORD_SIZE_BITS_MASK   0b11111
# define WORD_MAXBOUND         0xffffffff
# define INT_MINBOUND         -0x80000000
# define INT_MAXBOUND          0x7fffffff
# define ABS_INT_MINBOUND      0x80000000
# define SQRT_INT_MAXBOUND     0xb504

#else
# error unsupported WORD_SIZE_IN_BITS config
#endif

