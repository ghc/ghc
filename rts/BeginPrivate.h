#if defined(IN_RTS_PRIVATE_GROUP)
#error Nested Begin/EndPrivate.h
#else
#pragma GCC visibility push(hidden)
#define IN_RTS_PRIVATE_GROUP
#undef ERROR_IF_IN_RTS_PRIVATE_GROUP
// It would be nice to use #error here, but we can't use macros
// inside of a define, so we're forced to just use garbage. This
// gives a compile time error as desired, but the error is a bit
// cryptic. That's ok because we intend to remove this machinery
// soon along with Begin/EndPrivate.h
#define ERROR_IF_IN_RTS_PRIVATE_GROUP ;!_ERROR__IN_RTS_PRIVATE_GROUP_!;
#endif
