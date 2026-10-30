#if !defined(IN_RTS_PRIVATE_GROUP)
#error EndPrivate.h without matching BeginPrivate
#else
#undef ERROR_IF_IN_RTS_PRIVATE_GROUP
#define ERROR_IF_IN_RTS_PRIVATE_GROUP /**/
#undef IN_RTS_PRIVATE_GROUP
#pragma GCC visibility pop
#endif
