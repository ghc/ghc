#if !defined(IN_RTS_PRIVATE_GROUP)
#error EndPrivate.h without matching BeginPrivate
#else
#undef IN_RTS_PRIVATE_GROUP
#pragma GCC visibility pop
#endif
