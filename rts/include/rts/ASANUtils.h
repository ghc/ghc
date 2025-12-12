#pragma once

#if defined(__SANITIZE_ADDRESS__)
#define ASAN_ENABLED
#elif defined(__has_feature)
#if __has_feature(address_sanitizer)
#define ASAN_ENABLED
#endif
#endif

#if defined(ASAN_ENABLED)
#include <sanitizer/asan_interface.h>
#define USED_IF_ASAN
#else
#include <stdlib.h>
#define USED_IF_ASAN __attribute__((unused))
#endif

static inline void
__ghc_asan_poison_memory_region(void const volatile *addr USED_IF_ASAN,
                                size_t size USED_IF_ASAN) {
#if defined(ASAN_ENABLED)
  __asan_poison_memory_region(addr, size);
#endif
}

static inline void
__ghc_asan_unpoison_memory_region(void const volatile *addr USED_IF_ASAN,
                                  size_t size USED_IF_ASAN) {
#if defined(ASAN_ENABLED)
  __asan_unpoison_memory_region(addr, size);
#endif
}
