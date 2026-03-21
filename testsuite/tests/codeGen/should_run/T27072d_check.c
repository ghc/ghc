// Checker dylib for T27072d.
//
// Compiled as a dylib and linked against the test executable. Because dylib
// initializers run before the main executable's __mod_init_func entries,
// our __cxa_atexit registration happens first. Since __cxa_atexit handlers
// fire in LIFO order, our checker runs *after* the GHC-generated finalizer,
// allowing us to observe that SPT entries were removed.

#include <stdio.h>

// Provided by the RTS.
extern int hs_spt_key_count(void);

static void check_spt_finalizer(void *arg __attribute__((unused))) {
    int count = hs_spt_key_count();
    printf("SPT entries after finalizer: %d\n", count);
    fflush(stdout);
}

// Register the checker. This constructor runs during dylib initialization,
// which happens before the main executable's initializers.
__attribute__((constructor))
static void register_spt_checker(void) {
    // Use __cxa_atexit so we participate in the same LIFO chain as the
    // GHC-generated finalizer.
    extern int __cxa_atexit(void (*)(void *), void *, void *);
    extern void *__dso_handle;
    __cxa_atexit(check_spt_finalizer, (void *)0, &__dso_handle);
}
