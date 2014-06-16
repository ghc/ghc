#include <dlfcn.h>
#include <stdio.h>

int main(void) {
    int i;
    void *dh;
    void (*test_init)(void);
    int (*test_foo)(void);
    void (*test_exit)(void);

    dh = dlopen("./T3807test.so", RTLD_NOW | RTLD_GLOBAL);
    if (!dh) {
        printf("Failed to open shared library: %s\n", dlerror());
        return 1;
    }

    test_init = dlsym(dh, "test_init");
    if (!test_init) {
        printf("Failed to find test_init: %s", dlerror());
        return 1;
    }
    test_foo = dlsym(dh, "foo");
    if (!test_foo) {
        printf("Failed to find test_foo: %s", dlerror());
        return 1;
    }
    test_exit = dlsym(dh, "test_exit");
    if (!test_exit) {
        printf("Failed to find test_exit: %s", dlerror());
        return 1;
    }

    test_init();
    i = test_foo();
    printf("i is %d\n", i);
    test_exit();

    return 0;
}
