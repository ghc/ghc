#include <stddef.h>
#include <stdio.h>

// This test tries to ensure not only that all .ctors sections are run
// (#21618), but also that they are run in the right order (#21847).

int n1 = 0;
int n2 = 0;
int n3 = 0;

// This should be run first
void ctor1(void) {
    n1 = 42;
}

__attribute__((section(".ctors.2000"))) void* ctor_list1[] = { &ctor1 };

// This should be run second
void ctor2(void) {
    n2 = n1+1;
}

__attribute__((section(".ctors.1000"))) void * ctor_list2[] = { &ctor2 };

// This should be run last
void ctor3(void) {
    n3 = n2+1;
}

__attribute__((section(".ctors"))) void * ctor_list3[] = { &ctor3 };

void test(void) {
    printf("hello %d %d %d\n", n1, n2, n3);
}

