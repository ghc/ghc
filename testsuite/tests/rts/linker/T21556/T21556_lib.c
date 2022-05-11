#include <stdio.h>

__attribute__((weak)) void hello(int x) {
	printf("hello %d\n", x+1);
}
