#include<stdio.h>
#include<stdlib.h>

int hello_c(int a) {
    printf("Hello from C: %d\n", a);
    return a+1;
}


void write_c(char * s) {
    printf("Received string: %s\n", s);
}

char * alloc_c() {
    char * s = malloc(4);
    s[0] = 'g';
    s[1] = 'h';
    s[2] = 'c';
    s[3] = '\0';
    return s;
}

void modify_c(char * s) {
    for (int i=0;s[i]!=0;i++) {
        if (s[i] >= 'a' && s[i] <= 'z') s[i] -= 32;
    }
}

void callback_c(char *s, char (*f)(char)) {
    for (int i=0;s[i]!=0;i++) {
        s[i] = f(s[i]);
    }
}
