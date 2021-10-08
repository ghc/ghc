#include <cstdio>

class A {
public:
    const char *msg;
    A(const char *msg) {
        printf("constr %s\n", msg); 
        this->msg = msg;
    }

    ~A() {
        printf("destroy %s\n", this->msg); 
    }
};

A a("helloA");
A b("helloB");

int main() {
    printf("main\n");
    return 0;
}
