#include <iostream>

extern "C" {
void hello(int x) {
    std::cout << "hello world " << x << std::endl;
}
}
