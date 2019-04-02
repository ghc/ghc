#include <iostream>
#include <stdexcept>

extern "C" {

void fn_hs();
void fn() {
    fn_hs();
}

void check(double sqrt2, double sqrt3,  double sqrt5,
           double sqrt8, double sqrt13, double sqrt21) {
    std::cout << std::fixed << sqrt2 << " " << sqrt3 << " " << sqrt5 << " "
              << sqrt8 << " " << sqrt13 << " " << sqrt21 << std::endl;
    if (sqrt2 != 1.41421 || sqrt3  != 1.73205 || sqrt5  != 2.23607 ||
        sqrt8 != 2.82843 || sqrt13 != 3.60555 || sqrt21 != 4.58258) {
        throw std::runtime_error("xmm registers have been scratched");
    }
}

int test() {
    try {
        double sqrt2  = 1.41421;
        double sqrt3  = 1.73205;
        double sqrt5  = 2.23607;
        double sqrt8  = 2.82843;
        double sqrt13 = 3.60555;
        double sqrt21 = 4.58258;
        check(sqrt2, sqrt3, sqrt5, sqrt8, sqrt13, sqrt21);
        fn();
        check(sqrt2, sqrt3, sqrt5, sqrt8, sqrt13, sqrt21);
        try {
            fn();
        } catch (const std::exception &) {
        }
        check(sqrt2, sqrt3, sqrt5, sqrt8, sqrt13, sqrt21);
    } catch (const std::exception &e) {
        std::cerr << e.what() << std::endl;
        return 1;
    }
    return 0;
}

} // extern "C"

