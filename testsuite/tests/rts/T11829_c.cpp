#include <iostream>
#include <exception>
extern "C" {

int func() {
  try {
    throw std::runtime_error("This is a test");
  } catch(const std::runtime_error &c) {
    std::cerr << c.what() << std::endl;
    return 42;
  } catch(...) {
    std::cerr << "Catch-all must not fire!" << std::endl;
    return -1;
  }
  std::cerr << "Control must not reach past try-catch block!" << std::endl;
  return -2;
}

}
