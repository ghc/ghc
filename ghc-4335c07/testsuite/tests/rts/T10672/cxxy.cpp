// Copyright (C) 2015, Luke Iannini

#include <iostream>
#include <exception>
#include <string.h>

// Make sure can call unmangled names from Haskell's FFI
extern "C" {

int talkToCxx() {

  try {
    throw 20;
  }
  catch (int e) {
    std::cout << "An exception occurred. Exception Nr. " << e << '\n';
  }

  std::cout << "Hello From C++!";
}


}
