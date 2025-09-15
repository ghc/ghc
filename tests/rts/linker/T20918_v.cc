#include <stdlib.h>
#include <string>
#include <iostream>

extern "C"
{

  double equivalent_to_id(double my_double)
  {
    try {
      std::cout << "equivalent_to_id test-throw 1" << std::endl;
      throw 20;
    } catch(int my_error)
    {
      std::cout << "equivalent_to_id test-throw caught " << my_error << std::endl;
    }
    return my_double;
  }
}
