#include "Rts.h"

void _TEST( char* flagToTest
          , int expectedFlagKey
          , char* expectedLongName
          , char* expectedShortName
          , RtsFlagValueType expectedFlagValueType
          , bool safe
          , RtsFlagValue expectedValue
          );

void _FAIL_TEST(char* flagToTest);
void _VOID_FLAG_TEST(const RtsFlagKey i);
void _BOOL_FLAG_TEST(const RtsFlagKey i);
