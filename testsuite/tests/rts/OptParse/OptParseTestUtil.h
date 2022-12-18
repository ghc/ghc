#include "Rts.h"

#define CMP_BUF_LEN 100
extern char CMP_BUF[CMP_BUF_LEN];

void _TEST( char* flagToTest, int expectedFlagKey
          , RtsFlagValue expectedValue
          );
void _FAIL_TEST(char* flagToTest);

#define FAIL_TEST(...) \
    do { \
        snprintf(CMP_BUF, sizeof(CMP_BUF), __VA_ARGS__); \
        _FAIL_TEST(CMP_BUF); \
    } while (false)

#define VOID_TEST(i, ...) \
    do { \
        snprintf(CMP_BUF, CMP_BUF_LEN, __VA_ARGS__); \
        _TEST(CMP_BUF, i, NO_VAL(i)); \
    } while (false)

#define BOOL_TEST(i, value, ...) \
    do { \
        snprintf(CMP_BUF, CMP_BUF_LEN, __VA_ARGS__); \
        _TEST(CMP_BUF, i, BOOL_VAL(i, value)); \
    } while (false)

#define DOUBLE_TEST(i, value, ...) \
    do { \
        snprintf(CMP_BUF, CMP_BUF_LEN, __VA_ARGS__); \
        _TEST(CMP_BUF, i, DOUBLE_VAL(i, value)); \
    } while (false)

#define STGWORD64_TEST(i, value, ...) \
    do { \
        snprintf(CMP_BUF, CMP_BUF_LEN, __VA_ARGS__); \
        _TEST(CMP_BUF, i, STGWORD64_VAL(i, value)); \
    } while (false)

void VOID_FLAG_TEST(const RtsFlagKey i);
void BOOL_FLAG_TEST(const RtsFlagKey i);
void DOUBLE_FLAG_TEST(const RtsFlagKey i);
void STGWORD64_FLAG_TEST(const RtsFlagKey i);
void NATURAL_NUM_FLAG_TEST(const RtsFlagKey i, int min, int max);
