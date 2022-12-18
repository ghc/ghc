#include <stdio.h>
#include <string.h>
#include "Rts.h"
#include "OptParseTestUtil.h"

extern bool ERROR;
char CMP_BUF[CMP_BUF_LEN];

void _TEST( char* flagToTest
          , int expectedFlagKey
          , RtsFlagValue expectedValue)
{
    debugBelch("\n(TEST) input: %s\n", flagToTest);
    printf("\n(TEST) input: %s\n", flagToTest);
    RtsFlagValue flagValue = parseArg(flagToTest, &ERROR);
    CHECK(!ERROR);
    RtsFlagName flag = rtsFlags[flagValue.key];
    printf("%i: %s %s %s\n", flagValue.key , flag.longName, flag.shortName, flag.optionSafe ? "SAFE": "UNSAFE");
    debugBelch("%i: %s %s %s\n", flagValue.key , flag.longName, flag.shortName, flag.optionSafe ? "SAFE": "UNSAFE");
    CHECK(flagValue.key == expectedFlagKey);
    RtsFlagValueType valueTy = flag.valueType;
    if (valueTy == BOOL) {
        printf("\tvalue: %s\n", flagValue.as.boolean ? "true" : "false");
        CHECK(expectedValue.as.boolean == flagValue.as.boolean);
    }
    if (valueTy == ENUM) {
        printf("\tvalue: %i\n", flagValue.as._enum);
        CHECK(expectedValue.as._enum == flagValue.as._enum);
    }
    if (valueTy == DOUBLE) {
        debugBelch("expected: %f actual: %f\n", expectedValue.as._double, flagValue.as._double);
        printf("\tvalue: %f\n", flagValue.as._double);
        CHECK(expectedValue.as._double == flagValue.as._double);
    }
    if (valueTy == STGWORD64) {
        debugBelch("expected: %" FMT_Word64 " actual: %" FMT_Word64 "\n", expectedValue.as.stgWord64, flagValue.as.stgWord64);
        printf("\tvalue: %" FMT_Word64 "\n", flagValue.as.stgWord64);
        CHECK(expectedValue.as.stgWord64 == flagValue.as.stgWord64);
    }
}

void _FAIL_TEST(char* flagToTest)
{
    debugBelch("\n(FAIL_TEST) input: %s\n", flagToTest);
    RtsFlagValue flagValue = parseArg(flagToTest, &ERROR);
    CHECK(ERROR);
    ERROR = false;
}

void VOID_FLAG_TEST(const RtsFlagKey i)
{
    RtsFlagName name = rtsFlags[i];
    if (name.longName)
    {
        VOID_TEST(i, "--%s", name.longName);

        FAIL_TEST("-%s=",       name.longName);
        FAIL_TEST("--%s=123G",  name.longName);
        FAIL_TEST("--%s=false", name.longName);
        FAIL_TEST("--%s=true",  name.longName);
    }
    if (name.shortName)
    {
        VOID_TEST(i, "-%s", name.shortName);

        FAIL_TEST("-%s=",       name.shortName);
        FAIL_TEST("-%s3621",    name.shortName);
        FAIL_TEST("-%s=3622",   name.shortName);
        FAIL_TEST("-%s=true",   name.shortName);
        FAIL_TEST("-%s=",       name.shortName);
        FAIL_TEST("-%s3622",    name.shortName);
        FAIL_TEST("-%s=3600",   name.shortName);
    }
}

void BOOL_FLAG_TEST(const RtsFlagKey i)
{
    RtsFlagName name = rtsFlags[i];
    if (name.longName != NULL)
    {
        BOOL_TEST(i, true,  "--%s",     name.longName);
        BOOL_TEST(i, true,  "--%s=yes", name.longName);
        BOOL_TEST(i, false, "--%s=no",  name.longName);

        FAIL_TEST("--%s=",    name.longName);
        FAIL_TEST("--%s=foo", name.longName);
        FAIL_TEST("--%s=1",   name.longName);
        FAIL_TEST("--%sjhgl", name.longName);
    }
    if (name.shortName != NULL)
    {
        BOOL_TEST(i, true, "-%s", name.shortName);

        FAIL_TEST("-%s=",    name.shortName);
        FAIL_TEST("-%s=foo", name.shortName);
        FAIL_TEST("-%s=1",   name.shortName);
        FAIL_TEST("-%sjhgl", name.shortName);
    }
}

void DOUBLE_FLAG_TEST(const RtsFlagKey i)
{
    RtsFlagName name = rtsFlags[i];
    if (name.longName != NULL)
    {
        if (!name.valueRequired)
        {
            VOID_TEST(i, "--%s", name.longName);
        }
        DOUBLE_TEST(i, 1125.0,  "--%s=1125",    name.longName);
        DOUBLE_TEST(i, 909.909, "--%s=909.909", name.longName);
        DOUBLE_TEST(i, 0.00567, "--%s=0.00567", name.longName);

        FAIL_TEST("--%s=",      name.longName);
        FAIL_TEST("--%s=bar",   name.longName);
        FAIL_TEST("--%s=false", name.longName);
        FAIL_TEST("--%s=true",  name.longName);
        FAIL_TEST("--%sxxzag",  name.longName);
    }
    if (name.shortName != NULL)
    {
        if (!name.valueRequired)
        {
            VOID_TEST(i, "-%s", name.shortName);
        }
        DOUBLE_TEST(i, 0.125, "-%s0.125", name.shortName);
        DOUBLE_TEST(i, 707.76, "-%s707.76", name.shortName);

        FAIL_TEST("-%s=",      name.shortName);
        FAIL_TEST("-%s=foo",   name.shortName);
        FAIL_TEST("-%s=false", name.shortName);
        FAIL_TEST("-%s=true",  name.shortName);
    }
}

void STGWORD64_FLAG_TEST(const RtsFlagKey i)
{
    RtsFlagName name = rtsFlags[i];
    if (name.longName != NULL)
    {
        STGWORD64_TEST(i, 8193.0 ,      "--%s=8193",   name.longName);
        STGWORD64_TEST(i, 8389632.0,    "--%s=8193K",  name.longName);
        STGWORD64_TEST(i, 2097152.0,    "--%s=2M",     name.longName);
        STGWORD64_TEST(i, 9663676416.0, "--%s=9G",     name.longName);
        STGWORD64_TEST(i, 2147483648.0, "--%s=2G",    name.longName);
        STGWORD64_TEST(i, 26664.0,      "--%s=3333w",  name.longName);

        FAIL_TEST("--%s=",      name.longName);
        FAIL_TEST("--%s=fbar",  name.longName);
        FAIL_TEST("--%s=false", name.longName);
        FAIL_TEST("--%s=true",  name.longName);
        FAIL_TEST("--%sxxzag",  name.longName);
    }
    if (name.shortName != NULL)
    {
        STGWORD64_TEST(i, 8193.0,       "-%s8193",   name.shortName);
        STGWORD64_TEST(i, 8389632.0,    "-%s8193k",  name.shortName);
        STGWORD64_TEST(i, 2097152.0,    "-%s2m",     name.shortName);
        STGWORD64_TEST(i, 9663676416.0, "-%s9g",     name.shortName);
        STGWORD64_TEST(i, 1073741824.0, "-%s1G",     name.shortName);
        STGWORD64_TEST(i, 26664.0,      "-%s3333w",  name.shortName);

        FAIL_TEST("-%s=",      name.shortName);
        FAIL_TEST("-%s=baz",   name.shortName);
        FAIL_TEST("-%s=false", name.shortName);
        FAIL_TEST("-%s=true",  name.shortName);
        FAIL_TEST("-%sjhgl",   name.shortName);
    }
}

void NATURAL_NUM_FLAG_TEST(const RtsFlagKey i, int min, int max)
{
    RtsFlagName name = rtsFlags[i];
    if (name.longName != NULL)
    {
        STGWORD64_TEST(i, (double)min,   "--%s=%i", name.longName, min);
        STGWORD64_TEST(i, (double)max,   "--%s=%i", name.longName, max);
        STGWORD64_TEST(i, (double)min+1, "--%s=%i", name.longName, min+1);
        STGWORD64_TEST(i, (double)max-5, "--%s=%i", name.longName, max-5);

        FAIL_TEST("--%s=%i",    name.longName, min-1);
        FAIL_TEST("--%s=%i",    name.longName, max+1);
        FAIL_TEST("--%s=",      name.longName);
        FAIL_TEST("--%s=fbar",  name.longName);
        FAIL_TEST("--%s=false", name.longName);
        FAIL_TEST("--%s=true",  name.longName);
        FAIL_TEST("--%sxxzag",  name.longName);
    }
    if (name.shortName != NULL)
    {
        STGWORD64_TEST(i, (double)min,   "--%s=%i", name.shortName, min);
        STGWORD64_TEST(i, (double)max,   "--%s=%i", name.shortName, max);
        STGWORD64_TEST(i, (double)min+2, "--%s=%i", name.shortName, min+2);
        STGWORD64_TEST(i, (double)max-3, "--%s=%i", name.shortName, max-3);

        FAIL_TEST("--%s=%i",    name.shortName, min-1);
        FAIL_TEST("--%s=%i",    name.shortName, max+1);
        FAIL_TEST("--%s=",      name.shortName);
        FAIL_TEST("--%s=fbar",  name.shortName);
        FAIL_TEST("--%s=false", name.shortName);
        FAIL_TEST("--%s=true",  name.shortName);
        FAIL_TEST("--%sxxzag",  name.shortName);
    }
}
