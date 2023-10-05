#include <stdio.h>
#include <wchar.h>

void hello(wchar_t *buf) {
  swprintf_s(buf, 12, L"hello");
}
