#include "Rts.h"

#include "LinkerInternals.h"

static int indent = 0;

static void printIndent(void)
{
  debugBelch("%*s", 2*indent, "");
}

void linkerDebug(const char *s, ...)
{
  va_list ap;
  va_start(ap,s);
  printIndent();
  vdebugBelch(s, ap);
  debugBelch("\n");
  va_end(ap);
}

void linkerDebugStart(const char *s, ...)
{
  va_list ap;
  va_start(ap,s);
  printIndent();
  vdebugBelch(s, ap);
  debugBelch("{\n");
  va_end(ap);
  indent++;
}

void linkerDebugEnd(const char *s, ...)
{
  va_list ap;
  va_start(ap,s);
  printIndent();
  vdebugBelch(s, ap);
  debugBelch("}\n");
  va_end(ap);
  indent--;
}

