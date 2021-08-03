#ifndef __GHCJS_H__
#define __GHCJS_H__

/*
  The JavaScript handler is a function that is called when a
  `foreign import javascript' function is called in native code
  compiled with GHCJS.

  The default handler panics and quits the program immediately,
  custom handlers could run the code in a JS engine.

  The arguments are as follows:

   1. the JavaScript pattern in the declaration

   2. safety:
        0: unsafe
        1: safe
        2: interruptible

   3. pattern for the argument and result type. the first character
      is the return value type, other characters are for the arguments

      possible values:
        'v'       : void  (only for return value)
        'p'       : general pointer
        'r'       : JSVal pointer
        'i' / 'I' : HsInt   / HsWord
        'l' / 'L' : HsInt32 / HsWord32
        'm' / 'M' : HsInt64 / HsWord64
        's' / 'S' : HsInt16 / HsWord16
        'b' / 'B' : HsInt8  / HsWord8
        'c'       : HsChar
        'd'       : HsDouble
        'f'       : HsFloat

   4. pointer to store the return value

   5... vararg arguments to the function, types according to arg 2.
 */

typedef void (*javaScriptHandler)(const char*, int, const char*, void*, ...);

javaScriptHandler getJavaScriptHandler();
void setJavaScriptHandler(javaScriptHandler);

#endif
