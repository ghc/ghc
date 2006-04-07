/*
 * Types and definitions to support GHC .NET interop.
 *
 * (c) 2003, sof.
 *
 */
#ifndef __DOTNET_H__
#define __DOTNET_H__

typedef enum { 
  Dotnet_Byte = 0,
  Dotnet_Boolean,
  Dotnet_Char,
  Dotnet_Double,
  Dotnet_Float,
  Dotnet_Int,
  Dotnet_Int8,
  Dotnet_Int16,
  Dotnet_Int32,
  Dotnet_Int64,
  Dotnet_Word8,
  Dotnet_Word16,
  Dotnet_Word32,
  Dotnet_Word64,
  Dotnet_Ptr,
  Dotnet_Unit,
  Dotnet_Object,
  Dotnet_String
} DotnetType;

typedef union {
  unsigned char      arg_byte;
  unsigned int       arg_bool;
  unsigned char      arg_char;
  int                arg_int;
  signed   char      arg_int8;
  signed   short     arg_int16;
  signed   int       arg_int32;
#if defined(_MSC_VER)
  signed  __int64    arg_int64;
#else
  signed  long long  arg_int64;
#endif
  float              arg_float;
  double             arg_double;
  unsigned char      arg_word8;
  unsigned short     arg_word16;
  unsigned int       arg_word32;
#if defined(_MSC_VER)
  unsigned __int64   arg_word64;
#else
  unsigned long long arg_word64;
#endif
  void*              arg_ptr;
  void*              arg_obj;
  void*              arg_str;
} DotnetArgVal;

typedef struct {
  DotnetArgVal arg;
  DotnetType   arg_type;
} DotnetArg;

#endif /* __DOTNET_H__ */
