/*
 * <ffi.h> wrapper working around #23568.
 *
 * (c) The University of Glasgow 2023
 *
 */

#pragma once

/*
 * Note [FFI_GO_CLOSURES workaround]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Apple ships a broken libffi with Xcode which lacks a definition of
 * FFI_GO_CLOSURES despite having references to said macro. Work around this
 * for now to avoid -Wundef warnings.
 *
 * We choose the value zero here by following the model of OpenJDK.
 * See https://github.com/openjdk/jdk17u-dev/pull/741/files.
 *
 * See #23568.
 */
#if defined(darwin_HOST_OS)
#if !defined(FFI_GO_CLOSURES)
#define FFI_GO_CLOSURES 0
#endif
#endif

#include "ffi.h"
