/*
 * C callable bridge to the .NET object model
 *
 * Managed C++ is used to access the .NET object model via
 * System.Reflection. Here we provide C callable functions
 * to that functionality, which we then export via a COM
 * component.
 *
 * Note: the _only_ reason why we're going via COM and not simply
 * exposing the required via some DLL entry points, is that COM
 * gives us location independence (i.e., the RTS doesn't need
 * be told where this interop layer resides in order to hoik
 * it in, the CLSID suffices (provided the component has been
 * registered, of course.)) It is a bit tiresome to have play
 * by the .NET COM Interop's rules as regards argument arrays,
 * so we may want to revisit this issue at some point.
 * 
 * [ But why not simply use MC++ and provide C-callable entry
 *   points to the relevant functionality, and avoid COM interop
 *   alltogether? Because we have to be able to (statically)
 *   link with gcc-compiled code, and linking MC++ and gcc-compiled
 *   object files doesn't work.]
 *
 * Note: you need something never than gcc-2.95 to compile this
 *       code (I'm using gcc-3.2, which comes with mingw-2).
 */
#define _WIN32_DCOM
#define COBJMACROS
#include <stdio.h>
#include <stdlib.h>
#include <wtypes.h>
#ifndef _MSC_VER
#include <oaidl.h>
#include <objbase.h>
#include <oleauto.h>
# if defined(COBJMACROS) && !defined(_MSC_VER)
#define IErrorInfo_QueryInterface(T,r,O) (T)->lpVtbl->QueryInterface(T,r,O)
#define IErrorInfo_AddRef(T) (T)->lpVtbl->AddRef(T)
#define IErrorInfo_Release(T) (T)->lpVtbl->Release(T)
#define IErrorInfo_GetSource(T,pbstr) (T)->lpVtbl->GetSource(T,pbstr)
#define IErrorInfo_GetDescription(T,pbstr) (T)->lpVtbl->GetDescription(T,pbstr)

#define ISupportErrorInfo_QueryInterface(T,r,O) (T)->lpVtbl->QueryInterface(T,r,O)
#define ISupportErrorInfo_AddRef(T) (T)->lpVtbl->AddRef(T)
#define ISupportErrorInfo_Release(T) (T)->lpVtbl->Release(T)
#define ISupportErrorInfo_InterfaceSupportsErrorInfo(T,iid) (T)->lpVtbl->InterfaceSupportsErrorInfo(T,iid)
# endif
#endif
#include "DNInvoke.h"
#define WANT_UUID_DECLS
#include "InvokerClient.h"
#include "Dotnet.h"

/* Local prototypes */
static void genError( IUnknown* pUnk,
		      HRESULT hr,
		      char* loc,
		      char** pErrMsg);
static int  startBridge(char**);
static int  fromVariant
                    ( DotnetType resTy, 
		      VARIANT* pVar, 
		      void* res,
		      char** pErrMsg);
static VARIANT* toVariant ( DotnetArg* p );

/* Pointer to .NET COM component instance; instantiated on demand. */
static InvokeBridge* pBridge = NULL;

/* convert a char* to a BSTR, copied from the HDirect comlib/ sources */
static
HRESULT
stringToBSTR( /*[in,ptr]*/const char* pstrz
	    , /*[out]*/ BSTR* pbstr
	    )
{
  int i;

  if (!pbstr) {
    return E_FAIL;
  } else {
    *pbstr = NULL;
  }
  if (!pstrz) {
    return S_OK;
  }

  i = MultiByteToWideChar(CP_ACP, 0, pstrz, -1, NULL, 0);
  if ( i < 0 ) {
    return E_FAIL;
  }
  *pbstr = SysAllocStringLen(NULL,i-1);
  if (*pbstr != NULL) {
    MultiByteToWideChar(CP_ACP, 0, pstrz, -1, *pbstr, i-1); 
    //    (*pbstr)[i]=0;
    return S_OK;
  } else {
    return E_FAIL;
  }
}

static
char*
bstrToString( BSTR bstr )
{
    int  i,len;
    char *res;
    int  blen;

    if (!bstr) {
	return NULL;
    }
    
    blen =  SysStringLen(bstr);
    
    /* pass in NULL for the multi-byte arg in order to compute length first */
    len = WideCharToMultiByte(CP_ACP, 0, bstr, blen,
			      NULL, 0, NULL, NULL);
    if (len == 0) return NULL;
    
    /* Allocate string of required length. */
    res = (char*)malloc(sizeof(char) * (len + 1));
    if (!res) return NULL;
    
    i = WideCharToMultiByte(CP_ACP, 0, bstr, blen,
			    res, (len+1), NULL, NULL);
			    
    /* Poor error handling to map this to NULL. */
    if ( i == 0 ) return NULL;

    /* Terminate and return */
    res[i] = '\0';
    return res;
}

static
void
freeArgs ( SAFEARRAY* psa )
{
  /* The argument SAFEARRAYs contain dynamically allocated
   * VARIANTs. Release the VARIANT contents and its memory here.
   */
  long lb,ub;
  int i;
  HRESULT hr;
  VARIANT *pv = NULL;
  
  hr = SafeArrayGetLBound(psa, 1, &lb);
  if (FAILED(hr)) {
    fprintf(stderr, "freeArgs: failed fetching lower bound\n");
    SafeArrayDestroy(psa);
    return;
  }
  hr = SafeArrayGetUBound(psa, 1, &ub);
  if (FAILED(hr)) {
    fprintf(stderr, "freeArgs: failed fetching upper bound\n");
    SafeArrayDestroy(psa);
    return;
  }
  for ( i = 0; i < (ub - lb); i++ ) {
    hr = SafeArrayGetElement(psa,(long*)&i,(void*)pv);
    if (FAILED(hr)) {
      fprintf(stderr, "freeArgs: unable to fetch element %d\n", i);
      SafeArrayDestroy(psa);
      return;
    }
    VariantClear(pv);
    free(pv);
  }
  SafeArrayDestroy(psa);
}

static
SAFEARRAY*
marshalArgs ( DotnetArg*   args,
	      unsigned int n_args )
{
  SAFEARRAY *psa;
  SAFEARRAYBOUND rgsabound[1];
  int i;
  long idxArr[1];
  HRESULT hr;
  VARIANT* var;

  rgsabound[0].lLbound   = 0;
  rgsabound[0].cElements = n_args;
  psa = SafeArrayCreate(VT_VARIANT, 1, rgsabound);
  
  for(i=0;i < n_args; i++) {
    idxArr[0] = i;
    var = toVariant(&args[i]);
    hr = SafeArrayPutElement(psa, idxArr, (void*)var);
  }
  return psa;
}

/* 
 * ***** Accessing the .NET object model *****
 *
 * General remarks:
 *
 *   - the functions report error conditions via their return value; a char*.
 *     If NULL, the call was successful. If not, the returned string 
 *     contains the (dynamically allocated) error message. 
 * 
 *     This unorthodox calling convetion is used to simplify the task
 *     of interfacing to these funs from GHC-generated code.
 */

/*
 * Function: DN_invokeStatic()
 *
 * Given assembly and fully-qualified name of a static .NET method,
 * invoke it using the supplied arguments.
 *
 * Returns NULL on success, pointer to error message if an error.
 *
 */
char*
DN_invokeStatic ( char       *assemName,
		  char       *methName,
		  DotnetArg  *args,
		  int        n_args,
		  DotnetType resultTy,
		  void       *res)
{
    SAFEARRAY* psa;
    VARIANT    result;
    HRESULT    hr;
    BSTR       b_assemName;
    BSTR       b_methName;
    char*      errMsg = NULL;
    
    if (!pBridge && !startBridge(&errMsg)) {
      return errMsg;
    }
    
    /* Package up arguments */
    psa = marshalArgs(args, n_args);
    VariantInit(&result);
    
    hr = stringToBSTR(assemName, &b_assemName);
    hr = stringToBSTR(methName, &b_methName);

    hr = InvokeBridge_InvokeStaticMethod(pBridge,
					 b_assemName,
					 b_methName,
					 psa,
					 &result);
    SysFreeString(b_assemName);
    SysFreeString(b_methName);
    if (FAILED(hr)) {
	genError((IUnknown*)pBridge, hr, "DInvoke.invokeStatic", &errMsg);
	return errMsg;
    }
   
    fromVariant(resultTy, &result, res, &errMsg);
    freeArgs(psa);
  
    return errMsg;
}

/*
 * Function: DN_invokeMethod()
 *
 * Given method name and arguments, invoke .NET method on an object.
 * The object ref / this-pointer is passed in as the last argument.
 *
 * Returns NULL on success, pointer to error message if an error.
 *
 */
char*
DN_invokeMethod ( char       *clsAndMethName,
		  DotnetArg  *args,
		  int        n_args,
		  DotnetType resultTy,
		  void       *res)
{
    SAFEARRAY* psa;
    VARIANT    result;
    HRESULT    hr;
    char*      methName;
    BSTR       b_methName;
    char*      errMsg = NULL;
    VARIANT    *thisPtr;
    
    if (!pBridge && !startBridge(&errMsg)) {
      return errMsg;
    }
    
    if (n_args <= 0) {
      genError(NULL, 0x0, "Invoke.invokeMethod - missing this pointer", &errMsg);
      return errMsg;
    }
    
    /* The this-pointer is last */
    thisPtr = toVariant(&args[n_args-1]);

    /* Package up arguments */
    psa = marshalArgs(args, n_args-1);
    VariantInit(&result);
    
    /* If the user has qualified method with class, ignore the class bit. */
    if ( (methName = strrchr(clsAndMethName, '.')) == NULL) {
      methName = clsAndMethName;
    } else {
      /* Skip past '.' */
      methName++;
    }
    
    hr = stringToBSTR(methName, &b_methName);
    hr = InvokeBridge_InvokeMethod(pBridge,
				   *thisPtr,
				   b_methName,
				   psa,
				   &result);
    SysFreeString(b_methName);
    if (FAILED(hr)) {
	genError((IUnknown*)pBridge, hr, "Invoke.invokeMethod", &errMsg);
	return errMsg;
    }
    
    fromVariant(resultTy, &result, res, &errMsg);
    freeArgs(psa);
  
    return errMsg;
}

/*
 * Function: DN_getField()
 *
 * Given a field name and an object pointer, read a field value.
 * The object ref / this-pointer is passed in as the last argument.
 *
 * Returns NULL on success, pointer to error message if an error.
 *
 */
char*
DN_getField ( char       *clsAndMethName,
	      DotnetArg  *args,
	      int        n_args,
	      DotnetType resultTy,
	      void       *res)
{
    VARIANT    result;
    HRESULT    hr;
    char*      methName;
    BSTR       b_methName;
    char*      errMsg = NULL;
    VARIANT    *thisPtr;
    
    if (!pBridge && !startBridge(&errMsg)) {
      return errMsg;
    }
    
    if (n_args <= 0) {
      genError(NULL, 0x0, "Invoke.getField - missing this pointer", &errMsg);
      return errMsg;
    }
    
    /* The this-pointer is last */
    thisPtr = toVariant(&args[n_args-1]);
    VariantInit(&result);
    
    /* If the user has qualified method with class, ignore the class bit. */
    if ( (methName = strrchr(clsAndMethName, '.')) == NULL) {
      methName = clsAndMethName;
    } else {
      /* Skip past '.' */
      methName++;
    }
    
    hr = stringToBSTR(methName, &b_methName);
    hr = InvokeBridge_GetField(pBridge,
			       *thisPtr,
			       b_methName,
			       &result);
    SysFreeString(b_methName);
    if (FAILED(hr)) {
	genError((IUnknown*)pBridge, hr, "Invoke.getField", &errMsg);
	return errMsg;
    }
    
    fromVariant(resultTy, &result, res, &errMsg);
    return errMsg;
}

/*
 * Function: DN_setField()
 *
 * Given field name, a value and an object reference, set the field value of
 * an object.
 * The object ref / this-pointer is passed in as the last argument.
 *
 * Returns NULL on success, pointer to error message if an error.
 *
 */
char*
DN_setField ( char       *clsAndMethName,
	      DotnetArg  *args,
	      int        n_args,
	      /* next two args are ignored */
	      DotnetType resultTy,
	      void       *res)
{
    HRESULT    hr;
    char*      methName;
    BSTR       b_methName;
    char*      errMsg = NULL;
    VARIANT    *thisPtr;
    VARIANT    *pVal;

    if (!pBridge && !startBridge(&errMsg)) {
      return errMsg;
    }
    
    if (n_args != 2) {
      genError(NULL, 0x0, "Invoke.setField - missing this pointer", &errMsg);
      return errMsg;
    }
    
    /* The this-pointer is last */
    thisPtr = toVariant(&args[1]);

    /* Package up arguments */
    pVal = toVariant(&args[0]);
    
    /* If the user has qualified method with class, ignore the class bit. */
    if ( (methName = strrchr(clsAndMethName, '.')) == NULL) {
      methName = clsAndMethName;
    } else {
      /* Skip past '.' */
      methName++;
    }
    
    hr = stringToBSTR(methName, &b_methName);
    hr = InvokeBridge_SetField(pBridge,
			       *thisPtr,
			       b_methName,
			       *pVal);
    SysFreeString(b_methName);
    VariantClear(pVal);
    free(pVal);
    free(thisPtr);

    if (FAILED(hr)) {
	genError((IUnknown*)pBridge, hr, "Invoke.setField", &errMsg);
	return errMsg;
    }
    return errMsg;
}


/*
 * Function: DN_createObject()
 *
 * Given assembly and fully-qualified name of a type,
 * invoke its (possibly parameterised) constructor.
 *
 * Returns NULL on success, pointer to error message if an error.
 *
 */
char*
DN_createObject ( char       *assemName,
		  char       *methName,
		  DotnetArg  *args,
		  int        n_args,
		  DotnetType resultTy,
		  void       *res)
{
    SAFEARRAY* psa;
    VARIANT    result;
    HRESULT    hr;
    BSTR       b_assemName;
    BSTR       b_methName;
    char*      errMsg = NULL;
    
    if (!pBridge && !startBridge(&errMsg)) {
      return errMsg;
    }
    
    /* Package up arguments */
    psa = marshalArgs(args, n_args);
    VariantInit(&result);
    
    hr = stringToBSTR(assemName, &b_assemName);
    hr = stringToBSTR(methName, &b_methName);

    hr = InvokeBridge_CreateObject(pBridge,
				   b_assemName,
				   b_methName,
				   psa,
				   &result);
    SysFreeString(b_assemName);
    SysFreeString(b_methName);
    if (FAILED(hr)) {
	genError((IUnknown*)pBridge, hr, "DN_createObject", &errMsg);
	return errMsg;
    }
    
    fromVariant(resultTy, &result, res, &errMsg);
    freeArgs(psa);
  
    return errMsg;
}

/*
 * Function: DN_getStatic()
 *
 * Given assembly and fully-qualified field name, fetch value of static
 * field.
 *
 * Returns NULL on success, pointer to error message if an error.
 *
 */
char*
DN_getStatic ( char       *assemName,
	       char       *fieldClsName,
	       /* the next two args are ignored */
	       DotnetArg  *args,
	       int        n_args,
	       DotnetType resultTy,
	       void       *res)
{
    VARIANT    result;
    HRESULT    hr;
    BSTR       b_assemName;
    BSTR       b_clsName;
    BSTR       b_fieldName;
    char*      errMsg = NULL;
    char*      fieldName;
    char*      clsName = fieldName;
    
    if (!pBridge && !startBridge(&errMsg)) {
      return errMsg;
    }
    
    fieldName = (char*)malloc(sizeof(char) * (strlen(fieldClsName) + 1));
    strcpy(fieldName, fieldClsName);
    clsName = fieldName;
    
    if (( fieldName = strrchr(fieldName, '.')) == NULL ) {
      genError((IUnknown*)pBridge, 0x0, "Invoke.getStatic - malformed field spec", &errMsg);
      return errMsg;
    }
    *fieldName = '\0';
    fieldName++;
    
    VariantInit(&result);
    
    hr = stringToBSTR(assemName, &b_assemName);
    hr = stringToBSTR(fieldName, &b_fieldName);
    hr = stringToBSTR(clsName, &b_clsName);
    /* ToDo: honour assembly spec */
    hr = InvokeBridge_GetStaticField(pBridge,
				     b_clsName,
				     b_fieldName,
				     &result);
    SysFreeString(b_assemName);
    SysFreeString(b_clsName);
    SysFreeString(b_fieldName);
    if (FAILED(hr)) {
	genError((IUnknown*)pBridge, hr, "Invoke.getStatic", &errMsg);
	return errMsg;
    }
    fromVariant(resultTy, &result, res, &errMsg);
  
    return errMsg;
}

/*
 * Function: DN_setStatic()
 *
 * Given assembly and fully-qualified field name, set value of static
 * field.
 *
 * Returns NULL on success, pointer to error message if an error.
 *
 */
char*
DN_setStatic ( char       *assemName,
	       char       *fieldClsName,
	       DotnetArg  *args,
	       int        n_args,
	       /* the next two args are ignored */
	       DotnetType resultTy,
	       void       *res)
{
    VARIANT    result;
    VARIANT    *pVal;
    HRESULT    hr;
    BSTR       b_assemName;
    BSTR       b_clsName;
    BSTR       b_fieldName;
    char*      errMsg = NULL;
    char*      fieldName;
    char*      clsName = fieldName;
    
    if (!pBridge && !startBridge(&errMsg)) {
      return errMsg;
    }
    
    fieldName = (char*)malloc(sizeof(char) * (strlen(fieldClsName) + 1));
    strcpy(fieldName, fieldClsName);
    clsName = fieldName;
    
    if (( fieldName = strrchr(fieldName, '.')) == NULL ) {
      genError((IUnknown*)pBridge, 0x0, "Invoke.setStatic - malformed field spec", &errMsg);
      return errMsg;
    }
    *fieldName = '\0';
    fieldName++;
    
    pVal = toVariant(&args[0]);
    VariantInit(&result);
    
    hr = stringToBSTR(assemName, &b_assemName);
    hr = stringToBSTR(fieldName, &b_fieldName);
    hr = stringToBSTR(clsName, &b_clsName);
    /* ToDo: honour assembly spec */
    hr = InvokeBridge_SetStaticField(pBridge,
				     b_clsName,
				     b_fieldName,
				     *pVal);
    SysFreeString(b_assemName);
    SysFreeString(b_clsName);
    SysFreeString(b_fieldName);
    VariantClear(pVal);
    free(pVal);
    if (FAILED(hr)) {
	genError((IUnknown*)pBridge, hr, "Invoke.setStatic", &errMsg);
	return errMsg;
    }
    fromVariant(resultTy, &result, res, &errMsg);
  
    return errMsg;
}




/*
 * Function: startBridge(pErrMsg)
 *
 * Instantiates an InvokeBridge component, which is then
 * used to interact with the .NET world.
 *
 * If the component isn't available locally, zero is returned.
 * Otherwise, 1.
 */
static
int
startBridge(char** pErrMsg)
{
    HRESULT   hr;
    IUnknown *pUnk;

    hr = CoInitializeEx(NULL, COINIT_APARTMENTTHREADED);
    if (FAILED(hr)) {
	genError(NULL, hr, "DInvoke.createBridge.CoInitializeEx", pErrMsg);
	return FALSE;
    }

    hr = CoCreateInstance( &CLSID_InvokeBridge,
			   NULL,
			   CLSCTX_INPROC_SERVER,
			   &IID_IUnknown,
			   (void**)&pUnk);
    if (FAILED(hr)) {
	genError(NULL, hr, "DInvoke.createBridge.CoCreateInstance", pErrMsg);
	return 0;
    }
    
    hr = IUnknown_QueryInterface(pUnk, &IID_InvokeBridge, (void**)&pBridge);
    IUnknown_Release(pUnk);
    if (FAILED(hr)) {
	genError(pUnk, hr, "DInvoke.createBridge.QueryInterface.InvokeBridge", pErrMsg);
	return 0;
    }
    
    return 1;
}

/*
 * Function: stopBridge()
 *
 * Releases the InvokeBridge object and closes the COM library.
 * 
 */
void
stopDotnetBridge()
{
    if (pBridge) {
	InvokeBridge_Release(pBridge);
	pBridge = NULL;
	CoUninitialize();
    }
    /* Match up the call to CoInitializeEx() in startBridge(). */
}

/*
 * Function: genError()
 *
 * Construct a string describing an error condition given
 * an HRESULT and a location. 
 * 
 * If an interface pointer is passed in via the first arg, 
 * attempts are made to get at richer error information through
 * the IErrorInfo interface. (Note: we don't currently look for
 * the _Exception interface for even more detailed info.)
 *
 */
#define LOCATION_HDR "Location: "
#define HRESULT_HDR  "HRESULT: "
#define SOURCE_HDR   "Source: "
#define DESCR_HDR    "Description: "
#define NEWLINE_EXTRA 3

static
void
genError(IUnknown* pUnk,
	 HRESULT err,
	 char* loc,
	 char** pErrMsg)
{
  HRESULT hr;
  HRESULT invoke_hr = err;
  char*   invoke_src   = NULL;
  char*   invoke_descr = NULL;
  char*   buf;
  int     bufLen;
  
  /* If an interface pointer has been supplied, look for
   * IErrorInfo in order to get more detailed information
   * on the failure.
   *
   * The CLR's .NET COM Interop implementation does provide
   * IErrorInfo, so we're not really clutching at straws here..
   *
   * Note: CLR also reflects .NET exceptions via the _Exception*
   * interface..
   *
   */
  if (pUnk) {
    ISupportErrorInfo *pSupp;
    IErrorInfo        *pErrInfo;
    BSTR src   = NULL;
    BSTR descr = NULL;

    hr = IUnknown_QueryInterface(pUnk, 
				 &IID_ISupportErrorInfo,
				 (void**)&pSupp);
    if ( SUCCEEDED(hr) ) {
      hr = ISupportErrorInfo_InterfaceSupportsErrorInfo(pSupp,
							&IID_InvokeBridge);
      if ( SUCCEEDED(hr) ) {
	hr = GetErrorInfo(0,&pErrInfo);
	if ( SUCCEEDED(hr) ) {
	  IErrorInfo_GetSource(pErrInfo,&src);
	  IErrorInfo_GetDescription(pErrInfo,&descr);
	  invoke_src   = bstrToString(src);
	  invoke_descr = bstrToString(descr);

	  IErrorInfo_Release(pErrInfo);
	  if (src)   { SysFreeString(src);   src = NULL;   }
	  if (descr) { SysFreeString(descr); descr = NULL; }
	}
	ISupportErrorInfo_Release(pSupp);
      }
    }
  }
  /* Putting it all together.. */
  bufLen  = sizeof(LOCATION_HDR) + strlen(loc) + NEWLINE_EXTRA +
            sizeof(HRESULT_HDR)  + 16 + NEWLINE_EXTRA + 
            sizeof(SOURCE_HDR)   + (invoke_src ? strlen(invoke_src) : 16) + NEWLINE_EXTRA +
            sizeof(DESCR_HDR)    + (invoke_descr ? strlen(invoke_descr) : 16) + NEWLINE_EXTRA;
  buf = (char*) malloc(sizeof(char) * (bufLen + 1));
  if (!buf) {
    fprintf(stderr, "Unable to allocate %d for error message", (bufLen + 1));
    *pErrMsg = NULL;
    return;
  }
    
  _snprintf(buf, bufLen, "%s%s\n%s0x%08x\n%s%s\n%s%s",
	   LOCATION_HDR, loc,
	   HRESULT_HDR,  invoke_hr,
	   SOURCE_HDR,   invoke_src,
	   DESCR_HDR,    invoke_descr);

  /* Done with these chaps */
  if (invoke_src)   free(invoke_src);
  if (invoke_descr) free(invoke_descr);
  
  if (pErrMsg) *pErrMsg = buf;
  fprintf(stderr, "**InvokeBridge Error:\n%s", buf); fflush(stderr);
}

/* Converting to/from VARIANTs */

/*
 * Function: fromVariant()
 *
 * Unmarshal the contents of a VARIANT, converting its embedded value
 * into the desired DotnetType (if possible.)
 *
 * Returns 1 if successful, 0 otherwise. If the conversion fails, 
 * *pErrMsg holds the error message string.
 */
static
int
fromVariant (DotnetType resTy, 
	     VARIANT* pVar, 
	     void* res,
	     char** pErrMsg)
{
    VARIANT vNew;
    HRESULT hr;

    VariantInit(&vNew);
    switch(resTy) {
    case Dotnet_Byte:
    case Dotnet_Char:
	hr = VariantChangeType (&vNew, pVar, 0, VT_UI1);
	if (FAILED(hr)) {
	    genError(NULL, hr, "DInvoke.fromVariant{VT_UI1}", pErrMsg);
	    return FALSE;
	}
	*((unsigned char*)res) = vNew.bVal;
	return 1;
    case Dotnet_Boolean:
	hr = VariantChangeType (&vNew, pVar, 0, VT_BOOL);
	if (FAILED(hr)) {
	    genError(NULL, hr, "DInvoke.fromVariant{VT_BOOL}", pErrMsg);
	    return 0;
	}
	*((unsigned char*)res) = vNew.bVal;
	return 1;
    case Dotnet_Int:
	hr = VariantChangeType (&vNew, pVar, 0, VT_INT);
	if (FAILED(hr)) {
	    genError(NULL, hr, "DInvoke.fromVariant{VT_INT}", pErrMsg);
	    return 0;
	}
	*((int*)res) = vNew.intVal;
	return 1;
    case Dotnet_Int8:
	hr = VariantChangeType (&vNew, pVar, 0, VT_I1);
	if (FAILED(hr)) {
	    genError(NULL, hr, "DInvoke.fromVariant{VT_I1}", pErrMsg);
	    return 0;
	}
	*((signed char*)res) = vNew.bVal;
	return 1;
    case Dotnet_Int16:
	hr = VariantChangeType (&vNew, pVar, 0, VT_I2);
	if (FAILED(hr)) {
	    genError(NULL, hr, "DInvoke.fromVariant{VT_I2}", pErrMsg);
	    return 0;
	}
	*((signed short*)res) = vNew.iVal;
	return 1;
    case Dotnet_Int32:
	hr = VariantChangeType (&vNew, pVar, 0, VT_I4);
	if (FAILED(hr)) {
	    genError(NULL, hr, "DInvoke.fromVariant{VT_I4}", pErrMsg);
	    return 0;
	}
	*((signed int*)res) = vNew.lVal;
	return 1;
    case Dotnet_Int64:
	hr = VariantChangeType (&vNew, pVar, 0, VT_I8);
	if (FAILED(hr)) {
	    genError(NULL, hr, "DInvoke.fromVariant{VT_I8}", pErrMsg);
	    return 0;
	}
#ifdef _MSC_VER
	*((__int64*)res) = vNew.llVal;
#else
	*((long long*)res) = vNew.lVal;
#endif
	return 1;
    case Dotnet_Float:
	hr = VariantChangeType (&vNew, pVar, 0, VT_R4);
	if (FAILED(hr)) {
	    genError(NULL, hr, "DInvoke.fromVariant{VT_R4}", pErrMsg);
	    return 0;
	}
	*((float*)res) = vNew.fltVal;
	return 1;
    case Dotnet_Double:
	hr = VariantChangeType (&vNew, pVar, 0, VT_R8);
	if (FAILED(hr)) {
	    genError(NULL, hr, "DInvoke.fromVariant{VT_R4}", pErrMsg);
	    return 0;
	}
	*((double*)res) = vNew.dblVal;
	return 1;
    case Dotnet_Word8:
	hr = VariantChangeType (&vNew, pVar, 0, VT_UI1);
	if (FAILED(hr)) {
	    genError(NULL, hr, "DInvoke.fromVariant{VT_UI1}", pErrMsg);
	    return 0;
	}
	*((unsigned char*)res) = vNew.bVal;
	return 1;
    case Dotnet_Word16:
	hr = VariantChangeType (&vNew, pVar, 0, VT_UI2);
	if (FAILED(hr)) {
	    genError(NULL, hr, "DInvoke.fromVariant{VT_UI2}", pErrMsg);
	    return 0;
	}
	*((unsigned short*)res) = vNew.uiVal;
	return 1;
    case Dotnet_Word32:
	hr = VariantChangeType (&vNew, pVar, 0, VT_UI4);
	if (FAILED(hr)) {
	    genError(NULL, hr, "DInvoke.fromVariant{VT_UI4}", pErrMsg);
	    return 0;
	}
	*((unsigned int*)res) = vNew.ulVal;
	return 1;
    case Dotnet_Word64:
	hr = VariantChangeType (&vNew, pVar, 0, VT_UI8);
	if (FAILED(hr)) {
	    genError(NULL, hr, "DInvoke.fromVariant{VT_UI8}", pErrMsg);
	    return 0;
	}
#ifdef _MSC_VER
	*((unsigned __int64*)res) = vNew.ullVal;
#else
	*((unsigned long long*)res) = vNew.lVal;
#endif
	return 1;
    case Dotnet_Ptr:
	hr = VariantChangeType (&vNew, pVar, 0, VT_BYREF);
	if (FAILED(hr)) {
	    genError(NULL, hr, "DInvoke.fromVariant{VT_BYREF}", pErrMsg);
	    return 0;
	}
	*((void**)res) = vNew.byref;
	return 1;
    case Dotnet_Unit:
	return 0;
    case Dotnet_Object:
      if ( pVar->vt == VT_BSTR ) {
	/* Special handling for strings. If the user has asked for
	 * the string in object form, give him/her that. 
	 */
	VARIANT res;

	VariantInit(&res);
	hr = InvokeBridge_NewString(pBridge,
				    pVar->bstrVal,
				    &res);
	if (SUCCEEDED(hr)) {
	  pVar = &res;
	}
      }
	hr = VariantChangeType (&vNew, pVar, 0, VT_UNKNOWN);
	if (FAILED(hr)) {
	    genError(NULL, hr, "DInvoke.fromVariant{VT_UNKNOWN}", pErrMsg);
	    return 0;
	}
	*((IUnknown**)res) = vNew.punkVal;
	return 1;
    case Dotnet_String:
	hr = VariantChangeType (&vNew, pVar, 0, VT_BSTR);
	if (FAILED(hr)) {
	    genError(NULL, hr, "DInvoke.fromVariant{VT_BSTR}", pErrMsg);
	    return 0;
	}
	/* Storage is allocated by malloc(), caller is resp for freeing. */
	*((char**)res) = bstrToString(vNew.bstrVal);
	return 1;
    }
    return 0;
}

/*
 * Function: toVariant()
 *
 * Convert a DotnetArg into a VARIANT. The VARIANT
 * is dynamically allocated.
 *
 * The result is the pointer to the filled-in VARIANT structure;
 * NULL if allocation failed.
 *
 */
static
VARIANT*
toVariant ( DotnetArg* p )
{
  VARIANT* v = (VARIANT*)malloc(sizeof(VARIANT));
  if (!v) return NULL;
  VariantInit(v);
  switch (p->arg_type) {
  case Dotnet_Byte:
    v->vt = VT_UI1;
    v->bVal = p->arg.arg_byte;
    break;
  case Dotnet_Char:
    v->vt = VT_UI1;
    v->bVal = p->arg.arg_char;
    break;
  case Dotnet_Boolean:
    v->vt = VT_BOOL;
    v->boolVal = p->arg.arg_bool;
    break;
  case Dotnet_Int:
    v->vt = VT_INT;
    v->intVal = p->arg.arg_int;
    break;
  case Dotnet_Int8:
    v->vt = VT_I1;
    v->bVal = p->arg.arg_int8;
    break;
  case Dotnet_Int16:
    v->vt = VT_I2;
    v->iVal = p->arg.arg_int16;
    break;
  case Dotnet_Int32:
    v->vt = VT_I4;
    v->lVal = p->arg.arg_int32;
    break;
  case Dotnet_Int64:
    v->vt = VT_I8;
#ifdef _MSC_VER
    v->llVal = p->arg.arg_int64;
#else
    (long long*)(v->lVal) = p->arg.arg_int64;
#endif
    break;
  case Dotnet_Float:
    v->vt = VT_R4;
    v->fltVal = p->arg.arg_float;
    break;
  case Dotnet_Double:
    v->vt = VT_R8;
    v->dblVal = p->arg.arg_double;
    break;
  case Dotnet_Word8:
    v->vt = VT_UI1;
    v->bVal = p->arg.arg_word8;
    break;
  case Dotnet_Word16:
    v->vt = VT_UI2;
    v->uiVal = p->arg.arg_word16;
    break;
  case Dotnet_Word32:
    v->vt = VT_UI4;
    v->ulVal = p->arg.arg_word32;
    break;
  case Dotnet_Word64:
    v->vt = VT_UI8;
#ifdef _MSC_VER
    v->ullVal = p->arg.arg_word64;
#else
    (unsigned long long*)(v->lVal) = p->arg.arg_word64;
#endif
    break;
  case Dotnet_Ptr:
    v->vt = VT_BYREF;
    v->byref = p->arg.arg_ptr;
    break;
  case Dotnet_Unit:
    v->vt = VT_EMPTY;
    break;
  case Dotnet_Object:
    v->vt = VT_UNKNOWN;
    v->punkVal = (IUnknown*)p->arg.arg_obj;
    break;
  case Dotnet_String: {
    BSTR b;
    HRESULT hr;
    v->vt = VT_BSTR;
    hr = stringToBSTR((const char*)p->arg.arg_str,&b);
    v->bstrVal = b;
    break; }
  }
  return v;
}
