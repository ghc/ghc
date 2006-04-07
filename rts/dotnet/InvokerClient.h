/*
 * InvokerClient interface defns for use with gcc.
 *
 * Note: These declarations mirror those of the InvokeBridge
 *       class declaration. 
 *
 */

#include <windows.h>
#include <wtypes.h>
#include <oaidl.h>

#ifdef __cplusplus
extern "C"{
#endif

#ifndef STDCALL
#define STDCALL __stdcall
#endif

extern const CLSID CLSID_InvokeBridge;
extern const IID   IID_IUnknown;
extern const IID   IID_NULL;
extern const IID   IID_InvokeBridge;

#ifdef WANT_UUID_DECLS
const CLSID CLSID_InvokeBridge = { 0x39D497D9,0x60E0,0x3525,{0xB7,0xF2,0x7B,0xC0,0x96,0xD3,0xA2,0xA3}};
//const IID IID_NULL = {0x00000000L, 0x0000, 0x0000, {0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00}};
//const IID IID_IUnknown = {0x00000000L, 0x0000, 0x0000, {0xC0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x46}};
const IID IID_InvokeBridge = { 0xAFF5FFCA, 0xC5C2, 0x3D5B, {0xAF, 0xD5, 0xED, 0x8E, 0x4B, 0x38, 0xDB, 0x7B}};
  //0x3A85D703, 0xFAE4,0x3C5E, {0x9F,0x7E,0x20,0x98,0x31,0xCD,0x61,0x7A}};
#endif

#ifndef __InvokeBridge_INTERFACE_DEFINED__
#define __InvokeBridge_INTERFACE_DEFINED__
#undef INTERFACE
#define INTERFACE InvokeBridge
DECLARE_INTERFACE(InvokeBridge)
{
    STDMETHOD(QueryInterface)(THIS_ REFIID,PVOID*) PURE;
    STDMETHOD_(ULONG,AddRef)(THIS) PURE;
    STDMETHOD_(ULONG,Release)(THIS) PURE;
    STDMETHOD(GetTypeInfoCount)(THIS_ UINT*) PURE;
    STDMETHOD(GetTypeInfo)(THIS_ UINT,LCID,LPTYPEINFO*) PURE;
    STDMETHOD(GetIDsOfNames)(THIS_ REFIID,LPOLESTR*,UINT,LCID,DISPID*) PURE;
    STDMETHOD(Invoke)(THIS_ DISPID,REFIID,LCID,WORD,DISPPARAMS*,VARIANT*,EXCEPINFO*,UINT*) PURE;

    STDMETHOD(ToString)(THIS_ BSTR*) PURE;
    STDMETHOD(Equals)(THIS_ BSTR*) PURE;
    STDMETHOD(GetHashCode)(THIS_ long*) PURE;
    STDMETHOD(GetType)(THIS_ IUnknown**);
    STDMETHOD(CreateObject)(THIS_ BSTR,BSTR,SAFEARRAY*, VARIANT*) PURE;
    STDMETHOD(InvokeMethod)(THIS_ VARIANT,BSTR,SAFEARRAY*,VARIANT*) PURE;
    STDMETHOD(InvokeStaticMethod)(THIS_ BSTR,BSTR,SAFEARRAY*,VARIANT*) PURE;

    HRESULT ( STDCALL *GetField )( 
            InvokeBridge * This,
            /* [in] */ VARIANT obj,
            /* [in] */ BSTR fieldSpec,
            /* [retval][out] */ VARIANT *pRetVal);
        
        HRESULT ( STDCALL *GetStaticField )( 
            InvokeBridge * This,
            /* [in] */ BSTR clsName,
            /* [in] */ BSTR fieldSpec,
            /* [retval][out] */ VARIANT *pRetVal);
        
        HRESULT ( STDCALL *SetField )( 
            InvokeBridge * This,
            /* [in] */ VARIANT obj,
            /* [in] */ BSTR fieldSpec,
            /* [in] */ VARIANT val);
        
        HRESULT ( STDCALL *SetStaticField )( 
            InvokeBridge * This,
            /* [in] */ BSTR clsName,
            /* [in] */ BSTR fieldSpec,
            /* [in] */ VARIANT val);
        
        HRESULT ( STDCALL *NewString )( 
            InvokeBridge * This,
            /* [in] */ BSTR s,
            /* [retval][out] */VARIANT* pRetVal);
        
        HRESULT ( STDCALL *NewArgArray )( 
            InvokeBridge * This,
            /* [in] */ long sz,
            /* [retval][out] */IUnknown **pRetVal);
        
        HRESULT ( STDCALL *SetArg )( 
            InvokeBridge * This,
            /* [in] */ SAFEARRAY * arr,
            /* [in] */ VARIANT val,
            /* [in] */ long idx);
        
        HRESULT ( STDCALL *GetArg )( 
            InvokeBridge * This,
            /* [in] */ SAFEARRAY * arr,
            /* [in] */ long idx,
            /* [retval][out] */ VARIANT *pRetVal);
        
        HRESULT ( STDCALL *GetType_2 )( 
            InvokeBridge * This,
            /* [in] */ BSTR typeName,
            /* [retval][out] */ IUnknown **pRetVal);
};
#endif

#define InvokeBridge_QueryInterface(This,riid,ppvObject)	\
    (This)->lpVtbl->QueryInterface(This,riid,ppvObject)

#define InvokeBridge_AddRef(This)	\
    (This)->lpVtbl->AddRef(This)

#define InvokeBridge_Release(This)	\
    (This)->lpVtbl->Release(This)

#define InvokeBridge_GetTypeInfoCount(This,pctinfo)	\
    (This)->lpVtbl->GetTypeInfoCount(This,pctinfo)

#define InvokeBridge_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    (This)->lpVtbl->GetTypeInfo(This,iTInfo,lcid,ppTInfo)

#define InvokeBridge_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl->GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define InvokeBridge_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl->Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)

#define InvokeBridge_get_ToString(This,pRetVal)	\
    (This)->lpVtbl->get_ToString(This,pRetVal)

#define InvokeBridge_Equals(This,obj,pRetVal)	\
    (This)->lpVtbl->Equals(This,obj,pRetVal)

#define InvokeBridge_GetHashCode(This,pRetVal)	\
    (This)->lpVtbl->GetHashCode(This,pRetVal)

#define InvokeBridge_GetType(This,pRetVal)	\
    (This)->lpVtbl->GetType(This,pRetVal)

#define InvokeBridge_CreateObject(This,assemName,objSpec,args,pRetVal)	\
    (This)->lpVtbl->CreateObject(This,assemName,objSpec,args,pRetVal)

#define InvokeBridge_InvokeMethod(This,obj,methSpec,args,pRetVal)	\
    (This)->lpVtbl->InvokeMethod(This,obj,methSpec,args,pRetVal)

#define InvokeBridge_InvokeStaticMethod(This,assemName,methSpec,args,pRetVal)	\
    (This)->lpVtbl->InvokeStaticMethod(This,assemName,methSpec,args,pRetVal)

#define InvokeBridge_GetField(This,obj,fieldSpec,pRetVal)	\
    (This)->lpVtbl->GetField(This,obj,fieldSpec,pRetVal)

#define InvokeBridge_GetStaticField(This,clsName,fieldSpec,pRetVal)	\
    (This)->lpVtbl->GetStaticField(This,clsName,fieldSpec,pRetVal)

#define InvokeBridge_SetField(This,obj,fieldSpec,val)	\
    (This)->lpVtbl->SetField(This,obj,fieldSpec,val)

#define InvokeBridge_SetStaticField(This,clsName,fieldSpec,val)	\
    (This)->lpVtbl->SetStaticField(This,clsName,fieldSpec,val)

#define InvokeBridge_NewString(This,s,pRetVal)	\
    (This)->lpVtbl->NewString(This,s,pRetVal)

#define InvokeBridge_NewArgArray(This,sz,pRetVal)	\
    (This)->lpVtbl->NewArgArray(This,sz,pRetVal)

#define InvokeBridge_SetArg(This,arr,val,idx)	\
    (This)->lpVtbl->SetArg(This,arr,val,idx)

#define InvokeBridge_GetArg(This,arr,idx,pRetVal)	\
    (This)->lpVtbl->GetArg(This,arr,idx,pRetVal)

#define InvokeBridge_GetType_2(This,typeName,pRetVal)	\
    (This)->lpVtbl->GetType_2(This,typeName,pRetVal)

#ifdef __cplusplus
}
#endif
