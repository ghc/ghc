//
// (c) 2003, sof.
//
// Dynamic invocation helper classes. The details of how
// to access the .NET object model via the Reflection API
// is taken care of by Invoker.{h,cpp}
//
#pragma once
#using <mscorlib.dll>

using namespace System;
using namespace System::Reflection;
using namespace System::Text;
using namespace System::Runtime::InteropServices;

[assembly:AssemblyKeyFileAttribute(S"invoker.snk")];

namespace DynInvoke {

//
// Class: TypeName
//
// Purpose: pairing up an assembly name and the type/class name.
//
[ComVisible(false)]
public __gc class TypeName {

public:
  System::String* m_assembly;
  System::String* m_class;
  int     m_length;

  TypeName() { 
    m_assembly = String::Empty;
    m_class = String::Empty;
    m_length = 0;
  }

  void Print() {
    if (m_assembly && m_assembly != String::Empty ) {
      Console::Write("[");
      Console::Write(m_assembly);
      Console::Write("]");
    }
    Console::WriteLine(m_class);
  }
  
  int Length() { return m_length; }

  System::String* toStdString() {
    System::String* res = new System::String(m_class->ToCharArray());
    
    if (m_assembly && m_assembly != String::Empty ){
      res = String::Concat(res, S",");
      res = String::Concat(res, m_assembly);
    }
    return res;
  }
};

// 
// Class:   InvokeBridge
// 
// Purpose: Collection of (static) methods for dynamically creating
//          objects and accessing methods/fields on them. 
//
[ClassInterface(ClassInterfaceType::AutoDual),
GuidAttribute("39D497D9-60E0-3525-B7F2-7BC096D3A2A3"),
ComVisible(true)
]
public __gc class InvokeBridge {
public:
  InvokeBridge() {
    Assembly* corAss      = Assembly::Load("mscorlib.dll"); 
    System::String*  dir  = System::IO::Path::GetDirectoryName(corAss->Location);
   
    m_assemblies = new System::Collections::ArrayList();
   
    System::String* fs[] = System::IO::Directory::GetFiles(dir, "*.dll");
    for (int i=0;i < fs->Length; i++) {
      try {
	Assembly* tAss = Assembly::LoadFrom(fs[i]);
	m_assemblies->Add(tAss->FullName);
      } catch (Exception* e) {
	continue;
      }
    }
  }

  //
  // Method: CreateObject(String* assemName, String* objSpec, Object* args[])
  //
  // Purpose: Given a fully qualified name of a class/type, try
  //          to create an instance of it.
  //
  Object* CreateObject(System::String* assemName,
		       System::String* objSpec,
		       Object* args[]);
	  
  //
  // Method:  InvokeMethod
  // 
  // Purpose: Given a pointer to an already created object, look up
  //          one of its method. If found, invoke the method passing it
  //          'args' as arguments.
  //
  // Comments: the format of the method-spec is "methodName(type1,..,typeN)" [N>=0]
  //
  Object* InvokeMethod(Object* obj, 
		       System::String* methSpec,
		       Object* args[]);
			      
  //
  // Method:  InvokeStaticMethod
  // 
  // Purpose: Invoke a static method, given the fully qualified name
  //          of the method (and its arguments). If found, invoke the
  //          method passing it 'args' as arguments.
  //
  // Comments: the format of the method-spec is 
  //              "T1.T2.<..>.Tn.methodName(type1,..,typeN)" [N>=0]
  //
  Object* InvokeStaticMethod(System::String* assemName,
			     System::String* methSpec,
			     Object* args[]);
			      
  //
  // Method:  GetField
  //
  // Purpose: Fetch the (boxed) value of named field of a given object.
  //
  Object* GetField(Object* obj, System::String* fieldSpec);

  //
  // Method:  GetField
  //
  // Purpose: Fetch the (boxed) value of named static field.
  //
  Object* GetStaticField(System::String* clsName, 
			 System::String* fieldSpec);

  //
  // Method:  SetField
  //
  // Purpose: Replace the (boxed) value of named field of a given object.
  //
  void SetField(Object* obj, System::String* fieldSpec, Object* val);
	    
  //
  // Method:  SetStaticField
  //
  // Purpose: Replace the (boxed) value of named field of a given object.
  //
  void SetStaticField(System::String* clsName,
		      System::String* fieldSpec,
		      Object* val);
	    

  // 
  // Method:  NewString
  // 
  // Purpose: construct a System.String object copy in a manner that avoids
  //          COM Interop from deconstructing it to a BSTR.
  //
  System::Object* NewString( System::String* s);

  //
  // Method:  NewArgArray
  //
  // Purpose: create a new array for holding (boxed) arguments to constructors/
  //          methods.
  //
  Array* NewArgArray(int sz);
  
  //
  // Method: SetArg
  //
  // Purpose: set an entry in the argument vector.
  //
  void SetArg(Object* arr[], Object* val, int idx);

  //
  // Method: GetArg
  //
  // Purpose: get an entry in the argument vector.
  //
  Object* GetArg(Object* arr[], int idx);

  System::Type* InvokeBridge::GetType(System::String* typeName);

protected:
  System::Collections::ArrayList __gc* m_assemblies;
  Object* InvokeBridge::CreateInstance(TypeName* typeName,
				       Object* args[]);
};

} /* namespace */
