//
// (c) 2002-2003, sof.
//
// Dynamic invocation helper classes. The details of how
// to access the .NET object model via the Reflection API
// is taken care of by Invoker.{h,cpp}
//
#include "Invoker.h"

namespace DynInvoke {

static TypeName* ParseType(String* str) {
    int curPos = 0;
    int endPos;

    //    Console::WriteLine("x{0}y", str);
    TypeName* typeName = new TypeName();

    if ( str->get_Chars(0) == '[' ) {
      endPos = str->IndexOf(']');
      curPos = endPos + 1;
      typeName->m_assembly = str->Substring(1,endPos-1);
      typeName->m_length = endPos+1;
    }
    String* delimStr = " ,()";
    Char delims __gc [] = delimStr->ToCharArray();

    endPos  = str->IndexOfAny(delims,curPos);
    //    Console::WriteLine("{0} {1} x{2}x", __box(endPos), __box(curPos), str);
    if ( endPos == -1 ) {
      typeName->m_class = str->Substring(curPos);
    } else {
      typeName->m_class = str->Substring(curPos,endPos-curPos);
    }

    //    typeName->m_class = str->Substring(curPos,endPos-curPos);
    typeName->m_length += endPos-curPos;

    return typeName;
}

// Method: GetType(String* typeName);
// 
// Purpose: Assembly-savvy version of Type::GetType()
//
Type* InvokeBridge::GetType(String* typeName) {

  try {
    Type* t = Type::GetType(typeName);
    if (t) return t;
  } catch (Exception*) {
    ;
  }

  for (int i=0;i < InvokeBridge::m_assemblies->Count; i++) {
     try {
       String* stuff = String::Format("{0},{1}",typeName,InvokeBridge::m_assemblies->get_Item(i)->ToString());
       //       Console::WriteLine(stuff);
       Type* t = Type::GetType(stuff);
       if (t) {
	 return t;
       }
     } catch (Exception*) {
       continue;
     }
  }
  return 0;
}

//
// Method:  CreateInstance(String* typeName, Object* [])
//
// Purpose: Assembly-savvy invocation of Activator::CreateInstance
Object* InvokeBridge::CreateInstance(TypeName* typeName,
				     Object* args[]) {

  Object* instance = 0;
  Type*   t = InvokeBridge::GetType(typeName->toStdString());

  //  Console::WriteLine("x{0} y{1}", typeName->toStdString(), t);
  if (!t) {
    try {
      Assembly* localA = Assembly::LoadFrom(typeName->m_assembly);
      t = localA->GetType(typeName->m_class);
    } catch (Exception* e) {
      ;
    }
  }
  
  if (!t) {
    try {
      AppDomain* currentDomain = AppDomain::CurrentDomain;
      
      //      Assembly* stuff[] = currentDomain->GetAssemblies();
      //      for (int i=0;i < stuff.Length; i++) {
      //	Console::WriteLine("x{0} y{1}", stuff[i]->ToString(), stuff[i]->FullName);
      //      }
      //      Console::WriteLine("x{0} y{1}", typeName->toStdString(), t);
      Assembly* localA = Assembly::LoadWithPartialName("HugsAssembly");
      t = localA->GetType(typeName->m_class);
      //      Console::WriteLine("x{0} y{1}", typeName->toStdString(), t);
    } catch (Exception*) {
      ;
    }
  }

  if (t) {
    try {
      Object* o =Activator::CreateInstance(t,(Object* [])args);
      return o;
    } catch (Exception* e) {
      Console::WriteLine("Failure: {0}", e);
      return 0;
    }
  }
}

//
// Method: CreateObject(String* objSpec, Object* args[])
//
// Purpose: Given a fully qualified name of a class/type, try
//          to create an instance of it.
//
Object* InvokeBridge::CreateObject(String* assemName,
				   String* objSpec,
				   Object* args[]) {
 
  Object* instance = 0;

  // Unravel the name of the class/type.
  TypeName* typeName = ParseType(objSpec);
  
  if (assemName != 0 && assemName->Length > 0) {
    typeName->m_assembly = assemName;
  }

  // Try creating the instance..
  try {
    instance = InvokeBridge::CreateInstance(typeName,(Object* [])args);
  } catch (Exception* e) {
    Console::WriteLine("Unable to create instance \"{0}\" {1}", objSpec, e);
    throw(e);
  }
  if (!instance) {
    Console::WriteLine("Unable to create instance \"{0}\"", objSpec);
  }
  return instance;
}

//
// Method:  InvokeMethod
// 
// Purpose: Given a pointer to an already created object, look up
//          one of its method. If found, invoke the method passing it
//          'args' as arguments.
//
Object*
InvokeBridge::InvokeMethod(Object* obj, 
			   String* methName,
			   Object* args[]) {
  // Get the methods from the type
  MethodInfo* methods __gc[] = obj->GetType()->GetMethods();
  MethodInfo* mInfo;

  if (!methods) {
    Console::WriteLine("InvokeMethod: No matching types found");
    return 0;
  }
			
  System::Reflection::BindingFlags flgs 
    = (System::Reflection::BindingFlags) // why do I need to cast?
      (System::Reflection::BindingFlags::Public       |
       System::Reflection::BindingFlags::NonPublic    |
       System::Reflection::BindingFlags::Instance     |
       System::Reflection::BindingFlags::Static       |
       System::Reflection::BindingFlags::InvokeMethod);
    
  /* Caller is assumed to catch any exceptions raised. */
  return obj->GetType()->InvokeMember(methName,
				      flgs,
				      0,
				      obj,
				      (Object __gc* [])args);
}

//
// Method:  InvokeStaticMethod
// 
// Purpose: Invoke a static method, given the fully qualified name
//          of the method (and its arguments). If found, invoke the
//          method passing it 'args' as arguments.
//
Object* InvokeBridge::InvokeStaticMethod(String* assemName,
					 String* typeAndMethName,
					 Object* args[]) {

  // Get the methods from the type
  MethodInfo* methods __gc[];
  MethodInfo* mInfo;

  int lastDot = typeAndMethName->LastIndexOf('.');
  String* className = typeAndMethName->Substring(0,lastDot);
  String* methName  = typeAndMethName->Substring(lastDot+1);

  // Unravel the name of the class/type.
  TypeName* typeName = ParseType(className);
  Type* t;
  
  if (assemName != 0 && assemName->Length > 0) {
    typeName->m_assembly = assemName;
  }
  
  try {
    t = InvokeBridge::GetType(typeName->toStdString());
    
    if (!t) {
      try {
	Assembly* localA = Assembly::LoadFrom(typeName->m_assembly);
	t = localA->GetType(typeName->m_class);
	//	Console::WriteLine("InvokeStaticMethod: Type {0} found", t);
      } catch (Exception* e) {
	;
      }
    }

    if (t) {
      methods = t->GetMethods();
    } else {
      Console::WriteLine("InvokeStaticMethod: Type {0} not found", className);
      return 0;
    }
  } catch (Exception *e) {
      Console::WriteLine("InvokeStaticMethod: Type {0} not found", className);
      throw(e);
  }

  System::Reflection::BindingFlags flgs 
    = (System::Reflection::BindingFlags) // why do I need to cast?
      (System::Reflection::BindingFlags::DeclaredOnly |
       System::Reflection::BindingFlags::Public       |
       System::Reflection::BindingFlags::NonPublic    |
       System::Reflection::BindingFlags::Static       |
       System::Reflection::BindingFlags::InvokeMethod);
    
  return t->InvokeMember(methName,
			 flgs,
			 0,
			 0,
			 (Object __gc* [])args);
}

//
// Method:  GetField
//
// Purpose: Fetch the (boxed) value of named field of a given object.
//
Object* InvokeBridge::GetField(Object* obj, System::String* fieldName) {

  FieldInfo* fInfo = obj->GetType()->GetField(fieldName);
  return fInfo->GetValue(obj);
}

//
// Method:  GetStaticField
//
// Purpose: Fetch the (boxed) value of named static field.
//
Object* InvokeBridge::GetStaticField(System::String* clsName,
				     System::String* fieldName) {

  Type* ty = InvokeBridge::GetType(clsName);
  System::Reflection::BindingFlags static_field_flgs 
    = (System::Reflection::BindingFlags)
    (System::Reflection::BindingFlags::Public       |
     System::Reflection::BindingFlags::NonPublic    |
     System::Reflection::BindingFlags::FlattenHierarchy |
     System::Reflection::BindingFlags::Static);

  FieldInfo* fInfo = ty->GetField(fieldName, static_field_flgs);
  return fInfo->GetValue(0); // according to doc, ok to pass any val here.
}

//
// Method:  SetField
//
// Purpose: Replace the (boxed) value of named field of a given object.
//
void InvokeBridge::SetField(Object* obj, System::String* fieldName, Object* val) {

  FieldInfo* fInfo = obj->GetType()->GetField(fieldName);
  fInfo->SetValue(obj,val);
  return;
}

//
// Method:  SetStaticField
//
// Purpose: Replace the (boxed) value of named static field.
//
void InvokeBridge::SetStaticField(System::String* clsName,
				  System::String* fieldName,
				  Object* val) {

  Type* ty = InvokeBridge::GetType(clsName);
  System::Reflection::BindingFlags static_field_flgs 
    = (System::Reflection::BindingFlags)
    (System::Reflection::BindingFlags::Public       |
     System::Reflection::BindingFlags::NonPublic    |
     System::Reflection::BindingFlags::FlattenHierarchy |
     System::Reflection::BindingFlags::Static);
  
  FieldInfo* fInfo = ty->GetField(fieldName,static_field_flgs);
  fInfo->SetValue(0,val);
  return;
}

Object* InvokeBridge::NewString(System::String* s)
{
  System::String* c = System::String::Copy(s);
  return dynamic_cast<Object*>(c);
}

Array* InvokeBridge::NewArgArray(int sz)
{
 return Array::CreateInstance(__typeof(Object), sz); 
}

void InvokeBridge::SetArg(Object* arr[], Object* val, int idx)
{
 arr->SetValue(val,idx);
}

Object* InvokeBridge::GetArg(Object* arr[], int idx)
{
 return arr->GetValue(idx);
}

} /* namespace */
