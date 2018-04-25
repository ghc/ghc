#include "Queue.h"

template<class T> inline Queue<T>::Queue(int sz) 
{ 
   Q = new T[sz];
   i = Q;
   j = Q; 
}

template<class T> inline Queue<T>::~Queue()
{
   delete Q;
}


template<class T> inline Queue<T>::QReset() 
{ 
   i = Q;
   j = Q; 
}

template<class T> inline void Queue<T>::Cons(T elem)             
{
   *j = elem;
   ++j;
}

template<class T> inline void Queue<T>::RemoveHead()      
{
   --j;
}

template<class T> inline void Queue<T>::RemoveLast()
{
   ++i;
}

template<class T> inline int Queue<T>::NotEmpty() const
{
    return (j > i); 
}


template<class T> inline int Queue<T>::NotSingle() const
{
    return (j > i+1); 
}



template<class T> inline T Queue<T>::Head() const
{
   return *(j-1); 
}

template<class T> inline T Queue<T>::Head1() const
{
   return *(j-2); 
}

template<class T> inline T Queue<T>::Last() const
{
   return *i; 
}

template<class T> inline T Queue<T>::Last1() const
{
   return *(i+1);
}

