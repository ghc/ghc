
#include "Thin.h"

inline int Thin::TheBest() const
{
  return Last().LenTl();
}

inline Thin::Thin(int maxSize)
 : Queue<Para>(maxSize)
{ }

inline Thin::Reset(int w)
{
  QReset();
  Para P;
  ISetW(w);
  Cons(P);
  SetTots();
}


inline void Thin::Add(int w)
{
  SetW(w);
  Para r(Last(),0);
  while (NotSingle() && (bf(r,Head()) <= bf(Head(),Head1())) )
    RemoveHead(); 
  Cons(r);
}

inline void Thin::Trim()
{
  while (NotSingle() && (Last1().Cost() <= Last().Cost()))
    RemoveLast();
  SetTots();
}  

inline void Thin::DropNoFit()
{
  while (NotEmpty() && Last().WidthHd() > Para::MaxW)
    RemoveLast();
}

