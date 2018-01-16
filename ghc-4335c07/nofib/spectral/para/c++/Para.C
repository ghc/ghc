
#include "Para.h"

int Para::OptW;
int Para::MaxW;
int Para::TotWidth;
int Para::TotLen;
int Para::TL;
int Para::TW;
int Para::W;

inline Para::Para()
{
  WidthTl = 0;
  CostTl  = 0;
  LengthTail = 0;
}

inline int sqr(int n)
{
  return n*n;
}

Para::Para(const Para& P,int dummy)
{
  WidthTl = TW;
  LengthTail = TL;

  if(P.SingleP())
      CostTl = 0;
  else
      CostTl = P.CostTl + sqr(OptW - P.OldWidthHd());
}

inline int Para::SingleP() const
{
  return (LengthTail == 0);
}

inline int Para::LenTl() const
{
  return LengthTail;
}

inline int Para::OldWidthHd() const
{
   if (SingleP())
     return TW;
   else return TW - WidthTl - 1;
}

inline int Para::WidthHd() const
{
   if (SingleP()) 
      return TotWidth; 
   else return TotWidth - WidthTl - 1;
}

inline int Para::Cost() const
{
  if (SingleP())
    return 0;
  else
    return (CostTl + sqr (OptW - WidthHd()));
}

inline int min(int n, int m)
{
  if (n<=m)
    return n;
  else
    return m;
}

inline int ceildiv(int x, int y)
{
   return (x+y-1) / y;
}

   
inline int bf(const Para& p,const Para& q) 
{
  int wp0, wq0, rq0;
  wp0 = p.WidthHd();
  wq0 = q.WidthHd();
  rq0 = Para::MaxW - wq0 + 1;
  if (q.SingleP())
    { if (p.CostTl == 0)
        return min(Para::OptW - wp0,rq0);
      else return rq0; }
  else return min(rq0,
                  ceildiv(p.Cost() - q.Cost(), 2*(wq0-wp0)));
}

inline void SetW(int v)
{
   Para::W = v;
   Para::TotWidth = Para::W + 1 + Para::TW;
   Para::TotLen = 1 + Para::TL;
}

inline void ISetW(int v)
{
   Para::W = v;
   Para::TotWidth = v;
   Para::TotLen = 1;
}


inline void SetTots()
{
   Para::TW = Para::TotWidth;
   Para::TL = Para::TotLen;
}

inline void InitTots()
{
   Para::TW = 0;
   Para::TL = 0;
}

