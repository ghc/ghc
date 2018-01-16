
class Para 
{      
   static int TW, W, TL, TotWidth, TotLen;

   int WidthTl, CostTl;

   int LengthTail;

   int OldWidthHd() const;
  
public:

   static int OptW, MaxW;

   Para();
   Para(const Para&,int);   

   int SingleP() const;
   int LenTl() const;
   int WidthHd() const;
   int Cost() const;

   friend int bf(const Para&,const Para&);

   friend void SetW(int);
   friend void ISetW(int v);
   friend void SetTots();
   friend void InitTots();

};   

