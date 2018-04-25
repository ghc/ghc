
//    Thin.h


class Thin : Queue<Para> 
{      

public:

   Thin(int);

   Reset(int);

   int TheBest() const;

   void Add(int);

   void Trim();

   void DropNoFit();
};

