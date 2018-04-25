#include <stdlib.h>
#include <fstream.h>
#include "Para.C"
#include "Queue.C"
#include "Thin.C"

const int MaxNoWords = 5000;
const int MaxWordLen = 100;

typedef char myword[MaxWordLen];

int i;
int first=1; 
int WordLengths[MaxNoWords];
int Best[MaxNoWords];
myword Words[MaxNoWords];

int ReadPara(ifstream&);
void WritePara(ofstream&);

int main (int argc, char* argv[])
{
   Para::MaxW = atoi(argv[1]);
   Para::OptW = Para::MaxW - (Para::MaxW / 10);
   ifstream from(argv[2]);
   ofstream to(argv[3]);
   Thin Q(MaxNoWords);
   while (ReadPara(from))
    {
       InitTots();
       Q.Reset(WordLengths[i]);
       Best[i-1] = Q.TheBest();
       for (int k=(i-1); k>0; k--)
          {
             Q.Add(WordLengths[k]);
             Q.DropNoFit();
             Q.Trim();
             Best[k-1] = Q.TheBest();
          };
        WritePara(to);   
    };
}

inline int ReadPara(ifstream& from)
{
   char c;

   while (from.get(c) && (c <= ' ')) { /* skip */ };
   if (from.eof())
      return 0;
   else
     {int* wl = &WordLengths[0];
      i = 0;
      myword* w = &Words[0];
      char* p;
      do { i++; wl++; w++;
           p = (char*)w;
           do { *p = c; p++; }  
           while (from.get(c) && (c > ' '));
           *wl = (int)p - (int)w;
           int NL = (c =='\n');
           while (from.get(c) && 
             (((c <= ' ') && (c !='\n')) || (NL = ((c=='\n') && !NL))));
          }
      while  ((c != '\n') && !from.eof());
      return 1;
      }
}

inline void WritePara(ofstream& to)
{

   int m,w,n;
   m = 0;
   if (first)
     first=0;
   else
     to.put('\n');
   while (m<i)
    { w = Best[m];
      n = (i-w) - m;
      for (int j=1; j< n; j++)
        {
          for (int k=0; k< WordLengths[m+j]; k++)
            to.put(Words[m+j][k]);
          to.put(' ');
        };
      for (int k=0; k< WordLengths[m+n]; k++)
        to.put(Words[m+n][k]);
      to.put('\n');
      m = i-w;
    }
}
      

