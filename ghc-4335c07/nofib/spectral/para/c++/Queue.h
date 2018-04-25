
// Queue.h 


template<class T>
class Queue 
{      
   T* Q;
   T* i;             
   T* j;            
  
public:

   Queue(int);    
   ~Queue();    

   QReset();

   void Cons(T elem);             
   void RemoveHead();      
   void RemoveLast();     

   int NotEmpty() const;
   int NotSingle() const;

   T Head() const;    
   T Head1() const;    
   T Last() const;      
   T Last1() const;      

};   

