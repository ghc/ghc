// Big Integer class for .NET
// (c) The GHC Team 2001

// TODO:
// Constructors from Single, Double, Currency, String
//

using System;
using System.Diagnostics;

public class BigInteger : IComparable, IConvertible, IFormattable {

 int sign;
 int size;
 int used;
 byte[] body;

 const int B_BASE = 256;
 const double B_BASE_FLT = 256.0;


 // Constructors

 public BigInteger() {   
#if BIGINTEGER_DEBUG
   Debug.Assert(this.sane());
#endif
 }

 public BigInteger(Int32 n) {
   this.size = 4;
   this.body = new byte[this.size];
   this.sign = this.used = 0;
   if (n == 0) {
#if BIGINTEGER_DEBUG
     Debug.Assert(this.sane());
#endif
     return;
   }
   if (n < 0) {
     this.sign = -1;
   }
   else {
     this.sign = 1;
   }
   if (n < 0) {
     n = -n;
   }
   while (n != 0) {
     this.body[this.used] = (byte)(n % B_BASE);
     n /= B_BASE;
     this.used++;
   }
#if BIGINTEGER_DEBUG
   Debug.Assert(this.sane());
#endif
 }

 public BigInteger(UInt32 n) {
   this.size = 4;
   this.body = new byte[this.size];
   this.sign = this.used = 0;
   if (n == 0) {
#if BIGINTEGER_DEBUG
     Debug.Assert(this.sane());
#endif
     return;
   }
   this.sign = 1;
   while (n != 0) {
     this.body[this.used] = (byte)(n % B_BASE);
     n /= B_BASE;
     this.used++;
   }
#if BIGINTEGER_DEBUG
   Debug.Assert(this.sane());
#endif
 }

 public BigInteger(Int64 n) {
   this.size = 8;
   this.body = new byte[this.size];
   this.sign = this.used = 0;
   if (n == 0) {
#if BIGINTEGER_DEBUG
     Debug.Assert(this.sane());
#endif
     return;
   }
   if (n < 0) {
     this.sign = -1;
   }
   else {
     this.sign = 1;
   }
   if (n < 0) {
     n = -n;
   }
   while (n != 0) {
     this.body[this.used] = (byte)(n % B_BASE);
     n /= B_BASE;
     this.used++;
   }
#if BIGINTEGER_DEBUG
   Debug.Assert(this.sane());
#endif
 }

 public BigInteger(UInt64 n) {
   this.size = 8;
   this.body = new byte[this.size];
   this.sign = this.used = 0;
   if (n == 0) {
#if BIGINTEGER_DEBUG
     Debug.Assert(this.sane());
#endif
     return;
   }
   this.sign = 1;
   while (n != 0) {
     this.body[this.used] = (byte)(n % B_BASE);
     n /= B_BASE;
     this.used++;
   }
#if BIGINTEGER_DEBUG
   Debug.Assert(this.sane());
#endif
 }

 // NOTE: This only works currectly if B_BASE >= 10
 // TODO: Turn this into a Parse method taking a String
 public BigInteger (char [] str) {
   int sign, d, t, i, j, carry;

   for (i = 0; str[i] != 0; i++) {
   }
   this.size = i;
   this.body = new byte[this.size];
   this.sign = this.used = 0;
   sign = 1;
   i = 0;
   if (str[0] == '-') {
     i++;
     sign = -1;
   }

   while (Char.IsDigit(str[i])) {

     // multiply this by 10
     carry = 0;
     for (j = 0; j < this.used; j++) {
       t = 10 * this.body[j] + carry;
       this.body[j] = (byte)(t % B_BASE);
       carry = t / B_BASE;
     }
#if BIGINTEGER_DEBUG
     Debug.Assert(carry < B_BASE);
#endif
     if (carry > 0) {
       this.body[this.used++] = (byte)carry;
     }
     // add a digit on
     d = str[i] - '0';
     i++;

     carry = d;
     for (j = 0; j < this.used; j++) {
       carry += this.body[j];
       this.body[j] = (byte)(carry % B_BASE);
       carry /= B_BASE;
       if (carry == 0) {
	 break;
       }
     }
     if (carry > 0) {
       this.body[this.used++] = (byte)carry;
     }
   }

   this.sign = sign;
#if BIGINTEGER_DEBUG
   Debug.Assert(this.sane());
#endif
 }

 
 // Constants
 static readonly BigInteger Zero = new BigInteger(0);
 static readonly BigInteger One = new BigInteger(1);
 static readonly BigInteger MinusOne = new BigInteger(-1);


 // Conversions

 // Implicit
 public static implicit operator BigInteger(SByte n) {
   return new BigInteger((Int32)n);
 }

 public static implicit operator BigInteger(Byte n) {
   return new BigInteger((UInt32)n);
 }

 public static implicit operator BigInteger(Int16 n) {
   return new BigInteger((Int32)n);
 }

 public static implicit operator BigInteger(UInt16 n) {
   return new BigInteger((UInt32)n);
 }

 public static implicit operator BigInteger(Char n) {
   return new BigInteger((Int32)n);
 }

 public static implicit operator BigInteger(Int32 n) {
   return new BigInteger(n);
 }

 public static implicit operator BigInteger(UInt32 n) {
   return new BigInteger(n);
 }

 public static implicit operator BigInteger(Int64 n) {
   return new BigInteger(n);
 }

 public static implicit operator BigInteger(UInt64 n) {
   return new BigInteger(n);
 }

 // Explicit
 
 public static Boolean ToBoolean(BigInteger n) {
   throw new InvalidCastException();
 }

 public static explicit operator Boolean(BigInteger n) {
   return ToBoolean(n);
 }

 Boolean IConvertible.ToBoolean(IFormatProvider p) {
   return ToBoolean(this);
 }
 
 public static DateTime ToDateTime(BigInteger n) {
   throw new InvalidCastException();
 }

 DateTime IConvertible.ToDateTime(IFormatProvider p) {
   return ToDateTime(this);
 }
 
 public static explicit operator DateTime(BigInteger n) {
   return ToDateTime(n);
 }

 public static SByte ToSByte(BigInteger n) {
   SByte res;
   if (n.sign == 0) {
     return 0;
   }
   res = 0;
   if (n.used > 0) {
     res = (SByte)n.body[0];
   }
   if (n.sign < 0) {
     res = (SByte)(-res);
   }
   return res;
 }

 SByte IConvertible.ToSByte(IFormatProvider p) {
   return ToSByte(this);
 }
 
 public static explicit operator SByte(BigInteger n) {
   return ToSByte(n);
 }

 public static Byte ToByte(BigInteger n) {
   Byte res;
   if (n.sign == 0) {
     return 0;
   }
   res = 0;
   if (n.used > 0) {
     res = (Byte)n.body[0];
   }
   return res;
 }

 Byte IConvertible.ToByte(IFormatProvider p) {
   return ToByte(this);
 }
 
 public static explicit operator Byte(BigInteger n) {
   return ToByte(n);
 }

 public static Int16 ToInt16(BigInteger n) {
   int i, d;
   Int16 res;
   if (n.sign == 0) {
     return 0;
   }
   res = 0;
   for (i = n.used-1; i >= 0; i--) {
     d = n.body[i];
     res = (Int16)(res * B_BASE + d);
   }
   if (n.sign < 0) {
     res = (Int16)(-res);
   }
   return res;
 }

 Int16 IConvertible.ToInt16(IFormatProvider p) {
   return ToInt16(this);
 }
 
 public static explicit operator Int16(BigInteger n) {
   return ToInt16(n);
 }

 public static UInt16 ToUInt16(BigInteger n) {
   int i, d;
   UInt16 res;
   if (n.sign == 0) {
     return 0;
   }
   res = 0;
   for (i = n.used-1; i >= 0; i--) {
     d = n.body[i];
     res = (UInt16)(res * B_BASE + d);
   }
   return res;
 }

 UInt16 IConvertible.ToUInt16(IFormatProvider p) {
   return ToUInt16(this);
 }
 
 public static explicit operator UInt16(BigInteger n) {
   return ToUInt16(n);
 }

 public static Char ToChar(BigInteger n) {
   throw new InvalidCastException();
 }

 Char IConvertible.ToChar(IFormatProvider p) {
   return ToChar(this);
 }
 
 public static explicit operator Char(BigInteger n) {
   return ToChar(n);
 }

 public static Int32 ToInt32(BigInteger n) {
   int i, d;
   Int32 res;
   if (n.sign == 0) {
     return 0;
   }
   res = 0;
   for (i = n.used-1; i >= 0; i--) {
     d = n.body[i];
     res = res * B_BASE + d;
   }
   if (n.sign < 0) {
     res = -res;
   }
   return res;
 }

 Int32 IConvertible.ToInt32(IFormatProvider p) {
   return ToInt32(this);
 }
 
 public static explicit operator Int32(BigInteger n) {
   return ToInt32(n);
 }

 public static UInt32 ToUInt32(BigInteger n) {
   int i, d;
   UInt32 res;
   if (n.sign == 0) {
     return 0;
   }
   res = 0;
   for (i = n.used-1; i >= 0; i--) {
     d = n.body[i];
     res = res * B_BASE + (UInt32)d;
   }
   return res;
 }

 UInt32 IConvertible.ToUInt32(IFormatProvider p) {
   return ToUInt32(this);
 }
 
 public static explicit operator UInt32(BigInteger n) {
   return ToUInt32(n);
 }

 public static Int64 ToInt64(BigInteger n) {
   int i, d;
   Int64 res;
   if (n.sign == 0) {
     return 0;
   }
   res = 0;
   for (i = n.used-1; i >= 0; i--) {
     d = n.body[i];
     res = res * B_BASE + d;
   }
   if (n.sign < 0) {
     res = -res;
   }
   return res;
 }

 Int64 IConvertible.ToInt64(IFormatProvider p) {
   return ToInt64(this);
 }
 
 public static explicit operator Int64(BigInteger n) {
   return ToInt64(n);
 }

 public static UInt64 ToUInt64(BigInteger n) {
   int i, d;
   UInt64 res;
   if (n.sign == 0) {
     return 0;
   }
   res = 0;
   for (i = n.used-1; i >= 0; i--) {
     d = n.body[i];
     res = res * B_BASE + (UInt64)d;
   }
   return res;
 }

 UInt64 IConvertible.ToUInt64(IFormatProvider p) {
   return ToUInt64(this);
 }
 
 public static explicit operator UInt64(BigInteger n) {
   return ToUInt64(n);
 }

 public static Decimal ToDecimal(BigInteger n) {
   int i, d;
   Decimal res;
   if (n.sign == 0) {
     return 0;
   }
   res = 0;
   for (i = n.used-1; i >= 0; i--) {
     d = n.body[i];
     res = res * B_BASE + (Decimal)d;
   }
   return res;
 }

 Decimal IConvertible.ToDecimal(IFormatProvider p) {
   return ToDecimal(this);
 }
 
 public static explicit operator Decimal(BigInteger n) {
   return ToDecimal(n);
 }

 public static Single ToSingle(BigInteger n) {
   int i, d;
   Single res;
   if (n.sign == 0) {
     return 0.0F;
   }
   res = 0.0F;
   for (i = n.used-1; i >= 0; i--) {
     d = n.body[i];
     res = res * (Single)B_BASE_FLT + d;
   }
   if (n.sign < 0) {
     res = -res;
   }
   return res;
 }

 Single IConvertible.ToSingle(IFormatProvider p) {
   return ToSingle(this);
 }
 
 public static explicit operator Single(BigInteger n) {
   return ToSingle(n);
 }

 public static Double ToDouble(BigInteger n) {
   int i, d;
   Double res;
   if (n.sign == 0) {
     return 0.0;
   }
   res = 0.0;
   for (i = n.used-1; i >= 0; i--) {
     d = n.body[i];
     res = res * B_BASE_FLT + d;
   }
   if (n.sign < 0) {
     res = -res;
   }
   return res;
 }

 Double IConvertible.ToDouble(IFormatProvider p) {
   return ToDouble(this);
 }
 
 public static explicit operator Double(BigInteger n) {
   return ToDouble(n);
 }

 override public String ToString() {
   int i;
   Console.Write ( "sign={0}  used={1}  size={2}   ", this.sign, this.used, this.size );
   for (i = this.used-1; i >= 0; i--) {
     Console.Write ( "{0} ", (int)(this.body[i]) );
   }
   Console.Write ( "\n" );
   return "(some number or other)";
 }

 public String ToString(IFormatProvider p) {
   return ToString(null, p);
 }

 public String ToString(String fmt) {
   return this.ToString();
 }

 public String ToString(String fmt, IFormatProvider p) {
   throw new InvalidCastException();
 }

 public Object ToType(Type ty, IFormatProvider n) {
   throw new InvalidCastException();
 }
 
 // public object GetFormat(Type 

 public TypeCode GetTypeCode() {
   return TypeCode.Int64;
 }
 
 // Basics

 bool sane() {
   if (this.sign == 0 && this.used != 0) {
     return false;
   }
   if (this.sign != -1 && this.sign != 0 && this.sign != 1) {
     return false;
   }
   if (this.used < 0) {
     return false;
   }
   if (this.size < 0) {
     return false;
   }
   if (this.used > this.size) {
     return false;
   }
   if (this.used == 0) {
     return true;
   }
   if (this.body[this.used-1] == 0) {
     return false;
   }
   return true;
 }

 void u_renormalise() {
   while (this.used > 0 && this.body[this.used-1] == 0) {
     this.used--;
   }
   if (this.used == 0) {
     this.sign = 0;
   }
   else {
     this.sign = 1;
   }
 }
 

 public void renormalise() {
   while (this.used > 0 && this.body[this.used-1] == 0) {
     this.used--;
   }
   if (this.used == 0) {
     this.sign = 0;
   }
 }

 
 // Size of things

 static int maxused_addsub ( BigInteger x, BigInteger y ) {
#if BIGINTEGER_DEBUG
   Debug.Assert(x.sane());
   Debug.Assert(y.sane());
#endif
   return 1 + (x.used > y.used ? x.used : y.used);
 }

 static int maxused_mul ( BigInteger x, BigInteger y ) {
#if BIGINTEGER_DEBUG
   Debug.Assert(x.sane());
   Debug.Assert(y.sane());
#endif
   return x.used + y.used;
 }

 static int maxused_qrm ( BigInteger x, BigInteger y ) {
#if BIGINTEGER_DEBUG
   Debug.Assert(x.sane());
   Debug.Assert(y.sane());
#endif
   return (x.used > y.used ? x.used : y.used);
 }

 int maxused_neg() {
#if BIGINTEGER_DEBUG
   Debug.Assert(this.sane());
#endif
   return this.used;
 }


 // Signed ops

 // A helper for signed + and -.  sdiff(x,y) ignores the signs of x and y
 // sets p to the signed value abs(x)-abs(y).
 static void sdiff(BigInteger x, BigInteger y, BigInteger res) {
   int t;
#if BIGINTEGER_DEBUG
   Debug.Assert(x.sane());
   Debug.Assert(y.sane());
   Debug.Assert(res.size == maxused_addsub(x,y));
#endif
   t = ucmp(x,y);
   if (t == 0) {
     res.sign = res.used = 0;
     return;
   }
   if (t == -1) {
     // x < y
     usub(y,x,res);
     res.sign = -1;
   }
   else {
     // x > y
     usub(x,y,res);
     res.sign = 1;
   }
#if BIGINTEGER_DEBUG
   Debug.Assert(res.sane());
#endif
 }

 public BigInteger Negate() {
#if BIGINTEGER_DEBUG
   Debug.Assert(this.sane());
#endif
   BigInteger res = new BigInteger();
   res.size = this.used;
   res.body = new byte[res.used];
   res.used = this.used;
   for (int i = 0; i < this.used; i++) {
     res.body[i] = this.body[i];
   }
   res.sign = -(this.sign);
   return res;
 }

 public static BigInteger Add(BigInteger x, BigInteger y) {
#if BIGINTEGER_DEBUG
   Debug.Assert(x.sane());
   Debug.Assert(y.sane());
#endif
   BigInteger res = new BigInteger();
   res.size = maxused_addsub(x, y);
   res.used = res.sign = 0;
   
   if ( (x.sign >= 0 && y.sign >= 0) ||
	(x.sign < 0  && y.sign < 0)) {
     // same sign; add magnitude and clone sign
     uadd(x,y,res);
     if (x.sign < 0 && res.sign != 0) {
       res.sign = -1;
     }
   } 
   else {
     // signs differ; use sdiff
     if (x.sign >= 0 && y.sign < 0) {
       sdiff(x,y,res);
     }
     else {
#if BIGINTEGER_DEBUG
       Debug.Assert(x.sign < 0 && y.sign >= 0);
#endif
       sdiff(y,x,res);
     }
   }
#if BIGINTEGER_DEBUG
   Debug.Assert(res.sane());
#endif
   return res;
 }
 
 public BigInteger Increment() {
   return this + 1;
 }
 
 public static BigInteger Sub(BigInteger x, BigInteger y) {
#if BIGINTEGER_DEBUG
   Debug.Assert(x.sane());
   Debug.Assert(y.sane());
#endif
   BigInteger res = new BigInteger();
   res.size = maxused_addsub(x, y);
   res.used = res.sign = 0;

   if ( (x.sign >= 0 && y.sign < 0) ||
        (x.sign < 0  && y.sign >= 0)) {
     // opposite signs; add magnitudes and clone sign of x
     uadd(x,y,res);
#if BIGINTEGER_DEBUG
     Debug.Assert(res.sign != 0);
#endif
     if (x.sign < 0) {
       res.sign = -1;
     }
   } 
   else
     // signs are the same; use sdiff
     if (x.sign >= 0 && y.sign >= 0) {
       sdiff(x,y,res);
     }
     else {
#if BIGINTEGER_DEBUG
       Debug.Assert(x.sign < 0 && y.sign < 0);
#endif
       sdiff(y,x,res);
     }
#if BIGINTEGER_DEBUG
   Debug.Assert(res.sane());
#endif
   return res;
 }

 public BigInteger Decrement() {
   return this - 1;
 }
 
 public static BigInteger Multiply(BigInteger x, BigInteger y) {
#if BIGINTEGER_DEBUG
   Debug.Assert(x.sane());
   Debug.Assert(y.sane());
#endif
   BigInteger res = new BigInteger();
   res.size = maxused_mul(x, y);
   res.body = new byte[res.size];
   res.used = res.sign = 0;

   if (x.sign == 0 || y.sign == 0) {
     res.sign = res.used = 0;
#if BIGINTEGER_DEBUG
     Debug.Assert(res.sane());
#endif
     return res;
   }
   umul(x,y,res);
   if (x.sign != y.sign) {
     res.sign = -1;
   }
#if BIGINTEGER_DEBUG
   Debug.Assert(res.sane());
#endif
   return res;
 }

 public static BigInteger Divide(BigInteger x, BigInteger y) {
   BigInteger q, r;
   QuotientRemainder(x, y, out q, out r);
   return q;
 }
 
 public static BigInteger Remainder(BigInteger x, BigInteger y) {
   BigInteger q, r;
   QuotientRemainder(x, y, out q, out r);
   return r;
 }

 public static Int32 Compare(BigInteger x, BigInteger y) {
#if BIGINTEGER_DEBUG
   Debug.Assert(x.sane());
   Debug.Assert(y.sane());
#endif
   if (x.sign < y.sign) {
     return -1;
   }
   if (x.sign > y.sign) {
     return 1;
   }
#if BIGINTEGER_DEBUG
   Debug.Assert(x.sign == y.sign);
#endif
   if (x.sign == 0) {
     return 0;
   }
   if (x.sign == 1) {
     return ucmp(x,y);
   }
   else {
     return ucmp(y,x);
   }
 }

 public Int32 CompareTo(Object o) {
   return Compare(this, (BigInteger)o);
 }
 
 public static Boolean Equals(BigInteger x, BigInteger y) {
   return Compare(x, y) == 0;
 }

 override public Boolean Equals(Object o) {
   return this == (BigInteger)o;
 }

 override public Int32 GetHashCode() {
   int i;
   UInt32 h = 0;
   for (i = 0; i < this.used; i++) {
     h = (h << 4) + this.body[i];
     UInt32 g = h & 0xf0000000;
     if (g != 0) {
       h ^= g >> 24;
       h ^= g;
     }
   }
   return (Int32)h;
 }
 
 // Overloaded operators

 public static BigInteger operator +(BigInteger x) {
   return x;
 }
 
 public static BigInteger operator -(BigInteger x) {
   return x.Negate();
 }
 
 public static BigInteger operator +(BigInteger x, BigInteger y) {
   return Add(x, y);
 }
 
 public static BigInteger operator -(BigInteger x, BigInteger y) {
   return Sub(x, y);
 }
 
 public static BigInteger operator ++(BigInteger x) {
   return x + 1;
 }
 
 public static BigInteger operator --(BigInteger x) {
   return x - 1;
 }
 
 public static BigInteger operator *(BigInteger x, BigInteger y) {
   return Multiply(x, y);
 }

 public static BigInteger operator /(BigInteger x, BigInteger y) {
   return Divide(x, y);
 }

 public static BigInteger operator %(BigInteger x, BigInteger y) {
   return Remainder(x, y);
 }

 public static Boolean operator ==(BigInteger x, BigInteger y) {
   return Equals(x, y);
 }

 public static Boolean operator !=(BigInteger x, BigInteger y) {
   return !Equals(x, y);
 }
 public static Boolean operator <(BigInteger x, BigInteger y) {
   return Compare(x, y) == -1;
 }

 public static Boolean operator <=(BigInteger x, BigInteger y) {
   return Compare(x, y) < 1;
 }
 
 public static Boolean operator >(BigInteger x, BigInteger y) {
   return Compare(x, y) == 1;
 }

 public static Boolean operator >=(BigInteger x, BigInteger y) {
   return Compare(x, y) > 0;
 }

 
 // Quotient and remainder (private)
 
 public static void QuotientRemainder(BigInteger x, BigInteger y, out BigInteger q, out BigInteger r) {
#if BIGINTEGER_DEBUG
   Debug.Assert(x.sane());
   Debug.Assert(y.sane());
#endif

   if (y.sign == 0) {
     throw(new System.DivideByZeroException());
   }

   if (x.sign == 0) {
     q = new BigInteger();
     r = new BigInteger();
     q.used = r.used = q.sign = r.sign = 0;
#if BIGINTEGER_DEBUG
     Debug.Assert(q.sane());
     Debug.Assert(r.sane());
#endif
     return;
   }

   uqrm(x, y, out q, out r);
   if (x.sign != y.sign && q.sign != 0) {
     q.sign = -1;
   }
   if (x.sign == -1 && r.sign != 0) {
     r.sign = -1;
   }

#if BIGINTEGER_DEBUG
   Debug.Assert(q.sane());
   Debug.Assert(r.sane());
#endif
 }

 
 // Unsigned ops (private)

 static int ucmp(BigInteger x, BigInteger y) {
   int i;
#if BIGINTEGER_DEBUG
   Debug.Assert(x.sane());
   Debug.Assert(y.sane());
#endif
   if (x.used < y.used) {
     return -1;
   }
   if (x.used > y.used) {
     return 1;
   }
   for (i = x.used-1; i >= 0; i--) {
     if (x.body[i] < y.body[i]) {
       return -1;
     }
     if (x.body[i] > y.body[i]) {
       return 1;
     }
   }
   return 0;
 }

 static void uadd ( BigInteger x, BigInteger y, BigInteger res ) {
   int c, i, t, n;
   BigInteger longer;

#if BIGINTEGER_DEBUG
   Debug.Assert(x.sane());
   Debug.Assert(y.sane());
   Debug.Assert (res.size == maxused_addsub(x,y));
#endif
   res.used = res.size;
   res.body[res.used-1] = 0;

   if (x.used > y.used) {
     n = y.used;
     longer = x;
   }
   else {
     n = x.used;
     longer = y;
   }

   c = 0;
   for (i = 0; i < n; i++) {
     t = x.body[i] + y.body[i] + c;
     if (t >= B_BASE) {
       res.body[i] = (byte)(t-B_BASE);
       c = 1;
     }
     else {
       res.body[i] = (byte)t;
       c = 0;
     }
   }

   for (i = n; i < longer.used; i++) {
     t = longer.body[i] + c;
     if (t >= B_BASE) {
       res.body[i] = (byte)(t-B_BASE);
     }
     else {
       res.body[i] = (byte)t;
       c = 0;
     }
   }
   if (c > 0) {
#if BIGINTEGER_DEBUG
     Debug.Assert(res.used == longer.used+1);
#endif
     res.body[longer.used] = (byte)c;
   }

   res.u_renormalise();
#if BIGINTEGER_DEBUG
   Debug.Assert(res.sane());
#endif
 }

 static void usub(BigInteger x, BigInteger y, BigInteger res) {
#if BIGINTEGER_DEBUG
   Debug.Assert(x.sane());
   Debug.Assert(y.sane());
   Debug.Assert(x.used >= y.used);
   Debug.Assert(res.size == maxused_addsub(x,y));
#endif

   int b, i, t;

   b = 0;
   for (i = 0; i < y.used; i++) {
     t = x.body[i] - y.body[i] - b;
     if (t < 0) {
       res.body[i] = (byte)(t + B_BASE);
       b = 1;
     }
     else {
       res.body[i] = (byte)t;
       b = 0;
     }
   }

   for (i = y.used; i < x.used; i++) {
     t = x.body[i] - b;
     if (t < 0) {
       res.body[i] = (byte)(t + B_BASE);
     }
     else {
       res.body[i] = (byte)t;
       b = 0;
     }
   }
#if BIGINTEGER_DEBUG
   Debug.Assert (b == 0);
#endif
   
   res.used = x.used;
   res.u_renormalise();
#if BIGINTEGER_DEBUG
   Debug.Assert(res.sane());
#endif
 }

 static void umul(BigInteger x, BigInteger y, BigInteger res) {
   int i, j, carry;

#if BIGINTEGER_DEBUG
   Debug.Assert(x.sane());
   Debug.Assert(y.sane());
   Debug.Assert(res.size == maxused_mul(x,y));
#endif

   for (j = 0; j < y.used; j++) {
     res.body[j] = 0;
   }

   for (i = 0; i < x.used; i++) {
     carry = 0;
     for (j = 0; j < y.used; j++) {
       carry += res.body[i+j] + x.body[i]*y.body[j];
       res.body[i+j] = (byte)(carry % B_BASE);
       carry /= B_BASE;
#if BIGINTEGER_DEBUG
       Debug.Assert (carry < B_BASE);
#endif
     }
     res.body[i+y.used] = (byte)carry;
   }

   res.used = x.used+y.used;
   res.u_renormalise();
#if BIGINTEGER_DEBUG
   Debug.Assert(res.sane());
#endif
 }

 static void uqrm(BigInteger dend, BigInteger isor, out BigInteger dres, out BigInteger mres) {
   int i, j, t, vh, delta, carry, scaleup;
   byte [] dend_body, isor_body, tmp;
   bool toolarge;

#if BIGINTEGER_DEBUG
   Debug.Assert(isor.sane());
   Debug.Assert(dend.sane());
   Debug.Assert(isor.used > 0);  // against division by zero
#endif
   dres = new BigInteger();
   mres = new BigInteger();
   mres.size = dres.size = maxused_qrm(isor, dend);
   dres.body = new byte[dres.size];
   mres.body = new byte[mres.size];

   if (dend.used < isor.used) {
     // Result of division must be zero, since dividend has
     // fewer digits than the divisor.  Remainder is the
     // original dividend.
     dres.used = 0;
     mres.used = dend.used;
     for (j = 0; j < mres.used; j++) {
       mres.body[j] = dend.body[j];
     }
     dres.u_renormalise();
     mres.u_renormalise();
#if BIGINTEGER_DEBUG
     Debug.Assert(dres.sane());
     Debug.Assert(mres.sane());
#endif
     return;
   }

   if (isor.used == 1) {

     // Simple case; divisor is a single digit
     carry = 0;
     for (j = dend.used-1; j >= 0; j--) {
       carry += dend.body[j];
       dres.body[j] = (byte)(carry/isor.body[0]);
       carry = B_BASE*(carry%isor.body[0]);
     }
     carry /= B_BASE;
     dres.used = dend.used;
     dres.u_renormalise();

     // Remainder is the final carry value
     mres.used = 0;
     if (carry > 0) {
       mres.used = 1;
       mres.body[0] = (byte)carry;
     }
     dres.u_renormalise();
     mres.u_renormalise();
#if BIGINTEGER_DEBUG
     Debug.Assert(dres.sane());
     Debug.Assert(mres.sane());
#endif
     return;

   }
   else {

     // Complex case: both dividend and divisor have two or more digits.
#if BIGINTEGER_DEBUG
     Debug.Assert(isor.used >= 2);
     Debug.Assert(dend.used >= 2);
#endif

     // Allocate space for a copy of both dividend and divisor, since we 
     // need to mess with them.  Also allocate tmp as a place to hold
     // values of the form   quotient_digit * divisor.
     dend_body = new byte[dend.used+1];
     isor_body = new byte[isor.used];
     tmp       = new byte[isor.used+1];
      
     // Calculate a scaling-up factor, and multiply both divisor and 
     // dividend by it.  Doing this reduces the number of corrections
     // needed to the quotient-digit-estimates made in the loop below,
     // and thus speeds up division, but is not actually needed to
     // get the correct results.  The scaleup factor should not increase
     // the number of digits needed to represent either the divisor
     // (since the factor is derived from it) or the dividend (since
     // we already gave it a new leading zero).
     scaleup = B_BASE / (1 + isor.body[isor.used-1]);
#if BIGINTEGER_DEBUG
     Debug.Assert (1 <= scaleup && scaleup <= B_BASE/2);
#endif
     
     if (scaleup == 1) {
       // Don't bother to multiply; just copy.
       for (j = 0; j < dend.used; j++) {
	 dend_body[j] = dend.body[j];
       }
       for (j = 0; j < isor.used; j++) {
	 isor_body[j] = isor.body[j];
       }

       // Extend dividend with leading zero.
       dend_body[dend.used] = tmp[isor.used] = 0;

     }
     else {
       carry = 0;
       for (j = 0; j < isor.used; j++) {
	 t = scaleup * isor.body[j] + carry;
	 isor_body[j] = (byte)(t % B_BASE);
	 carry = t / B_BASE;
       }
#if BIGINTEGER_DEBUG
       Debug.Assert (carry == 0);
#endif
       
       carry = 0;
       for (j = 0; j < dend.used; j++) {
	 t = scaleup * dend.body[j] + carry;
	 dend_body[j] = (byte)(t % B_BASE);
	 carry = t / B_BASE;
       }
       dend_body[dend.used] = (byte)carry;
       tmp[isor.used] = 0;
     }

     // For each quotient digit ...
     for (i = dend.used; i >= isor.used; i--) {
#if BIGINTEGER_DEBUG
       Debug.Assert (i-2 >= 0);
       Debug.Assert (i <= dend.used);
       Debug.Assert (isor.used >= 2);
#endif

#if BIGINTEGER_DEBUG
       Console.WriteLine("\n---------\nqdigit {0}", i );
       Console.Write("dend_body is ");
       for (j = dend.used; j>= 0; j--) {
	 Console.Write("{0} ",dend_body[j]);
       }
       Console.Write("\n");
#endif
       // Make a guess vh of the quotient digit
       vh = (B_BASE*B_BASE*dend_body[i] + B_BASE*dend_body[i-1] + dend_body[i-2])
       /
       (B_BASE*isor_body[isor.used-1] + isor_body[isor.used-2]);
       if (vh > B_BASE-1) {
	 vh = B_BASE-1;
       }
#if BIGINTEGER_DEBUG
       Console.WriteLine("guess formed from {0} {1} {2}   {3} {4}", 
	      dend_body[i], dend_body[i-1] , dend_body[i-2], 
	      isor_body[isor.used-1], isor_body[isor.used-2]);
       Console.WriteLine("guess is {0}", vh );
#endif
       // Check if vh is too large (by 1).  Calculate vh * isor into tmp
       // and see if it exceeds the same length prefix of dend.  If so, 
       // vh needs to be decremented.
       carry = 0;
       for (j = 0; j < isor.used; j++) {
	 t = vh * isor_body[j] + carry;
	 tmp[j] = (byte)(t % B_BASE);
	 carry = t / B_BASE;
       }
       tmp[isor.used] = (byte)carry;
       delta = i - isor.used;
#if BIGINTEGER_DEBUG
       Console.WriteLine("final carry is {0}", carry);
       Console.Write("vh * isor is " );
       for (j = isor.used; j >=0; j--) {
	 Console.Write("{0} ",tmp[j]);Console.Write("\n");
       }
       Console.WriteLine("delta = {0}", delta );
#endif
       toolarge = false;
       for (j = isor.used; j >= 0; j--) {
#if BIGINTEGER_DEBUG
	 Console.Write ( "({0},{1})  ", (int)(tmp[j]), (int)(dend_body[j+delta]) );
#endif
	 if (tmp[j] > dend_body[j+delta]) {
	   toolarge=true;
	   break;
	 }
	 if (tmp[j] < dend_body[j+delta]) {
	   break;
	 }
       }

       // If we did guess too large, decrement vh and subtract a copy of
       // isor from tmp.  This had better not go negative!
       if (toolarge) {
#if BIGINTEGER_DEBUG
	 Console.WriteLine ( "guess too large" );
#endif
	 vh--;
	 carry = 0;
	 for (j = 0; j < isor.used; j++) {
	   if (carry + isor_body[j] > tmp[j]) {
	     tmp[j] = (byte)((B_BASE + tmp[j]) - isor_body[j] - carry);
	     carry = 1;
	   }
	   else {
	     tmp[j] = (byte)(tmp[j] - isor_body[j] - carry);
	     carry = 0;
	   }
	 }
	 //if (carry > 0) {pp(isor);pp(dend);};
	 //Debug.Assert(carry == 0);
	 if (carry > 0) {
	   Debug.Assert(tmp[isor.used] > 0);
	   tmp[isor.used]--;
	 }
#if BIGINTEGER_DEBUG
	 Console.Write("after adjustment of tmp ");
	 for (j = isor.used; j >=0; j--) {
	   Console.Write("{0} ",tmp[j]);
	 }
	 Console.Write("\n");
#endif
       }

       // Now vh really is the i'th quotient digit.  
       // Subtract (tmp << delta) from
       // the dividend.
       carry = 0;
       for (j = 0; j <= isor.used; j++) {
	 if (carry + tmp[j] > dend_body[j+delta]) {
	   dend_body[j+delta] = (byte)((B_BASE+dend_body[j+delta]) - tmp[j]
				       - carry);
	   carry = 1;
	 }
	 else {
	   dend_body[j+delta] = (byte)(dend_body[j+delta] - tmp[j] - carry);
	   carry = 0;
	 }
       }
#if BIGINTEGER_DEBUG
       Debug.Assert(carry==0);
#endif
       
#if BIGINTEGER_DEBUG
       Console.Write("after final sub ");
       for(j=dend.used; j>=0; j--) Console.Write("{0} ", dend_body[j]);
       Console.Write("\n");
#endif

       // park vh in the result array
#if DEBUG_SAINTEGER_UDIV
       Console.WriteLine("[{0}] <- {1}", i-isor.used, vh );
#endif
       dres.body[i-isor.used] = (byte)vh;
     }
   }

   // Now we've got all the quotient digits.  Zap leading zeroes.
   dres.used = dend.used - isor.used + 1;
   dres.u_renormalise();
#if BIGINTEGER_DEBUG
   Debug.Assert(dres.sane());
#endif
   
   // The remainder is in dend_body.  Copy, divide by the original scaling 
   // factor, and zap leading zeroes.
   mres.used = dend.used;
   for (j = 0; j < dend.used; j++) {
     mres.body[j] = dend_body[j];
   }
   mres.u_renormalise();
#if BIGINTEGER_DEBUG
   Debug.Assert(mres.sane());
#endif
   
   if (scaleup > 1) {
     carry = 0;
     for (j = mres.used-1; j >= 0; j--) {
       carry += mres.body[j];
       mres.body[j] = (byte)(carry/scaleup);
       carry = B_BASE*(carry%scaleup);
     }
#if BIGINTEGER_DEBUG
     Debug.Assert (carry == 0);
#endif
     mres.u_renormalise();
#if BIGINTEGER_DEBUG
     Debug.Assert(mres.sane());
#endif
   }

 }


 // Test framework

#if BIGINTEGER_DEBUG
 public static void Test ( ) {
   int i, j, t, k, m;
   BigInteger bi, bj, bk, bm;

   BigInteger a, b;
   a = new BigInteger(1);
   for (int n = 1; n <= 10; n++) {
     b = new BigInteger(n);
     a *= n;
   }
   Console.WriteLine("{0}", (double)a);

   for (i = -10007; i <= 10007; i++) {
     Console.WriteLine ( "i = {0}", i );

     bi = new BigInteger(i);
     t = (int)bi;
     Debug.Assert(i == t);
     
     for (j = -10007; j <= 10007; j++) {
       bj = new BigInteger(j);
       t = (int)bj;
       Debug.Assert(j == t);
       bk = bi + bj;
       k = (int)bk;
       if (i+j != k) {
	 bi.ToString();
	 bj.ToString();
	 bk.ToString();
	 Debug.Assert(i + j == k);
       }

       bk = bi - bj;
       k = (int)bk;
       if (i-j != k) {
	 bi.ToString();
	 bj.ToString();
	 bk.ToString();
	 Debug.Assert(i - j == k);
       }

       bk = bi * bj;
       k = (int)bk;
       if (i*j != k) {
	 bi.ToString();
	 bj.ToString();
	 bk.ToString();
	 Debug.Assert(i * j == k);
       }

       if (j != 0) {
	 QuotientRemainder(bi, bj, out bk, out bm);
	 k = (int)bk;
	 m = (int)bm;
	 Debug.Assert(k == i / j);
	 Debug.Assert(m == i % j);
       }
     }
   }
   Console.WriteLine("done");
 }
#endif

}
