﻿/* The Computer Language Benchmarks Game
   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
 *
 * contributed by Mike Pall
 * Java by Stefan Krause
 * C# by Miguel de Icaza
 * modified by Pavel Rusin 
 */
using System;
using System.Text;
using System.Runtime.InteropServices;

public class pidigits {

   GmpInteger q = new GmpInteger(), r = new GmpInteger(), s = new GmpInteger(),
t = new GmpInteger();
   GmpInteger u = new GmpInteger(), v = new GmpInteger(), w = new GmpInteger();

   int i;
   readonly StringBuilder strBuf = new StringBuilder ();
   int n;
   private const int Zero = ʼ0ʼ;

   private char FastDigitToString(int value)
   {
      return (char)(Zero + value);
   }

   pidigits (int n)
   {
      this.n=n;
   }

   private void compose_r(int bq, int br, int bs, int bt)
   {
     u.mul(r, bs);
     r.mul(r, bq);
     v.mul(t, br);
     r.add(r, v);
     t.mul(t, bt);
     t.add(t, u);
     s.mul(s, bt);
     u.mul(q, bs);
     s.add(s, u);
     q.mul(q, bq);
   }

   /* Compose matrix with numbers on the left. */
   private void compose_l(int bq, int br, int bs, int bt)
   {
     r.mul(r, bt);
     u.mul(q, br);
     r.add(r, u);
     u.mul(t, bs);
     t.mul(t, bt);
     v.mul(s, br);
     t.add(t, v);
     s.mul(s, bq);
     s.add(s, u);
     q.mul(q, bq);
   }

   /* Extract one digit. */
   private int extract(int j)
   {
     u.mul(q, j);
     u.add(u, r);
     v.mul(s, j);
     v.add(v, t);
     w.div(u, v);
     return w.intValue();
   }

   /* Print one digit. Returns 1 for the last digit. */
   private bool prdigit(int y)
   {
      strBuf.Append(FastDigitToString(y));
      if (++i % 10 == 0 || i == n)
      {
         if (i % 10 != 0)
         {
            strBuf.Append(ʼ ʼ, 10 - this.i % 10);
         }

         strBuf.Append("\t:");
         strBuf.Append(i);
         strBuf.Append(Environment.NewLine);
      }

      return i == n;
   }

   /* Generate successive digits of PI. */
   void Run()
   {
     strBuf.Capacity = n % 10 * 25;
     int k = 1;
     i = 0;
     q.set(1);
     r.set(0);
     s.set(0);
     t.set(1);
     for (;;) {
       int y = extract(3);
       if (y == extract(4)) {
         if (prdigit(y))
         {
            Console.Write(this.strBuf.ToString());
            return;
         }

         compose_r(10, -10 * y, 0, 1);

       }
       else
       {
         compose_l(k, 4 * k + 2, 0, 2 * k + 1);
         k++;
       }
     }
   }

   public static void Main(String[] args) {
       pidigits m = new pidigits(Int32.Parse (args[0]));
       m.Run();
   }
}

[StructLayout (LayoutKind.Sequential)]
struct mpz_t {
    public int _mp_alloc;
    public int _mp_size;
    public IntPtr ptr;
}

class GmpInteger {

   // Public methods

   public GmpInteger() {
      mpz_init(ref pointer);
   }

   public GmpInteger(int value) {
      mpz_set_si(ref pointer, value);
   }

   public void set(int value) { mpz_set_si(ref pointer, value); }

   public void mul(GmpInteger src, int val) { mpz_mul_si(ref pointer, ref src.po
inter, val); }

   public void add(GmpInteger op1, GmpInteger op2) { mpz_add(ref pointer, ref op
1.pointer, ref op2.pointer); }

   public void div(GmpInteger op1, GmpInteger op2) { mpz_tdiv_q(ref pointer, ref
 op1.pointer, ref op2.pointer); }

   public int intValue() { return mpz_get_si(ref pointer); }

   public double doubleValue() { return mpz_get_d(ref pointer); }

   // Non public stuff

   mpz_t pointer;

    [DllImport ("gmp", EntryPoint="__gmpz_init")]
    extern static void mpz_init(ref mpz_t value);

    [DllImport ("gmp", EntryPoint="__gmpz_mul_si")]
    extern static void mpz_mul_si(ref mpz_t dest, ref mpz_t src, int val);

    [DllImport ("gmp", EntryPoint="__gmpz_add")]
    extern static void mpz_add(ref mpz_t dest, ref mpz_t src, ref mpz_t src2);

    [DllImport ("gmp", EntryPoint="__gmpz_tdiv_q")]
    extern static void mpz_tdiv_q(ref mpz_t dest, ref mpz_t src, ref mpz_t src2)
;

    [DllImport ("gmp", EntryPoint="__gmpz_set_si")]
    extern static void mpz_set_si(ref mpz_t src, int value);

    [DllImport ("gmp", EntryPoint="__gmpz_get_si")]
    extern static int mpz_get_si(ref mpz_t src);

    [DllImport ("gmp", EntryPoint="__gmpz_get_d")]
    extern static double mpz_get_d(ref mpz_t src);
}
