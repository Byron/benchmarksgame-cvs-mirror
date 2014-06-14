/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   contributed by Isaac Gouy, optimization and use of more C# idioms by Robert F. Tobler
   Optimized further with SIMD by Nigel Delaney
*/

using System;
using Mono.Simd;

class NBody
{
   public static void Main (String[] args)
   {
      int n = args.Length > 0 ? Int32.Parse (args [0]) : 10000;
      NBodySystem bodies = new NBodySystem ();
      Console.WriteLine ("{0:f9}", bodies.Energy ());
      bodies.Advance (0.01, n);
      Console.WriteLine ("{0:f9}", bodies.Energy ());
   }
}

class Body
{
   public Vector2d xy, z_, vxvy, vzv_, mass;
}

struct Pair
{
   public Body bi, bj;
}

class NBodySystem
{
   private Body[] bodies;
   private Pair[] pairs;

   const double Pi = 3.141592653589793;
   const double Solarmass = 4 * Pi * Pi;
   const double DaysPeryear = 365.24;

   public NBodySystem ()
   {
      bodies = new Body[] {
         new Body () { // Sun
            mass = new Vector2d (Solarmass, Solarmass)
         },
         new Body () { // Jupiter
            xy = new Vector2d (
               4.84143144246472090e+00,
               -1.16032004402742839e+00),
            z_ = new Vector2d (
               -1.03622044471123109e-01,
               0),
            vxvy = new Vector2d (
               1.66007664274403694e-03 * DaysPeryear,
               7.69901118419740425e-03 * DaysPeryear),
            vzv_ = new Vector2d (
               -6.90460016972063023e-05 * DaysPeryear,
               0),
            mass = new Vector2d (
               9.54791938424326609e-04 * Solarmass,
               9.54791938424326609e-04 * Solarmass)
         },
         new Body () { // Saturn
            xy = new Vector2d (
               8.34336671824457987e+00,
               4.12479856412430479e+00),
            z_ = new Vector2d (
               -4.03523417114321381e-01,
               0.0),
            vxvy = new Vector2d (
               -2.76742510726862411e-03 * DaysPeryear,
               4.99852801234917238e-03 * DaysPeryear),
            vzv_ = new Vector2d (
               2.30417297573763929e-05 * DaysPeryear,
               0.0),
            mass = new Vector2d (
               2.85885980666130812e-04 * Solarmass,
               2.85885980666130812e-04 * Solarmass)
         },
         new Body () { // Uranus
            xy = new Vector2d (
               1.28943695621391310e+01,
               -1.51111514016986312e+01),
            z_ = new Vector2d (
               -2.23307578892655734e-01,
               0.0),
            
            vxvy = new Vector2d (
               2.96460137564761618e-03 * DaysPeryear,
               2.37847173959480950e-03 * DaysPeryear),
            vzv_ = new Vector2d (
               -2.96589568540237556e-05 * DaysPeryear,
               0.0),
            mass = new Vector2d (
               4.36624404335156298e-05 * Solarmass,
               4.36624404335156298e-05 * Solarmass)
         },
         new Body () { // Neptune
            xy = new Vector2d (
               1.53796971148509165e+01,
               -2.59193146099879641e+01),
            z_ = new Vector2d (
               1.79258772950371181e-01,
               0.0),

            vxvy = new Vector2d (
               2.68067772490389322e-03 * DaysPeryear,
               1.62824170038242295e-03 * DaysPeryear),
            vzv_ = new Vector2d (
               -9.51592254519715870e-05 * DaysPeryear,
               0.0),
            mass = new Vector2d (
               5.15138902046611451e-05 * Solarmass,
               5.15138902046611451e-05 * Solarmass)
         },
      };

      pairs = new Pair[bodies.Length * (bodies.Length - 1) / 2];        
      int pi = 0;
      for (int i = 0; i < bodies.Length - 1; i++)
         for (int j = i + 1; j < bodies.Length; j++)
            pairs [pi++] = new Pair () { bi = bodies [i], bj = bodies [j] };        

      Vector2d pxpy = new Vector2d ();
      Vector2d pzp_ = new Vector2d ();
      foreach (var b in bodies) {
         pxpy += b.vxvy * b.mass;
         pzp_ += b.vzv_ * b.mass;
      }
      var sol = bodies [0];
      var solarMassPacked = new Vector2d (Solarmass, Solarmass);
      sol.vxvy = Vector2d.MinusOne * pxpy / solarMassPacked;
      sol.vzv_ = Vector2d.MinusOne * pzp_ / solarMassPacked;
   }

   public void Advance (double dt, int n)
   {
      var dtp = new Vector2d (dt, dt);
      for (int i = 0; i < n; i++) {      
         foreach (var p in pairs) {
            Body bi = p.bi, bj = p.bj;
            //get differences
            var dxdy = bi.xy - bj.xy; 
            var dzd_ = bi.z_ - bj.z_;
            //square the differences
            var d2xy = dxdy * dxdy;
            var d2z = dzd_ * dzd_;
            //get x^2+y^2+z^2 in both elements of the packed double
            //Note assumes second element of dz is zero
            var d2 = d2xy.HorizontalAdd (d2xy) + d2z.HorizontalAdd (d2z);
            var mag = dtp / (d2 * d2.Sqrt ());
            var bjmassTimesMag = bj.mass * mag;
            bi.vxvy -= dxdy * bjmassTimesMag;
            bi.vzv_ -= dzd_ * bjmassTimesMag; 
            var bimassTimesMag = bi.mass * mag;
            bj.vxvy += dxdy * bimassTimesMag;
            bj.vzv_ += dzd_ * bimassTimesMag;
         }
         foreach (var b in bodies) {
            b.xy += dtp * b.vxvy;
            b.z_ += dtp * b.vzv_;
         }
      }
   }

   public double Energy ()
   {
      double e = 0.0;
      for (int i = 0; i < bodies.Length; i++) {
         var bi = bodies [i];
         e += 0.5 * bi.mass.X * (bi.vxvy.X * bi.vxvy.X + bi.vxvy.Y * bi.vxvy.Y + bi.vzv_.X * bi.vzv_.X);
         for (int j = i + 1; j < bodies.Length; j++) {
            var bj = bodies [j];
            double dx = bi.xy.X - bj.xy.X, dy = bi.xy.Y - bj.xy.Y, dz = bi.z_.X - bj.z_.X;

            e -= (bi.mass * bj.mass).X / Math.Sqrt (dx * dx + dy * dy + dz * dz);
         }
      }
      return e;
   }
}
