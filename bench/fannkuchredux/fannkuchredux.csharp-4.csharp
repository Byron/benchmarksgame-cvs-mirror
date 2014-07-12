/*
 * The Computer Language Benchmarks Game
 * http://benchmarksgame.alioth.debian.org/
 * 
 * contributed by Isaac Gouy, transliterated from Oleg Mazurov's Java program
 * modified by Konstantin Safonov
*/
using System;
internal static class FannkuchRedux {
    public static unsafe int[] Fannkuch( int n ) {
        long[] p = new long[n], q = new long[n], s = new long[n];
        int sign = 1, maxflips = 0, sum = 0, m = n - 1;
        for ( var i = 0; i < n; i++ ) s[i] = q[i] = p[i] = i;
        fixed (long* pp = p, sp = s, qp = q) {
            var pp1 = pp + 1; var qp1 = qp + 1; var qpm1 = qp - 1; var ppe = pp + n;
            do { // Copy and flip.
                var q0 = *pp; // Cache 0th element.
                if ( q0 != 0L ) { // Work on a copy.
                    for ( long* qi = qp1, pi = pp1; pi < ppe; ++qi, ++pi ) *qi = *pi;
                    var flips = 1;
                    do {
                        var qpq0 = qp + q0; var qq = *qpq0;
                        if ( qq == 0L ) { // ... until 0th element is 0.
                            sum = sum + sign * flips;//need FMA here
                            if (flips > maxflips) maxflips = flips;// New maximum?
                            break;
                        }
                        *qpq0 = q0;
                        if ( q0 >= 3L ) {
                            var qpi = qp1; var qpj = qpm1 + q0;
                            do { var t = *qpi; *qpi = *qpj; *qpj = t; ++qpi; --qpj; } while ( qpi < qpj );
                        }
                        q0 = qq;
                        flips++;
                    } while ( true );
                }
                // Permute.
                if ( sign == 1 ) {
                    var t = *pp1; *pp1 = *pp; *pp = t; sign = -1; // Rotate 0<-1.
                } else {
                    var t = *pp1; *pp1 = *(pp+2); *(pp+2) = t; sign = 1; // Rotate 0<-1 and 0<-1<-2.
                    for ( var i = 2; i < n; i++ ) {
                        var spi = sp + i;
                        var sx = *spi;
                        if ( sx != 0 ) { *spi = sx - 1; break; }
                        // Out of permutations.
                        if ( i == m ) return new[] { sum, maxflips }; 
                        *spi = i;
                        // Rotate 0<-...<-i+1.
                        t = *pp; var pi = pp + i;
                        for ( var j = pp; j <= pi; ) { var ji = j; *j = *++ji; j=ji; }
                        *++pi = t;
                    }
                }
            } while ( true );
        }
    }
    private static void Main( string[] args ) {
        var n = ( args.Length > 0 ) ? Int32.Parse( args[ 0 ] ) : 7;
        var pf = Fannkuch( n );
        Console.Write( "{0}\nPfannkuchen({1}) = {2}\n", pf[ 0 ], n, pf[ 1 ] );
    }
}
