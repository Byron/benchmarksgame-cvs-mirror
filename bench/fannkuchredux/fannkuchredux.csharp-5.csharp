﻿/*
 * The Computer Language Benchmarks Game
 * http://benchmarksgame.alioth.debian.org/
 * contributed by Isaac Gouy, transliterated from Oleg Mazurov's Java program
 * modified by Konstantin Safonov
*/
using System;
using System.Threading;

internal static class FannkuchRedux {
    private const int Nchunks = 150;
    private static int _chunksz;
    private static int _ntasks;
    private static int _n;
    private static int[] _fact;
    private static int[] _maxFlips;
    private static int[] _chkSums;
    private static int _taskId;

    private static readonly object TaskidLocker = new object();
    private static int GetTaskId() { lock ( TaskidLocker ) { return _taskId++; } }

    private static unsafe void Run() {
        var p = new long[_n];
        var pp = new long[_n];
        var count = new int[_n];
        fixed (int* qc = count, qf = _fact)
        fixed ( long* qp = p, qpp = pp ) RunCore( qpp, qp, p, qc, count, qf );
    }

    private static unsafe void RunCore( long* qpp, long* qp, long[] p, int* qc,
        int[] count, int* qf ) {
        int task;
        long* pp1 = qpp + 1L, ppm1 = qpp - 1L, qpe = qp + p.LongLength, qp1 = qp + 1L;
        while ( ( task = GetTaskId() ) < _ntasks ) {
            var idxMin = task * _chunksz;
            var idxMax = Math.Min( _fact[ _n ], idxMin + _chunksz );
            {
                var idx = idxMin;
                var pl = p.LongLength;
                for ( var i = 0L; i < pl; ++i ) p[ i ] = i;

                for ( var i = count.LongLength - 1; i > 0L; --i ) {
                    var qfi = *( qf + i );
                    var qpi1 = qp1 + i;
                    var qpi = qp + i;
                    var d = idx / qfi;
                    *( qc + i ) = d;
                    idx = idx % qfi;
                    //Array.Copy( p, 0, pp, 0, i + 1 );
                    for ( long* qs = qp, qd = qpp; qs < qpi1; *qd = *qs,++qs,++qd ) {}

                    long jd = d;
                    for ( var j = qp; j <= qpi; ++j, ++jd )
                        *j = jd <= i ? *( qpp + jd ) : *( ppm1 + jd - i );
                }
            }

            var maxflips = 1;
            var chksum = 0;
            for ( var i = idxMin;; ) {
                if ( *qp != 0 ) {
                    #region CountFlips();
                    var flips = 1;
                    var first = *qp;
                    if ( *( qp + first ) != 0 ) {
                        //Array.Copy( p, 0, pp, 0, pp.Length );
                        for ( long* s = qp, d = qpp; s < qpe; *d = *s, ++s, ++d ) {}
                        do {
                            ++flips;
                            for ( long* lo = pp1, hi = ppm1 + first; lo < hi; ++lo, --hi ) {
                                var t = *lo; *lo = *hi; *hi = t;
                            }
                            var qppfirst = qpp + first;
                            var tp = *qppfirst;
                            *qppfirst = first;
                            first = tp;
                        } while ( *( qpp + first ) != 0 );
                    }
                    #endregion
                    if ( flips > maxflips ) maxflips = flips;
                    chksum += ( i & 1 ) == 0 ? flips : -flips;
                }
                if ( ++i == idxMax ) break;
                #region NextPermutation();
                {
                    var first = *qp1;
                    *qp1 = *qp;
                    *qp = first;

                    var i2 = 1L;
                    while ( ++*( qc + i2 ) > i2 ) {
                        *( qc + ( i2++ ) ) = 0;
                        var next = *qp = *qp1;
                        var qpi2 = qp + i2;
                        for ( var j = qp1; j < qpi2; ) {
                            var ji = j;
                            ++ji;
                            *j = *ji;
                            j = ji;
                        }
                        *qpi2 = first;
                        first = next;
                    }
                }
                #endregion
            }
            _maxFlips[ task ] = maxflips;
            _chkSums[ task ] = chksum;
        }
    }
    private static void Main( String[] args ) {
        Main2( args );
    }
    private static void Main2( params string[] args ) {
        _n = 7;
        if ( args.Length > 0 ) _n = Int32.Parse( args[ 0 ] );

        _fact = new int[_n + 1];
        _fact[ 0 ] = 1;
        for ( var i = 1; i < _fact.Length; ++i ) _fact[ i ] = _fact[ i - 1 ] * i;

        _chunksz = ( _fact[ _n ] + Nchunks - 1 ) / Nchunks;
        _ntasks = ( _fact[ _n ] + _chunksz - 1 ) / _chunksz;
        _maxFlips = new int[_ntasks];
        _chkSums = new int[_ntasks];
        _taskId = 0;

        var nthreads = Environment.ProcessorCount;
        var threads = new Thread[nthreads];
        for (var i = 0; i < nthreads; ++i) (threads[i] = new Thread(Run)).Start();
        foreach (var t in threads) t.Join();
        var res = 0;
        foreach ( var v in _maxFlips ) res = Math.Max( res, v );
        var chk = 0;
        foreach ( var v in _chkSums ) chk += v;
        Console.WriteLine( "{0}\nPfannkuchen({1}) = {2}", chk, _n, res );
    }
}
