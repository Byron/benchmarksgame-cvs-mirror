/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/
   contributed by Isaac Gouy
   modified by Konstantin Safonov
 */

using System;
using System.Diagnostics;
using System.Linq;
using System.Threading;

internal class NamedThread {
    private readonly int name;
    private readonly ManualResetEventSlim signal = new ManualResetEventSlim(false);
    private int token;

    internal NamedThread( int name ) {
        this.name = name;
    }

    internal NamedThread NextThread() {
        return ThreadRing.threadRing[ name % ThreadRing.numberOfThreads ];
    }

    internal void Run() {
        while ( TokenNotDone() )
            NextThread().TakeToken( token - 1 );

        if ( token == 0 ) Console.WriteLine( name );
        NextThread().TakeToken( -1 );
    }

    internal void TakeToken( int x ) {
        token = x;
        signal.Set();
    }

    private bool TokenNotDone() {
        signal.Wait();
        signal.Reset();
        return token > 0;
    }
}

public class ThreadRing {
    internal const int numberOfThreads = 503;
    internal static NamedThread[] threadRing = new NamedThread[503];

    private static void Main( String[] args ) {
        Main2( args );
    }

    public static void Main2( string[] args ) {
        for ( var i = 0; i < numberOfThreads; i++ ) threadRing[ i ] = new NamedThread( i + 1 );
        var thrs = threadRing.Select( a => new Thread( a.Run ) ).ToArray();
        foreach ( var thread in thrs ) thread.Start(); 
        var cnt = args.Length > 0 ? int.Parse( args[ 0 ] ) : 50000000;
        threadRing[ 0 ].TakeToken( cnt );
        foreach ( var thread in thrs ) thread.Join();
    }
}
