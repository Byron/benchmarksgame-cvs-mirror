/* The Computer Language Benchmarks Game
 * http://benchmarksgame.alioth.debian.org/
 *
 * based on pidigits.c (by Paolo Bonzini & Sean Bartlett,
 *                      modified by Michael Mellor)
 *
 * contributed by The Go Authors.
 * flag.Arg hack by Isaac Gouy
 * line printer hack by Sean Lake
 */

package main

import (
   "math/big"
   "flag"
   "fmt"
   "strconv"
   "os"
   "bufio"
)

var n = 0
var silent = false

var (
   tmp1  = big.NewInt(0)
   tmp2  = big.NewInt(0)
   tmp3  = big.NewInt(0)
   y2    = big.NewInt(0)
   bigk  = big.NewInt(0)
   numer = big.NewInt(1)
   accum = big.NewInt(0)
   denom = big.NewInt(1)
   ten   = big.NewInt(10)
)

func extract_digit() int64 {
   if numer.Cmp(accum) > 0 {
      return -1
   }

   // Compute (numer * 3 + accum) / denom
   tmp1.Lsh(numer, 1)
   tmp1.Add(tmp1, numer)
   tmp1.Add(tmp1, accum)
   tmp1.DivMod(tmp1, denom, tmp2)

   // Now, if (numer * 4 + accum) % denom...
   tmp2.Add(tmp2, numer)

   // ... is normalized, then the two divisions have the same result.
   if tmp2.Cmp(denom) >= 0 {
      return -1
   }

   return tmp1.Int64()
}

func next_term(k int64) {
   y2.SetInt64(k*2 + 1)
   bigk.SetInt64(k)

   tmp1.Lsh(numer, 1)
   accum.Add(accum, tmp1)
   accum.Mul(accum, y2)
   numer.Mul(numer, bigk)
   denom.Mul(denom, y2)
}

func eliminate_digit(d int64) {
   tmp3.SetInt64(d)
   accum.Sub(accum, tmp3.Mul(denom, tmp3))
   accum.Mul(accum, ten)
   numer.Mul(numer, ten)
}


func main() {
   flag.Parse()
   if flag.NArg() > 0 { n,_ = strconv.Atoi( flag.Arg(0) ) }

   w := bufio.NewWriter( os.Stdout )
   defer w.Flush()

   line := make( []byte, 0, 10 )
   i := int(0)
   for k := int64(0); ; {
      d := int64(-1)
      for d < 0 {
         k++
         next_term(k)
         d = extract_digit()
      }

      i++

      line = append(line, byte(d)+'0')
	  if len(line) == 10 {
	  	 if silent != true {
	  	 	fmt.Fprintf( w, string(line) )
			fmt.Fprintf( w, "\t:%d\n", i)
		 }

		 line = line[:0]
	  }

      if i >= n {
         break
      }
      eliminate_digit(d)
   }

   if len(line) > 0 {
      fmt.Fprintf( w, string(line) )
      fmt.Fprintf( w, "%s\t:%d\n", "          "[len(line):], i)
   }

   return
}
