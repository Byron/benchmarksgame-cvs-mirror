/* The Computer Language Benchmarks Game
 * http://benchmarksgame.alioth.debian.org/
 *
 * contributed by The Go Authors.
 * modified by Dirk Moerenhout.
 */

package main

import (
   "bufio"
   "os"
   "runtime"
)

const lineSize = 60
const NCPU = 4

var complement = [256]uint8{
   'A': 'T', 'a': 'T',
   'C': 'G', 'c': 'G',
   'G': 'C', 'g': 'C',
   'T': 'A', 't': 'A',
   'U': 'A', 'u': 'A',
   'M': 'K', 'm': 'K',
   'R': 'Y', 'r': 'Y',
   'W': 'W', 'w': 'W',
   'S': 'S', 's': 'S',
   'Y': 'R', 'y': 'R',
   'K': 'M', 'k': 'M',
   'V': 'B', 'v': 'B',
   'H': 'D', 'h': 'D',
   'D': 'H', 'd': 'H',
   'B': 'V', 'b': 'V',
   'N': 'N', 'n': 'N',
}

func createoutput(buf []byte, obuf []byte, c chan int) {
   lines:=len(buf)/60
   bufpos:=len(buf)-1
   for obufpos := 0; obufpos < lines*61; obufpos++ {
      for end := obufpos+60; obufpos < end; obufpos++ {
         obuf[obufpos] = complement[buf[bufpos]]
         bufpos--
      }
      obuf[obufpos] = '\n'
   }
   c <- 1
}

func dumpoutput(obuf []byte, d chan int) {
   os.Stdout.Write(obuf)
   d <- 1
} 

func main() {
   runtime.GOMAXPROCS(NCPU)
   d := make(chan int, 1)
   d <- 1
   in := bufio.NewReader(os.Stdin)
   buf := make([]byte,0,64*1024*1024)
   line, err := in.ReadSlice('\n')
   for err == nil {
      title := make([]byte, len(line))
      copy(title, line)
      for {
         line, err = in.ReadSlice('\n')
         if err != nil || line[0] == '>' {
            break
         }
         if len(buf)+60>cap(buf) {
            nbuf := make([]byte, len(buf), cap(buf)+64*1024*1024)
            copy(nbuf, buf)
            buf = nbuf
         }
         buf = append(buf, line[0: len(line)-1]...)
      }
      
      lines := len(buf)/60
      charsleft := len(buf)%60
      obuf := make([]byte, len(buf)+lines+1 )

      c := make(chan int, NCPU)
      obufstart:=0;
      bufend:=len(buf)
      linesperthread:=lines/NCPU
      for i := 0; i < NCPU; i++ {
         if i==3 {
            linesperthread+=lines%NCPU
         }
         bufstart:=bufend-linesperthread*60
         obufend:=obufstart+linesperthread*61
         go createoutput(buf[bufstart:bufend], obuf[obufstart:obufend], c)
         bufend=bufstart
         obufstart=obufend
      }
      for i := 0; i < NCPU; i++ {
         <-c
      }
      
      <-d
      os.Stdout.Write(title)

      if charsleft > 0 {
         obufpos := lines * 61
         for bufpos := charsleft-1; bufpos >= 0; bufpos-- {
            obuf[obufpos] = complement[buf[bufpos]]
            obufpos++
         }
         obuf[obufpos] = '\n'
         go dumpoutput(obuf, d)
         os.Stdout.Write(obuf)
      } else {
         go dumpoutput(obuf[0:len(obuf)-1], d)
      }
      buf=buf[0:0]
   }
   <-d
}
