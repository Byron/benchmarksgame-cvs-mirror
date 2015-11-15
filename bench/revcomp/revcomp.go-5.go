// The Computer Language Benchmarks Game
// http://benchmarksgame.alioth.debian.org/
//
// contributed by Manlio Perillo

package main

import (
   "bytes"
   "os"
)

// PipeBufSize represents the pipe buffer size.
// Default pipe buf size on a Linux x86_64 system is 64K.  
// The max buf size is 1M.
// The buffer must be large enough to keep an entire FASTA sequence, including
// header.
const PipeBufSize = 2 * 64 * 1024

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

func main() {
   // no other buffers were harmed by this implementation
   buf := make([]byte, PipeBufSize)
   // position of the next sequence, including header
   pos := 0
   // read position
   last := 0
   // end of read buffer
   end := 0

   for {
      n, err := os.Stdin.Read(buf[end:])
      if err != nil {
         break
      } else if n == 0 {
         panic("buffer overflow")
      }
      end += n

      for {
         // Read sequence header.
         i := bytes.IndexByte(buf[last:end], '\n')
         if i == -1 {
            // Incomplete header.
            // Write previous sequences and slide last one to beginning.
            os.Stdout.Write(buf[:pos])

            copy(buf, buf[pos:end])
            end -= last
            last = 0
            pos = 0

            break
         }
         // header is in buf[last+1:last+i]
         last += i + 1

         // Read sequence data.
         i = bytes.IndexByte(buf[last:end], '>')
         if i == -1 {
            if end < PipeBufSize {
               // End of data.
               // Write previous sequences and the last one.
               transform(buf[last : end-1])
               os.Stdout.Write(buf[:end])
            } else {
               // Incomplete data.
               // Write previous sequences and slide last one to beginning.
               os.Stdout.Write(buf[:pos])
               copy(buf, buf[pos:end])
               end -= pos
            }
            last = 0
            pos = 0

            break
         }

         transform(buf[last : last+i-1])
         last += i
         pos = last
      }
   }
}

// transform computes the reverse complement of a sequence.
func transform(strand []byte) {
   for i, j := 0, len(strand)-1; i < j; i, j = i+1, j-1 {
      if strand[i] == '\n' {
         i++
      }
      if strand[j] == '\n' {
         j--
      }
      strand[i], strand[j] = complement[strand[j]], complement[strand[i]]
   }
}
