/* The Computer Language Benchmarks Game
 * http://benchmarksgame.alioth.debian.org/
 *
 * contributed by Tylor Arndt
 */

package main

import (
   "bufio"
   "bytes"
   "fmt"
   "io/ioutil"
   "log"
   "os"
   "runtime"
   "sort"
   "sync"
)

func main() {
   runtime.GOMAXPROCS(4)
   dna := readEncDNA()
   var wgs [7]*sync.WaitGroup
   for i := 0; i < 7; i++ {
      wgs[i] = new(sync.WaitGroup)
   }
   report(dna, 1, nil, wgs[0])
   report(dna, 2, wgs[0], wgs[1])
   report(dna, 3, wgs[1], wgs[2])
   report(dna, 4, wgs[2], wgs[3])
   report(dna, 6, wgs[3], wgs[4])
   report(dna, 12, wgs[4], wgs[5])
   report(dna, 18, wgs[5], wgs[6])
   wgs[6].Wait()
}

func readEncDNA() []byte {
   in, startTok := bufio.NewReader(os.Stdin), []byte(">THREE ")
   for line, err := in.ReadSlice('\n'); !bytes.HasPrefix(line, startTok); line, err = in.ReadSlice('\n') {
      if err != nil {
         log.Panicf("Error: Could not read input from stdin; Details: %s", err)
      }
   }
   ascii, err := ioutil.ReadAll(in)
   if err != nil {
      log.Panicf("Error: Could not read input from stdin; Details: %s", err)
   }
   j := 0
   for i, c, asciic := 0, byte(0), len(ascii); i < asciic; i++ {
      c = ascii[i]
      switch c {
      case 'a', 'A':
         c = 0
      case 't', 'T':
         c = 1
      case 'g', 'G':
         c = 2
      case 'c', 'C':
         c = 3
      case '\n':
         continue
      default:
         log.Fatalf("Error: Invalid nucleotide value: '%c'", ascii[i])
      }
      ascii[j] = c
      j++
   }
   return ascii[:j+1]
}

var targSeqs = []string{3: "GGT", 4: "GGTA", 6: "GGTATT", 12: "GGTATTTTAATT", 18: "GGTATTTTAATTTATAGT"}

func report(dna []byte, n int, prev, done *sync.WaitGroup) {
   done.Add(1)
   go func() {
      tbl, output := count(dna, n), ""
      switch n {
      case 1, 2:
         output = freqReport(tbl, n)
      default:
         targ := targSeqs[n]
         output = fmt.Sprintf("%d\t%s\n", tbl[compStr(targ)], targ)
      }
      if prev != nil {
         prev.Wait()
      }
      fmt.Print(output)
      done.Done()
   }()
}

func count(dna []byte, n int) map[uint64]uint64 {
   tbl := make(map[uint64]uint64, (2<<16)+1)
   for i, end := 0, len(dna)-n; i < end; i++ {
      tbl[compress(dna[i:i+n])]++
   }
   return tbl
}

func compress(dna []byte) uint64 {
   var val uint64
   for i, dnac := 0, len(dna); i < dnac; i++ {
      val = (val << 2) | uint64(dna[i])
   }
   return val
}

func compStr(dna string) uint64 {
   raw := []byte(dna)
   for i, rawc, c := 0, len(raw), byte(0); i < rawc; i++ {
      c = raw[i]
      switch c {
      case 'A':
         c = 0
      case 'T':
         c = 1
      case 'G':
         c = 2
      case 'C':
         c = 3
      }
      raw[i] = c
   }
   return compress(raw)
}

func decompToBytes(compDNA uint64, n int) []byte {
   buf := bytes.NewBuffer(make([]byte, 0, n))
   var c byte
   for i := 0; i < n; i++ {
      switch compDNA & 3 {
      case 0:
         c = 'A'
      case 1:
         c = 'T'
      case 2:
         c = 'G'
      case 3:
         c = 'C'
      }
      buf.WriteByte(c)
      compDNA = compDNA >> 2
   }
   if n > 1 {
      return reverse(buf.Bytes())
   }
   return buf.Bytes()
}

func reverse(s []byte) []byte {
   for i, j := 0, len(s)-1; i < j; i, j = i+1, j-1 {
      s[i], s[j] = s[j], s[i]
   }
   return s
}

func freqReport(tbl map[uint64]uint64, n int) string {
   seqs := make(seqSlice, 0, len(tbl))
   var val, count, sum uint64
   for val, count = range tbl {
      seqs = append(seqs, seq{nuc: decompToBytes(val, n), n: count})
      sum += count
   }
   sort.Sort(seqs)
   var buf bytes.Buffer
   sumFloat, entry := float64(sum), seq{}
   for _, entry = range seqs {
      fmt.Fprintf(&buf, "%s %.3f\n", entry.nuc, (100*float64(entry.n))/sumFloat)
   }
   buf.WriteByte('\n')
   return buf.String()
}

type seq struct {
   nuc []byte
   n   uint64
}

type seqSlice []seq

func (seq seqSlice) Len() int      { return len(seq) }
func (seq seqSlice) Swap(i, j int) { seq[i], seq[j] = seq[j], seq[i] }
func (seq seqSlice) Less(i, j int) bool {
   if seq[i].n == seq[j].n {
      return bytes.Compare(seq[i].nuc, seq[j].nuc) < 0
   }
   return seq[i].n > seq[j].n
}
