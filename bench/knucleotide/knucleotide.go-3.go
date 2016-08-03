/* The Computer Language Benchmarks Game
 * http://benchmarksgame.alioth.debian.org/
 *
 * contributed by Jason Alan Palmer
 */


package main

import (
   "bufio"
   "bytes"
   "fmt"
   "os"
   "sort"
   "strings"
)

var toNum = strings.NewReplacer(
   "A", string(0),
   "C", string(1),
   "G", string(2),
   "T", string(3),
   "a", string(0),
   "c", string(1),
   "g", string(2),
   "t", string(3),
)
var toChar = strings.NewReplacer(
   string(0), "A",
   string(1), "C",
   string(2), "G",
   string(3), "T",
)

func main() {
   dna := read()
   c3 := make(chan string)
   c4 := make(chan string)
   c6 := make(chan string)
   c12 := make(chan string)
   c18 := make(chan string)
   go func() { c3 <- seqReport(dna, "GGT") }()
   go func() { c4 <- seqReport(dna, "GGTA") }()
   go func() { c6 <- seqReport(dna, "GGTATT") }()
   go func() { c12 <- seqReport(dna, "GGTATTTTAATT") }()
   go func() { c18 <- seqReport(dna, "GGTATTTTAATTTATAGT") }()
   fmt.Println(freqReport(dna, 1))
   fmt.Println(freqReport(dna, 2))
   fmt.Println(<-c3)
   fmt.Println(<-c4)
   fmt.Println(<-c6)
   fmt.Println(<-c12)
   fmt.Println(<-c18)
}

func read() []byte {
   var buf bytes.Buffer
   scanner := bufio.NewScanner(os.Stdin)
   for !strings.HasPrefix(scanner.Text(), ">THREE") {
      scanner.Scan()
   }
   for scanner.Scan() {
      buf.WriteString(toNum.Replace(scanner.Text()))
   }
   return buf.Bytes()
}

type sequence struct {
   nucs  string
   count int
}
type sequenceSlice []sequence

func (ss sequenceSlice) Len() int {
   return len(ss)
}
func (ss sequenceSlice) Swap(i, j int) {
   ss[i], ss[j] = ss[j], ss[i]
}
func (ss sequenceSlice) Less(i, j int) bool {
   if ss[i].count == ss[j].count {
      return ss[i].nucs > ss[j].nucs
   }
   return ss[i].count > ss[j].count
}

func freqReport(dna []byte, length int) string {
   var buf bytes.Buffer
   var sortedSeqs sequenceSlice
   counts := count(dna, length)
   for num, pointer := range counts {
      sortedSeqs = append(
         sortedSeqs,
         sequence{toChar.Replace(string(decompress(num, length))), *pointer},
      )
   }
   sort.Sort(sortedSeqs)
   for _, seq := range sortedSeqs {
      buf.WriteString(fmt.Sprintf(
         "%v %.3f\n", seq.nucs,
         100.0*float32(seq.count)/float32(len(dna)-length+1)),
      )
   }
   return buf.String()
}

func seqReport(dna []byte, seq string) string {
   var seq_count int
   counts := count(dna, len(seq))
   pointer := counts[compress([]byte(toNum.Replace(seq)))]
   if pointer != nil {
      seq_count = *pointer
   }
   return fmt.Sprintf("%v\t%v", seq_count, seq)
}

func count(dna []byte, length int) map[uint64]*int {
   counts := make(map[uint64]*int)
   for index := 0; index < len(dna)-length+1; index++ {
      key := compress(dna[index : index+length])
      pointer, ok := counts[key]
      if !ok {
         pointer = new(int)
         counts[key] = pointer
      }
      *pointer++
   }
   return counts
}

func compress(seq []byte) uint64 {
   var num uint64
   for _, char := range seq {
      num = (num << 2) | uint64(char)
   }
   return num
}

func decompress(num uint64, length int) []byte {
   seq := make([]byte, length)
   for i := 0; i < length; i++ {
      seq[length-i-1] = byte(num & 3)
      num = num >> 2
   }
   return seq
}
