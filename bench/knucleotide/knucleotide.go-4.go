/* The Computer Language Benchmarks Game
 * http://benchmarksgame.alioth.debian.org/
 *
 * contributed by Jason Alan Palmer
 * modified by Scott Kirkwood
 */

package main

import (
   "bufio"
   "bytes"
   "fmt"
   "os"
   "sort"
   "strconv"
   "strings"
   "sync"
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

func outputReport(dna []byte) {
   sequences := []string{
      "1", "2",
      "GGT", "GGTA", "GGTATT", "GGTATTTTAATT", "GGTATTTTAATTTATAGT",
   }
   results := make([]string, len(sequences))
   var wg sync.WaitGroup
   for i, seq := range sequences {
      wg.Add(1)
      go func(i int, seq string) {
         num, err := strconv.Atoi(seq)
         if err == nil && num > 0 {
            results[i] = freqReport(dna, num)
         } else {
            results[i] = seqReport(dna, seq)
         }
         wg.Done()
      }(i, seq)
   }
   wg.Wait()
   for _, result := range results {
      fmt.Println(result)
   }
}

func main() {
   outputReport(read())
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
   counts := count(dna, length)
   sortedSeqs := make(sequenceSlice, 0, len(counts))
   for num, counter := range counts {
      sortedSeqs = append(
         sortedSeqs,
         sequence{toChar.Replace(string(decompress(num, length))), counter},
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
   counts := count(dna, len(seq))
   seq_count := counts[compress([]byte(toNum.Replace(seq)))]
   return fmt.Sprintf("%v\t%v", seq_count, seq)
}

func count(dna []byte, length int) (counts map[uint64]int) {
   // The Benchmark Game wants us to use the default map initialization.
   // Big speed gains can be made by starting with larger map size.
   counts = make(map[uint64]int)
   for index := 0; index < len(dna)-length+1; index++ {
      counts[compress(dna[index:index+length])]++
   }
   return counts
}

func compress(seq []byte) (num uint64) {
   for _, char := range seq {
      num = (num << 2) | uint64(char)
   }
   return num
}

func decompress(num uint64, length int) (seq []byte) {
   seq = make([]byte, length)
   for i := 0; i < length; i++ {
      seq[length-i-1] = byte(num & 3)
      num = num >> 2
   }
   return seq
}
