/* The Computer Language Benchmarks Game
 * http://benchmarksgame.alioth.debian.org/
 *
 * contributed by Dirk Moerenhout
 * based on work by Jason Alan Palmer and the Go Authors
 */

package main

import (
    "bufio"
    "bytes"
    "fmt"
    "io/ioutil"
    "os"
    "sort"
    "strings"
)

var toNum = strings.NewReplacer(
    "A", string(0),
    "C", string(1),
    "G", string(3),
    "T", string(2),
)

var toChar = strings.NewReplacer(
    string(0), "A",
    string(1), "C",
    string(3), "G",
    string(2), "T",
)

func main() {
    dna := read()
    c1 := make(chan string)
    c2 := make(chan string)
    c4 := make(chan string)
    c6 := make(chan string)
    c12 := make(chan string)
    c18 := make(chan string)
    go func() { c18 <- seqReport(dna, "GGTATTTTAATTTATAGT") }()
    go func() { c1 <- freqReport(dna, 1) }()
    go func() { c2 <- freqReport(dna, 2) }()
    c3 := seqReport(dna, "GGT")
    go func() { c4 <- seqReport(dna, "GGTA") }()
    go func() { c6 <- seqReport(dna, "GGTATT") }()
    go func() { c12 <- seqReport(dna, "GGTATTTTAATT") }()
    fmt.Println(<-c1)
    fmt.Println(<-c2)
    fmt.Println(c3)
    fmt.Println(<-c4)
    fmt.Println(<-c6)
    fmt.Println(<-c12)
    fmt.Println(<-c18)
}

func read() []byte {
    in := bufio.NewReader(os.Stdin)
    line, _ := in.Peek(61)
    for ; !strings.HasPrefix(string(line), ">THREE"); line, _ = in.Peek(61) {
        if line[60] == '\n' {
            in.Discard(61)
            continue
        }
        line, _ = in.ReadSlice('\n')
    }
    line, _ = in.ReadSlice('\n')

    data, _ := ioutil.ReadAll(in)

    j := 0
    for i := 0; i < len(data); i++ {
        if data[i] != '\n' {
            data[j] = (byte(data[i]) >> 1) & 3
            j++
        }
    }
    return data[0:j]
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
    key := compress(dna[0 : length-1])
    mask := (uint64(1) << (uint(length) * 2)) - 1
    for index := length - 1; index < len(dna); index++ {
        key = ((key << 2) & mask) | uint64(dna[index])
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
