/* The Computer Language Benchmarks Game
 * http://benchmarksgame.alioth.debian.org/
 *
 * contributed by The Go Authors.
 * modified by Tylor Arndt.
 * modified by Chandra Sekar S to use optimized PCRE binding.
 * modified by Bert Gijsbers.
 */

package main

import (
    "fmt"
    "io/ioutil"
    "os"
    "runtime"
    "sync"

    "github.com/tuxychandru/golang-pkg-pcre/src/pkg/pcre"
)

type Variant struct {
    pattern string
    regexp  pcre.Regexp
}

var variants = []Variant{
    {"agggtaaa|tttaccct", pcre.Regexp{}},
    {"[cgt]gggtaaa|tttaccc[acg]", pcre.Regexp{}},
    {"a[act]ggtaaa|tttacc[agt]t", pcre.Regexp{}},
    {"ag[act]gtaaa|tttac[agt]ct", pcre.Regexp{}},
    {"agg[act]taaa|ttta[agt]cct", pcre.Regexp{}},
    {"aggg[acg]aaa|ttt[cgt]ccct", pcre.Regexp{}},
    {"agggt[cgt]aa|tt[acg]accct", pcre.Regexp{}},
    {"agggta[cgt]a|t[acg]taccct", pcre.Regexp{}},
    {"agggtaa[cgt]|[acg]ttaccct", pcre.Regexp{}},
}

type Substitution struct {
    pattern string
    replace string
    regexp  pcre.Regexp
}

var substs = []Substitution{
    {"B", "(c|g|t)", pcre.Regexp{}},
    {"D", "(a|g|t)", pcre.Regexp{}},
    {"H", "(a|c|t)", pcre.Regexp{}},
    {"K", "(g|t)", pcre.Regexp{}},
    {"M", "(a|c)", pcre.Regexp{}},
    {"N", "(a|c|g|t)", pcre.Regexp{}},
    {"R", "(a|g)", pcre.Regexp{}},
    {"S", "(c|g)", pcre.Regexp{}},
    {"V", "(a|c|g)", pcre.Regexp{}},
    {"W", "(a|t)", pcre.Regexp{}},
    {"Y", "(c|t)", pcre.Regexp{}},
}

func (variant *Variant) countMatches(bytes []byte) (n int) {
    m := variant.regexp.Matcher(bytes, 0)
    for f := m.Matches(); f; f = m.Match(bytes, 0) {
        n++
        bytes = bytes[m.Index()[1]:]
    }
    return
}

func readInput() (bytes []byte) {
    bytes, err := ioutil.ReadAll(os.Stdin)
    if err != nil {
        fmt.Fprintf(os.Stderr, "can't read input: %s\n", err)
        os.Exit(2)
    }
    return
}

// cleanData removes descriptions and newlines
func cleanData(bytes []byte) []byte {
    var regexp = pcre.MustCompile("^>.*|\n", 0)
    var wg sync.WaitGroup
    ncpu := runtime.NumCPU()
    done := make([][]byte, ncpu)
    blen := len(bytes)
    todo := blen
    for i := 0; i < ncpu; i++ {
        size := todo / (ncpu - i)
        for size < todo && bytes[blen-todo+size-1] != '\n' {
            size++
        }
        work := bytes[blen-todo : blen-todo+size]
        wg.Add(1)
        go func(work []byte, index int) {
            done[index] = regexp.ReplaceAll(work, []byte{}, 0)
            wg.Done()
        }(work, i)
        todo -= size
    }
    wg.Wait()
    size := 0
    for i := 0; i < ncpu; i++ {
        size += len(done[i])
    }
    clean := make([]byte, 0, size)
    for i := 0; i < ncpu; i++ {
        clean = append(clean, done[i]...)
    }
    return clean
}

func substitute(bytes []byte, resultChan chan<- int) {
    var wg sync.WaitGroup
    ncpu := runtime.NumCPU()
    done := make([][]byte, ncpu)
    blen := len(bytes)
    todo := blen
    for i := 0; i < ncpu; i++ {
        size := todo / (ncpu - i)
        work := bytes[blen-todo : blen-todo+size]
        wg.Add(1)
        go func(work []byte, index int) {
            for i := range substs {
                sub := &substs[i]
                work = sub.regexp.ReplaceAll(work, []byte(sub.replace), 0)
            }
            done[index] = work
            wg.Done()
        }(work, i)
        todo -= size
    }
    wg.Wait()
    var sum int
    for i := 0; i < ncpu; i++ {
        sum += len(done[i])
    }
    resultChan <- sum
}

func precompile() {
    for i := range variants {
        variants[i].regexp = pcre.MustCompile(variants[i].pattern, 0)
    }
    for i := range substs {
        substs[i].regexp = pcre.MustCompile(substs[i].pattern, 0)
    }
}

func main() {
    var wg sync.WaitGroup
    wg.Add(1)
    go func() {
        precompile()
        wg.Done()
    }()

    bytes := readInput()
    inputLength := len(bytes)
    bytes = cleanData(bytes)
    cleanLength := len(bytes)

    wg.Wait()
    varResults := make([]chan int, len(variants))
    for i := range variants {
        varResults[i] = make(chan int, 1)
        go func(k int) {
            varResults[k] <- (&variants[k]).countMatches(bytes)
        }(i)
    }

    subResult := make(chan int, 1)
    go substitute(bytes, subResult)

    for i := range variants {
        fmt.Printf("%s %d\n", variants[i].pattern, <-varResults[i])
    }

    fmt.Printf("\n%d\n%d\n%d\n", inputLength, cleanLength, <-subResult)
}
