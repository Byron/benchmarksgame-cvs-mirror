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

    "github.com/gijsbers/go-pcre"
)

const (
    comFlags = pcre.NEVER_UTF
    jitFlags = pcre.STUDY_JIT_COMPILE
)

var variants = []string{
    "agggtaaa|tttaccct",
    "[cgt]gggtaaa|tttaccc[acg]",
    "a[act]ggtaaa|tttacc[agt]t",
    "ag[act]gtaaa|tttac[agt]ct",
    "agg[act]taaa|ttta[agt]cct",
    "aggg[acg]aaa|ttt[cgt]ccct",
    "agggt[cgt]aa|tt[acg]accct",
    "agggta[cgt]a|t[acg]taccct",
    "agggtaa[cgt]|[acg]ttaccct",
}

type Substitution struct {
    pattern string
    replace string
    regexp  *pcre.Regexp
}

var substs = []Substitution{
    {"B", "(c|g|t)", nil},
    {"D", "(a|g|t)", nil},
    {"H", "(a|c|t)", nil},
    {"K", "(g|t)", nil},
    {"M", "(a|c)", nil},
    {"N", "(a|c|g|t)", nil},
    {"R", "(a|g)", nil},
    {"S", "(c|g)", nil},
    {"V", "(a|c|g)", nil},
    {"W", "(a|t)", nil},
    {"Y", "(c|t)", nil},
}

func countMatches(pattern string, bytes []byte) (count int) {
    m := pcre.MustCompileJIT(pattern, comFlags, jitFlags).Matcher(bytes, 0)
    for f := m.Matches(); f; f = m.Match(bytes, 0) {
        count++
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
    var regexp = pcre.MustCompileJIT("^>.*|\n", comFlags, jitFlags)
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
    for i := range substs {
        re := pcre.MustCompileJIT(substs[i].pattern, comFlags, jitFlags)
        substs[i].regexp = &re
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
            varResults[k] <- countMatches(variants[k], bytes)
        }(i)
    }

    subResult := make(chan int, 1)
    go substitute(bytes, subResult)

    for i := range variants {
        fmt.Printf("%s %d\n", variants[i], <-varResults[i])
    }

    fmt.Printf("\n%d\n%d\n%d\n", inputLength, cleanLength, <-subResult)
}
