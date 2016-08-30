/* The Computer Language Benchmarks Game
 * http://benchmarksgame.alioth.debian.org/
 *
 * contributed by The Go Authors.
 * modified by Glenn Brown.
 * modified by Scott Kirkwood.
 */
package main

import (
   "fmt"
   "io/ioutil"
   "os"
   "sync"

   "github.com/glenn-brown/golang-pkg-pcre/src/pkg/pcre"
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

var substs = []struct {
   pat, repl string
}{
   {"B", "(c|g|t)"},
   {"D", "(a|g|t)"},
   {"H", "(a|c|t)"},
   {"K", "(g|t)"},
   {"M", "(a|c)"},
   {"N", "(a|c|g|t)"},
   {"R", "(a|g)"},
   {"S", "(c|g)"},
   {"V", "(a|c|g)"},
   {"W", "(a|t)"},
   {"Y", "(c|t)"},
}

func countMatches(search string, b []byte) (count int) {
   re := pcre.MustCompile(search, 0)
   e := []int{0, 0}
   for count = 0; ; count++ {
      e = re.FindIndex(b, 0)
      if e == nil {
         break
      }
      b = b[e[1]:]
   }
   return count
}

func regexDNAReport(b []byte) {
   inputLen := len(b)

   // Delete the comment lines and newlines
   b = pcre.MustCompile("(>[^\n]+)?\n", 0).
      ReplaceAll(b, []byte{}, 0)
   commentlessLen := len(b)

   var wg sync.WaitGroup

   counts := make([]int, len(variants))
   for i, s := range variants {
      wg.Add(1)
      go func(i int, s string) {
         counts[i] = countMatches(s, b)
         wg.Done()
      }(i, s)
   }

   // Alias the original b slice, probably not required.
   bb := b

   wg.Add(1)
   go func() {
      for _, sub := range substs {
         bb = pcre.MustCompile(sub.pat, 0).
            ReplaceAll(bb, []byte(sub.repl), 0)
      }
      wg.Done()
   }()

   wg.Wait()
   for i, s := range variants {
      fmt.Printf("%s %d\n", s, counts[i])
   }
   fmt.Printf("\n%d\n%d\n%d\n",
      inputLen, commentlessLen, len(bb))
}

func main() {
   b, err := ioutil.ReadAll(os.Stdin)
   if err != nil {
      fmt.Fprintf(os.Stderr, "can't read input: %s\n", err)
      os.Exit(2)
   }
   regexDNAReport(b)
}
