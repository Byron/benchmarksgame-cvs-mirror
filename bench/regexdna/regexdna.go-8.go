/* The Computer Language Benchmarks Game
 * http://benchmarksgame.alioth.debian.org/
 *
 * contributed by The Go Authors.
 * modified by Tylor Arndt.
 */

package main

import (
   "fmt"
   "github.com/glenn-brown/golang-pkg-pcre/src/pkg/pcre"
   "io/ioutil"
   "os"
   "runtime"
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

type Subst struct {
   pat, repl string
}

var substs = []Subst{
   Subst{"B", "(c|g|t)"},
   Subst{"D", "(a|g|t)"},
   Subst{"H", "(a|c|t)"},
   Subst{"K", "(g|t)"},
   Subst{"M", "(a|c)"},
   Subst{"N", "(a|c|g|t)"},
   Subst{"R", "(a|g)"},
   Subst{"S", "(c|g)"},
   Subst{"V", "(a|c|g)"},
   Subst{"W", "(a|t)"},
   Subst{"Y", "(c|t)"},
}

func countMatches(pat string, bytes []byte) int {
   re := pcre.MustCompile(pat, 0)
   n := 0
   for {
      e := re.FindIndex(bytes, 0)
      if e == nil {
         break
      }
      n++
      bytes = bytes[e[1]:]
   }
   return n
}

func main() {
   runtime.GOMAXPROCS(4)
   bytes, err := ioutil.ReadAll(os.Stdin)
   if err != nil {
      fmt.Fprintf(os.Stderr, "can't read input: %s\n", err)
      os.Exit(2)
   }
   ilen := len(bytes)
   // Delete the comment lines and newlines
   bytes = pcre.MustCompile("(>[^\n]+)?\n", 0).ReplaceAll(bytes, []byte{}, 0)
   clen := len(bytes)

   mresults := make([]chan int, len(variants))
   var i int
   var s string
   for i, s = range variants {
      ch := make(chan int)
      mresults[i] = ch
      go func(intch chan int, ss string) {
         intch <- countMatches(ss, bytes)
      }(ch, s)
   }

   lenresult := make(chan int)
   bb := bytes
   go func() {
      for _, sub := range substs {
         bb = pcre.MustCompile(sub.pat, 0).ReplaceAll(bb, []byte(sub.repl), 0)
      }
      lenresult <- len(bb)
   }()

   for i, s = range variants {
      fmt.Printf("%s %d\n", s, <-mresults[i])
   }
   fmt.Printf("\n%d\n%d\n%d\n", ilen, clen, <-lenresult)
}
