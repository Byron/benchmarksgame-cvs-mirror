/* The Computer Language Benchmarks Game
 * http://benchmarksgameshootout.alioth.debian.org/
 *
 * contributed by The Go Authors.
 * modified by roger peppe
 * atomics by Bert Gijsbers.
 */

package main

import (
    "flag"
    "fmt"
    "runtime"
    "strconv"
    atom "sync/atomic"
)

const (
    blue = iota
    red
    yellow
    ncol
)

var complement = [...]int{
    red | red<<2:       red,
    red | yellow<<2:    blue,
    red | blue<<2:      yellow,
    yellow | red<<2:    blue,
    yellow | yellow<<2: yellow,
    yellow | blue<<2:   red,
    blue | red<<2:      yellow,
    blue | yellow<<2:   red,
    blue | blue<<2:     blue,
}

var colname = [...]string{
    blue:   "blue",
    red:    "red",
    yellow: "yellow",
}

// result sent by each creature at the end of processing.
type result struct {
    met  int // number of meetings with any
    same int // number of meetings with self
}

// creature's state needed for a meeting
type creature struct {
    selfColour int     // my current colour
    mateColour int     // colour of mate
    handshake  *uint32 // name of mate when first
}

// mall organizes meetings between creatures
type mall struct {
    meetings  *uint32    // count number of meetings
    waiter    *uint32    // waiting creature if non-zero
    creatures []creature // all creatures indexed by name
}

// total meetings per game of pallmall
var meetings = 600

func main() {
    runtime.GOMAXPROCS(runtime.NumCPU())

    if flag.Parse(); flag.NArg() > 0 {
        meetings, _ = strconv.Atoi(flag.Arg(0))
    }

    for c0 := 0; c0 < ncol; c0++ {
        for c1 := 0; c1 < ncol; c1++ {
            fmt.Printf("%s + %s -> %s\n",
                colname[c0], colname[c1],
                colname[complement[c0|c1<<2]])
        }
    }
    fmt.Print("\n")

    pallmall([]int{blue, red, yellow})
    pallmall([]int{blue, red, yellow, red, yellow,
        blue, red, yellow, red, blue})
}

// pallmall starts a new game and reports statistics
func pallmall(cols []int) {
    // construct a new game
    creatures := make([]creature, len(cols)+1)
    mall := &mall{new(uint32), new(uint32), creatures}
    ended := make(chan result)
    msg := ""
    for i, col := range cols {
        name := i + 1
        go play(name, col, mall, ended)
        msg += " " + colname[col]
    }
    fmt.Println(msg)

    // wait for all results
    total := 0
    for _ = range cols {
        result := <-ended
        total += result.met
        fmt.Printf("%v%v\n", result.met, spell(result.same))
    }
    fmt.Printf("%v\n\n", spell(total))
}

func play(selfName int, startColour int, mall *mall, ended chan<- result) {
    atomName := uint32(selfName)
    self := &mall.creatures[selfName]
    self.selfColour = startColour
    self.handshake = new(uint32)
    const noone = 0
    result := result{}
    // obtain a license for a new meeting, until meetings are over
    for atom.AddUint32(mall.meetings, 1) <= uint32(2*meetings) {
        var other uint32
        for {
            other = atom.LoadUint32(mall.waiter)
            if other == noone {
                // nobody is waiting; try to become the first to enter
                if atom.CompareAndSwapUint32(mall.waiter, noone, atomName) {
                    // success; now wait for second creature
                    for i := 0; ; i++ {
                        other = atom.LoadUint32(self.handshake)
                        if other != noone {
                            // second creature is done; clear flag
                            *self.handshake = noone
                            break
                        }
                        if i > 15 {
                            // limit idle spinning
                            runtime.Gosched()
                        }
                    }
                    break
                }
            } else if atom.CompareAndSwapUint32(mall.waiter, other, noone) {
                // we found a mate and are the second to enter
                // swap colour info with our mate
                self.mateColour = mall.creatures[other].selfColour
                mall.creatures[other].mateColour = self.selfColour
                // tell waiting mate that we are done
                atom.StoreUint32(mall.creatures[other].handshake, atomName)
                break
            }
        }
        self.selfColour = complement[self.selfColour|self.mateColour<<2]
        if other == atomName {
            result.same++
        }
        result.met++
    }
    ended <- result
}

var digits = [...]string{
    " zero", " one", " two", " three", " four",
    " five", " six", " seven", " eight", " nine"}

func spell(n int) string {
    msg := digits[n%10]
    for n /= 10; n != 0; n /= 10 {
        msg = digits[n%10] + msg
    }
    return msg
}
