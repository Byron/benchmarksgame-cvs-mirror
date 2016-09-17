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
    // three colors in alphabetic order
    blue uint32 = 1 + iota
    red
    yellow
    colBits = 2
    colMask = 1<<colBits - 1
    noone   = 0
)

func complement(col1, col2 uint32) uint32 {
    switch col1 {
    case blue:
        switch col2 {
        case blue:
            return blue
        case red:
            return yellow
        case yellow:
            return red
        }
    case red:
        switch col2 {
        case blue:
            return yellow
        case red:
            return red
        case yellow:
            return blue
        }
    case yellow:
        switch col2 {
        case blue:
            return red
        case red:
            return blue
        case yellow:
            return yellow
        }
    }
    panic("Invalid colour in complement")
}

var colname = [...]string{
    blue:   "blue",
    red:    "red",
    yellow: "yellow",
}

// A creature is a naming identifier + a colour
type creature uint32

func newCreature(name, col uint32) creature {
    return creature(name<<colBits | col)
}

func (crea creature) name() uint32 {
    return uint32(crea) >> colBits
}

func (crea creature) colour() uint32 {
    return uint32(crea) & colMask
}

// mutate updates the colour after a mate was found
func (crea creature) mutate(mate creature) creature {
    newColour := complement(crea.colour(), mate.colour())
    return creature(uint32(crea)&^colMask | uint32(newColour))
}

// word type-converts a creature to a single word
func (crea creature) word() uint32 {
    return uint32(crea)
}

// mall organizes meetings between creatures
type mall struct {
    meetings   *uint32   // count number of meetings
    waiter     *uint32   // waiting creature if non-zero
    handshakes []*uint32 // mating handshakes indexed by name
}

// result is sent by a creature when game ends
type result struct {
    met  int // number of meetings with any
    same int // number of meetings with self
}

// game configuration
type game struct {
    colours []uint32
    results chan result
}

// total meetings per game of pallmall
var meetings = 600

func init() {
    if flag.Parse(); flag.NArg() > 0 {
        meetings, _ = strconv.Atoi(flag.Arg(0))
    }
}

// main starts two games and outputs results
func main() {
    game1 := &game{[]uint32{blue, red, yellow}, make(chan result)}
    go game1.pallmall()

    game2 := &game{[]uint32{blue, red, yellow, red, yellow,
        blue, red, yellow, red, blue}, make(chan result)}
    go game2.pallmall()

    printColours()
    game1.report()
    game2.report()
}

// printColours prints colour complements.
func printColours() {
    for col1 := blue; col1 <= yellow; col1++ {
        for col2 := blue; col2 <= yellow; col2++ {
            fmt.Printf("%s + %s -> %s\n",
                colname[col1], colname[col2],
                colname[complement(uint32(col1), uint32(col2))])
        }
    }
    fmt.Print("\n")
}

// report game colours and results
func (game *game) report() {
    // print game colours
    msg := ""
    for _, col := range game.colours {
        msg += " " + colname[col]
    }
    fmt.Println(msg)

    // wait for game results
    total := 0
    for _ = range game.colours {
        result := <-game.results
        total += result.met
        fmt.Printf("%d%s\n", result.met, spell(result.same))
    }
    fmt.Printf("%s\n\n", spell(total))
}

// pallmall starts a new game
func (game *game) pallmall() {
    // assuming a cache line size of 64 bytes.
    // give each element its own cache line to reduce contention.
    slab := make([]uint32, (4+len(game.colours))*16)
    handshakes := make([]*uint32, len(game.colours)+1)
    mall := &mall{&slab[16], &slab[2*16], handshakes}
    // each colour is one new creature
    for i, col := range game.colours {
        handshakes[i+1] = &slab[(3+i)*16]
        creature := newCreature(uint32(i+1), col)
        go creature.play(mall, game.results)
    }
}

// play realizes meetings for one creature with other creatures
func (crea creature) play(mall *mall, ended chan<- result) {
    handshake := mall.handshakes[crea.name()]
    result := result{}
    // obtain a license for a new meeting, until meetings are over
    for atom.AddUint32(mall.meetings, 1) <= uint32(2*meetings) {
        var mate creature
        for {
            // see if some other is already inside
            other := atom.LoadUint32(mall.waiter)
            if other == noone {
                // the room is still empty; try to get in first
                if atom.CompareAndSwapUint32(mall.waiter, noone, crea.word()) {
                    // we got in first; now wait for second creature
                    for i := 0; other == noone; i++ {
                        if i >= 50 {
                            // limit idle spinning
                            runtime.Gosched()
                        }
                        other = atom.LoadUint32(handshake)
                    }
                    // we found a mate
                    mate = creature(other)
                    // reset
                    *handshake = noone
                    break
                }
            } else {
                // someone is inside; try to enter as second
                if atom.CompareAndSwapUint32(mall.waiter, other, noone) {
                    // we found a mate
                    mate = creature(other)
                    // tell our mate about ourselves.
                    atom.StoreUint32(mall.handshakes[mate.name()], crea.word())
                    break
                }
            }
        }
        // update our colour
        crea = crea.mutate(mate)
        // count successful meetings
        result.met++
        if mate.name() == crea.name() {
            // count meetings with ourselves
            result.same++
        }
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
