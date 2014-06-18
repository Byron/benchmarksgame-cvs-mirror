/* The Computer Language Benchmarks Game
 * http://benchmarksgame.alioth.debian.org/
 *
 * contributed by Branimir Maksimovic
 */

package main

import (
   "bufio"
   "bytes"
   "fmt"
   "os"
   "sort"
   "runtime"
)

var (
   toNum [256]byte
   toChar [256]byte
)

func InitTables () {
   toNum['A'] = 0
   toNum['C'] = 1
   toNum['T'] = 2
   toNum['G'] = 3
   toNum['a'] = 0
   toNum['c'] = 1
   toNum['t'] = 2
   toNum['g'] = 3
   
   toChar[0] = 'A'
   toChar[1] = 'C'
   toChar[2] = 'T'
   toChar[3] = 'G'
}
const SIZE = 1<<16
type HTable struct {
   table *[SIZE]*Node
}
type Node struct {
   data T
   next *Node
}
func NewTable() HTable {
   return HTable{ &[SIZE]*Node{} }
}
func (t* HTable) Get(input T) (*T,bool) {
   hash := input.Hash()
   slot := hash & (SIZE-1)
   n := t.table[slot] 
   if n == nil {
      n = &Node{ input,nil }
      t.table[slot] = n
      return &n.data,false
   }
   for ;n != nil;n=n.next {
      if n.data.Equal(input) {
         return &n.data,true
      }
   } 
   n = &Node{ input, t.table[slot] }
   t.table[slot] = n
   return &n.data,false
}
func (t* HTable) ForEach(f func(T)) {
   for _,v := range t.table {
      for ;v != nil;v = v.next {
         f(v.data)
      }
   }
}

type T struct { 
   data uint64
   count int
   size byte
}
func Pack(input string) T {
   t := T{0,0,byte(len(input))}
   for i := 0 ; i < int(t.size); i++ {
      t.data <<= 2
      t.data |= uint64(toNum[input[i]])
   }
   return t
}
func (t* T) String() string {
   rc := []byte{}
   tmp := t.data
   for i:=0 ;i < int(t.size); i++ {
      rc = append(rc, toChar[tmp & 3])
      tmp >>= 2
   }
   for i:=0;i<len(rc)/2;i++ {
      tmp := rc[i]
      rc[i] = rc[len(rc)-i-1] 
      rc[len(rc) - i -1] = tmp
   }
   return string(rc)
}
func (t *T) Hash() uint64 {
   return t.data
}
func (t *T) Equal(in T) bool {
   return t.data == in.data
}

func calculate(input string,size int,begin int, step int,snd chan HTable){
   rc := NewTable()
   for i := begin;i<len(input)+1-size;i+=step {
      d,_ := rc.Get(Pack(input[i:i+size]))
      d.count++
   }
   snd <- rc
}

func Tcalculate(input string,size int) (rc HTable){
   var futures [8]chan HTable
   rc = NewTable()
   for i := 0;i<8; i++ {
      futures[i] = make(chan HTable)
      go calculate(input,size,i,8,futures[i])
   }
   var res [8]HTable
   for i := 0;i<8;i++ {
      res[i] = <- futures[i]
      f := func(in T) {
         d,ok := rc.Get(in)
         if ok {
            d.count += in.count
         }
      }
      res[i].ForEach(f)
   }
   return
}

func WriteFrequencies(input string,size int){
   var sum int = len(input) + 1 - size
   frequencies := Tcalculate(input,size)
   freq, mfreq := []int{},map[int]*T{}
   f := func(in T) {
      freq = append(freq,in.count)
      mfreq[in.count]=&in
   }
   frequencies.ForEach(f)
   sort.Sort(sort.Reverse(sort.IntSlice(freq)))
   for _,k := range freq {
      var val float64
      if sum == 0 {
         val = 0.0
      } else {
         val = float64(100*k)/float64(sum)
      }
      fmt.Printf("%s %.3f\n",mfreq[k].String(),val)
   }
   fmt.Println()
}

func WriteCount(input string,label string) {
   frequencies := Tcalculate(input,len(label))
   d,_ := frequencies.Get(Pack(label))
   fmt.Printf("%d\t%s\n",d.count,label)
}

func main() {
   runtime.GOMAXPROCS(4)
   InitTables()
   
   in := bufio.NewScanner(os.Stdin)
   three := []byte(">THREE")
   for {
      in.Scan()
      if bytes.HasPrefix(in.Bytes(), three) {
         break
      }
   }

   input := make([]byte,0,125000000)
   for in.Scan() {
      input = append(input,in.Bytes()...)
   }
   sinput := string(input)
   input = nil

   WriteFrequencies(sinput,1)
   WriteFrequencies(sinput,2)
   
   WriteCount(sinput, "GGT")
   WriteCount(sinput, "GGTA")
   WriteCount(sinput, "GGTATT")
   WriteCount(sinput, "GGTATTTTAATT")
   WriteCount(sinput, "GGTATTTTAATTTATAGT")
}
