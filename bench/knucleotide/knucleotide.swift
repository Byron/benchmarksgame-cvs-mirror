/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   contributed by Ralph Ganszky
*/

import Glibc
import Cdispatch
import CBlock

let ntasks = 20

let queue = dispatch_get_global_queue(Int(DISPATCH_QUEUE_PRIORITY_DEFAULT), 0)
let mQueue = dispatch_queue_create("mergeQueue", nil)

func compress(n: Int, seq: ArraySlice<Int8>) -> Int {
    var res = 0
    for i in seq.indices {
        res = (res << 2) | Int(seq[i])
    }
    return res
}

func getSequenceHash(n: Int, seq: [Int8]) -> [Int:Int] {
    var hash = [Int:Int]()
    let slice = (seq.count-(n-1)) / ntasks
    let remainder = (seq.count-(n-1)) % ntasks
    dispatch_apply(ntasks, queue) { i in
        var lHash = [Int:Int](minimumCapacity: 1 << min(n, 12))
        for l in i*slice..<(i+1)*slice {
            let idx = compress(n, seq: seq[l..<(l+n)])
            lHash[idx] = (lHash[idx] ?? 0) + 1
        }
        dispatch_sync(mQueue) {
            for (key, value) in lHash {
                hash[key] = (hash[key] ?? 0) + value
            }
        }
    }
    let startIdx = seq.count - remainder - (n - 1)
    let endIdx = seq.count - (n - 1)
    for i in startIdx..<endIdx {
        let idx = compress(n, seq: seq[i..<(i+n)])
        hash[idx] = (hash[idx] ?? 0) + 1
    }
    return hash
}

let c2i: [Character:Int] = [ "A": 0, "C": 1, "T": 2, "G": 3 ]

func encode(seq: String) ->  Int {
    let cSeq = seq.characters
    var res = 0
    for c in cSeq {
        res = res << 2 | c2i[c]!
    }
    return res
}

func roundDouble(num: Double, precision: Int) -> String {
    let exponent = pow(10.0, Double(precision))
    let number = Double(Int(num * exponent + 0.5)) / exponent
    var numberStr = "\(number)"
    while numberStr.characters.count < Int(log10(num)) + 2 + precision {
        numberStr = numberStr + "0"
    }
    return numberStr
}

func readInput(inStream: UnsafeMutablePointer<FILE>) -> [Int8] {
    var seq = [Int8]()
    var buf = UnsafeMutablePointer<Int8>.alloc(100)
    defer {
        buf.dealloc(100)
    }
    let pattern = ">THREE Homo sapiens frequency"
    let pat = pattern.withCString({ s in s })
    let patLen = pattern.nulTerminatedUTF8.count - 1
    while memcmp(buf, pat, patLen) != 0  {
        buf = fgets(buf, 100, inStream)
    }
    buf = fgets(buf, 100, inStream)
    while buf != nil {
        seq.appendContentsOf(UnsafeMutableBufferPointer(start: buf, count: 60))
        buf = fgets(buf, 100, inStream)
    }
    return seq
}

// Read sequence
let ins = stdin
var sequence = readInput(ins)

// Check if last line ended premature
let offset = sequence.count - 60
for i in 0..<60 {
    if sequence[offset + i] == 10 {
        let remain = 60 - i
        sequence.removeLast(remain)
        break
    }
}

// rewrite bytes with 2bit code
for i in 0..<sequence.count {
    sequence[i] = (sequence[i] & 0x6) >> 1
}

let hash = getSequenceHash(1, seq: sequence)

let i2c = [ 0: "A", 1: "C", 2: "T", 3: "G" ]

let total = hash.reduce(0, combine: { $0 + $1.1 })
for k in hash.keys.sort({hash[$1] < hash[$0]}) {
    print("\(i2c[k]!) \(roundDouble(100.0*Double(hash[k]!)/Double(total), precision: 3))")
}
print()

let hash2 = getSequenceHash(2, seq: sequence)

let total2 = hash2.reduce(0, combine: { $0 + $1.1 })
for k in hash2.keys.sort({hash2[$1] < hash2[$0]}) {
    print("\(i2c[k>>2]!)\(i2c[k&3]!) \(roundDouble(100.0*Double(hash2[k]!)/Double(total2), precision: 3))")
}
print()

let hash3 = getSequenceHash(3, seq: sequence)
print("\(hash3[encode("GGT")]!)\tGGT")

let hash4 = getSequenceHash(4, seq: sequence)
print("\(hash4[encode("GGTA")]!)\tGGTA")

let hash6 = getSequenceHash(6, seq: sequence)
print("\(hash6[encode("GGTATT")]!)\tGGTATT")

let hash12 = getSequenceHash(12, seq: sequence)
print("\(hash12[encode("GGTATTTTAATT")]!)\tGGTATTTTAATT")

let hash18 = getSequenceHash(18, seq: sequence)
print("\(hash18[encode("GGTATTTTAATTTATAGT")]!)\tGGTATTTTAATTTATAGT")
