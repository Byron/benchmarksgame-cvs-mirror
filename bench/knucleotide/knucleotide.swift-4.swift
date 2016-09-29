/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   contributed by Ralph Ganszky
   modified by Michael Morrell
*/

import Glibc

func compress(_ seq: ArraySlice<Int8>) -> Int {
    var res = 0
    for i in seq.indices {
        res = (res << 2) | Int(seq[i])
    }
    return res
}

func getSequenceHash(_ n: Int, seq: [Int8]) -> [Int:Int] {
    var hash = [Int:Int]()
    let mask = n > 1 ? ((1 << (2*(n-1))) - 1) : 0
    var idx = compress(seq[0..<n])
    hash[idx] = (hash[idx] ?? 0) + 1
    for i in n..<seq.count {
        idx = ((idx & mask) << 2) | Int(seq[i])
        hash[idx] = (hash[idx] ?? 0) + 1
    }
    return hash
}

let c2i: [Character:Int] = [ "A": 0, "C": 1, "T": 2, "G": 3 ]

func encode(_ seq: String) -> Int {
    let cSeq = seq.characters
    var res = 0
    for c in cSeq {
        res = res << 2 | c2i[c]!
    }
    return res
}

func roundDouble(_ num: Double, precision: Int) -> String {
    let exponent = pow(10.0, Double(precision))
    let number = Double(Int(num * exponent + 0.5)) / exponent
    var numberStr = "\(number)"
    while numberStr.characters.count < Int(log10(num)) + 2 + precision {
        numberStr = numberStr + "0"
    }
    return numberStr
}

func readInput(_ inStream: UnsafeMutablePointer<FILE>) -> [Int8] {
    var seq = [Int8]()
    var buf = UnsafeMutablePointer<Int8>.allocate(capacity: 100)
    defer {
        buf.deallocate(capacity: 100)
    }
    let pattern = ">THREE Homo sapiens frequency"
    let pat = pattern.withCString({ s in s })
    let patLen = pattern.utf8CString.count - 1
    var skipped = 0;
    var appended = 0;
    while memcmp(buf, pat, patLen) != 0  {
        buf = fgets(buf, 100, inStream)
        skipped += 100
    }
    while fgets(buf, 100, inStream) != nil {
        let buffer = UnsafeMutableBufferPointer(start: buf, count: 60)
        seq.append(contentsOf: buffer)
        appended += buffer.count
    }
    print("Pattern length: \(patLen), cstring len: \(pattern.characters.count), Skipped#: \(skipped), Appended#: \(appended)")
    return seq
}

// Read sequence
let ins = stdin
var sequence = readInput(ins!)

// Check if last line ended premature
let offset = sequence.count - 60
for i in 0..<60 {
    if sequence[offset + i] == 10 {
        let remain = 60 - i
        sequence.removeLast(remain)
        break
    }
}

var a_count = 0
var c_count = 0
var t_count = 0
var g_count = 0
// rewrite bytes with 2bit code
for i in 0..<sequence.count {
    sequence[i] = (sequence[i] & 0x6) >> 1
    if sequence[i] == 0 { a_count += 1 }
    if sequence[i] == 1 { c_count += 1 }
    if sequence[i] == 2 { t_count += 1 }
    if sequence[i] == 3 { g_count += 1 }
}
print("From Sequence:")
print("Total = \(sequence.count)")
print("A count = \(a_count)")
print("C count = \(c_count)")
print("T count = \(t_count)")
print("G count = \(g_count)")

let hash = getSequenceHash(1, seq: sequence)

let i2c = [ 0: "A", 1: "C", 2: "T", 3: "G" ]

let total = hash.reduce(0) { $0 + $1.1 }
print("From Hash:")
print("Total = \(total)")
print("A count = \(hash[0]!)")
print("C count = \(hash[1]!)")
print("T count = \(hash[2]!)")
print("G count = \(hash[3]!)")
for k in hash.keys.sorted(by: {hash[$1]! < hash[$0]!}) {
    print("\(i2c[k]!) \(roundDouble(100.0*Double(hash[k]!)/Double(total), precision: 3))")
}
