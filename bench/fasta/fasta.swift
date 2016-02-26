/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/
   contributed by Ralph Ganszky
*/

import Glibc

typealias AminoAcid = (prob: Double, sym: UInt8)

let IM = 139968
let IA = 3877
let IC = 29573
var seed = 42

let n: Int
if Process.arguments.count > 1 {
    n = Int(Process.arguments[1]) ?? 1000
} else {
    n = 1000
}

let bufferSize = 256*1024
let width = 60
let lookupSize = 4096
let lookupScale: Double = Double(lookupSize - 1)

let aluString = "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG" +
                "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA" +
                "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT" +
                "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA" +
                "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG" +
                "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC" +
                "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"
var alu = [UInt8](aluString.nulTerminatedUTF8)
alu.popLast()

var iub = [
    AminoAcid(0.27, 97), // "a"),
    AminoAcid(0.12, 99), // "c"),
    AminoAcid(0.12, 103), // "g"),
    AminoAcid(0.27, 116), // "t"),
    AminoAcid(0.02, 66), // "B"),
    AminoAcid(0.02, 68), // "D"),
    AminoAcid(0.02, 72), // "H"),
    AminoAcid(0.02, 75), // "K"),
    AminoAcid(0.02, 77), // "M"),
    AminoAcid(0.02, 78), // "N"),
    AminoAcid(0.02, 82), // "R"),
    AminoAcid(0.02, 83), // "S"),
    AminoAcid(0.02, 86), // "V"),
    AminoAcid(0.02, 87), // "W"),
    AminoAcid(0.02, 89), // "Y"),
]

var homosapiens = [
    AminoAcid(0.3029549426680, 97), // "a"),
    AminoAcid(0.1979883004921, 99), // "c"),
    AminoAcid(0.1975473066391, 103), // "g"),
    AminoAcid(0.3015094502008, 116), // "t"),
]

func write(array array: [UInt8], ofLen len: Int) {
    let iov = UnsafeMutablePointer<iovec>.alloc(1)
    defer {
        iov.dealloc(1)
    }
    let data = array.withUnsafeBufferPointer({ p in p })
    iov.memory.iov_base = UnsafeMutablePointer<()>(data.baseAddress)
    iov.memory.iov_len = len
    writev(STDOUT_FILENO, iov, 1)
}

func write(buffer buffer: UnsafePointer<Int8>, ofLen len: Int) {
    let iov = UnsafeMutablePointer<iovec>.alloc(1)
    defer {
        iov.dealloc(1)
    }
    iov.memory.iov_base = UnsafeMutablePointer<()>(buffer)
    iov.memory.iov_len = len
    writev(STDOUT_FILENO, iov, 1)
}

func repeatFasta(gene: [UInt8], _ n: Int) {
    let gene2 = gene + gene
    var buffer = [UInt8](count: bufferSize, repeatedValue: 10)
    var pos = 0
    var rpos = 0
    var cnt = n
    var lwidth = width
    while cnt > 0 {
        if pos + lwidth > buffer.count {
            write(array: buffer, ofLen: pos)
            pos = 0
        }
        if rpos + lwidth > gene.count {
            rpos = rpos % gene.count
        }
        if cnt < lwidth {
            lwidth = cnt
        }
        buffer[pos..<pos+lwidth] = gene2[rpos..<rpos+lwidth]
        buffer[pos+lwidth] = 10
        pos += lwidth + 1
        rpos += lwidth
        cnt -= lwidth
    }
    if pos > 0 && pos < buffer.count {
        buffer[pos] = 10
        write(array: buffer, ofLen: pos)
    } else if pos == buffer.count {
        write(array: buffer, ofLen: pos)
        buffer[0] = 10
        write(array: buffer, ofLen: 1)
    }
}

func search(rnd: Double, within arr: [AminoAcid]) -> UInt8 {
    var low = 0
    var high = arr.count - 1
    while low <= high {
        let mid = low + (high - low) / 2
        if arr[mid].prob >= rnd {
            high = mid - 1
        } else {
            low = mid + 1
        }
    }
    return arr[high+1].sym
}

func accumulateProbabilities(inout acid: [AminoAcid]) {
    for i in 1..<acid.count {
        acid[i].prob += acid[i-1].prob
    }
}

func randomFasta(inout acid: [AminoAcid], _ n: Int) {
    var cnt = n
    accumulateProbabilities(&acid)
    var buffer = [UInt8](count: bufferSize, repeatedValue: 10)
    var pos = 0
    while cnt > 0 {
        var m = cnt
        if m > width {
            m = width
        }
        let f = 1.0 / Double(IM)
        var myrand = seed
        for _ in 0..<m {
            myrand = (myrand * IA + IC) % IM
            let r = Double(myrand) * f
            buffer[pos] = search(r, within: acid)
            pos += 1
            if pos == buffer.count {
                write(array: buffer, ofLen: pos)
                pos = 0
            }
        }
        seed = myrand
        buffer[pos] = 10
        pos += 1
        if pos == buffer.count {
            write(array: buffer, ofLen: pos)
            pos = 0
        }
        cnt -= m
    }
    if pos > 0 {
        write(array: buffer, ofLen: pos)
    }
}

let one = ">ONE Homo sapiens alu\n"
let oneStr = one.withCString({ s in s })
write(buffer: oneStr, ofLen: one.nulTerminatedUTF8.count - 1)
repeatFasta(alu, 2*n)

let two = ">TWO IUB ambiguity codes\n"
let twoStr = two.withCString({ s in s })
write(buffer: twoStr, ofLen: two.nulTerminatedUTF8.count - 1)
randomFasta(&iub, 3*n)

let three = ">THREE Homo sapiens frequency\n"
let threeStr = three.withCString({ s in s })
write(buffer: threeStr, ofLen: three.nulTerminatedUTF8.count - 1)
randomFasta(&homosapiens, 5*n)

