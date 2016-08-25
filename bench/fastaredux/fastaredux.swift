/* The Computer Language Benchmarks Game
 http://benchmarksgame.alioth.debian.org/
 contributed by Ralph Ganszky
 converted to Swift 3 by Sergo Beruashvili
 
 Swift port of Go #3 implementation
 */

import Glibc
//import Darwin

struct AminoAcid {
    var sym: UInt8
    var prob: Double
    var cprob: Double
    
    init() {
        sym = 0
        prob = 0
        cprob = 0
    }
    
    init(_ probability: Double, _ symbol: String) {
        prob = probability
        sym = UInt8(symbol.utf8CString[0])
        cprob = 0
    }
}

let IM = 139968
let IA = 3877
let IC = 29573
var seed = 42

let n: Int
if CommandLine.arguments.count > 1 {
    n = Int(CommandLine.arguments[1]) ?? 1000
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
var alu = aluString.utf8CString.map({ UInt8($0) })
alu.popLast()	// Remove trailing 0-byte

var iub = [
    AminoAcid(0.27, "a"),
    AminoAcid(0.12, "c"),
    AminoAcid(0.12, "g"),
    AminoAcid(0.27, "t"),
    AminoAcid(0.02, "B"),
    AminoAcid(0.02, "D"),
    AminoAcid(0.02, "H"),
    AminoAcid(0.02, "K"),
    AminoAcid(0.02, "M"),
    AminoAcid(0.02, "N"),
    AminoAcid(0.02, "R"),
    AminoAcid(0.02, "S"),
    AminoAcid(0.02, "V"),
    AminoAcid(0.02, "W"),
    AminoAcid(0.02, "Y"),
]

var homosapiens = [
    AminoAcid(0.3029549426680, "a"),
    AminoAcid(0.1979883004921, "c"),
    AminoAcid(0.1975473066391, "g"),
    AminoAcid(0.3015094502008, "t"),
]

func computeLookup( acid: inout [AminoAcid]) -> [Int] {
    var lookup = [Int](repeating: 0, count: lookupSize)
    var p = 0.0
    for (i, _) in acid.enumerated() {
        p += acid[i].prob
        acid[i].cprob = p * lookupScale
    }
    acid[acid.count-1].cprob = 1.0 * lookupScale
    
    var j = 0
    for (i, _) in lookup.enumerated() {
        while acid[j].cprob < Double(i) {
            j += 1
        }
        lookup[i] = j
    }
    return lookup
}

func writeRBuffer(buffer: UnsafePointer<UInt8>, len: Int) {
    writeBuffer(buffer: UnsafeMutablePointer<UInt8>(mutating: buffer), len: len)
}

func writeBuffer(buffer: UnsafeMutablePointer<UInt8>,len: Int) {
    let iov = UnsafeMutablePointer<iovec>.allocate(capacity: 1)
    defer {
        iov.deallocate(capacity: 1)
    }
    iov[0].iov_base = unsafeBitCast(buffer, to: UnsafeMutableRawPointer.self)
    iov[0].iov_len = len
    writev(STDOUT_FILENO, iov, 1)
}

func randomFasta( acid: inout [AminoAcid], n: Int) {
    var cnt = n
    let lookup = computeLookup(acid: &acid)
    let buffer = UnsafeMutablePointer<UInt8>.allocate(capacity: bufferSize)
    defer {
        buffer.deallocate(capacity: bufferSize)
    }
    var pos = 0
    while cnt > 0 {
        var m = cnt
        if m > width {
            m = width
        }
        let f = lookupScale / Double(IM)
        var myrand = seed
        for _ in 0..<m {
            myrand = (myrand * IA + IC) % IM
            let r = Double(myrand) * f
            var a = lookup[Int(r)]
            while acid[a].cprob < r {
                a += 1
            }
            buffer[pos] = acid[a].sym
            pos += 1
            if pos == bufferSize {
                writeBuffer(buffer: buffer, len: pos)
                pos = 0
            }
        }
        seed = myrand
        buffer[pos] = 10
        pos += 1
        if pos == bufferSize {
            writeBuffer(buffer: buffer, len: pos)
            pos = 0
        }
        cnt -= m
    }
    if pos > 0 {
        writeBuffer(buffer: buffer, len: pos)
    }
}

func repeatFasta(gene: [UInt8], n: Int) {
    let gene2 = gene + gene
    let buffer = UnsafeMutablePointer<UInt8>.allocate(capacity: bufferSize)
    defer {
        buffer.deallocate(capacity: bufferSize)
    }
    var pos = 0
    var rpos = 0
    var cnt = n
    var lwidth = width
    while cnt > 0 {
        if pos + lwidth > bufferSize {
            writeBuffer(buffer: buffer, len: pos)
            pos = 0
        }
        if rpos + lwidth > gene.count {
            rpos = rpos % gene.count
        }
        if cnt < lwidth {
            lwidth = cnt
        }
        for i in 0..<lwidth {
            buffer[pos+i] = gene2[rpos+i]
        }
        buffer[pos+lwidth] = 10
        pos += lwidth + 1
        rpos += lwidth
        cnt -= lwidth
    }
    if pos > 0 && pos < bufferSize {
        buffer[pos] = 10
        writeBuffer(buffer: buffer, len: pos)
    } else if pos == bufferSize {
        writeBuffer(buffer: buffer, len: pos)
        buffer[0] = 10
        writeBuffer(buffer: buffer, len: 1)
    }
}

let one = ">ONE Homo sapiens alu\n"

writeRBuffer(buffer: one.utf8CString.map({ UInt8($0) }), len: one.utf8CString.count - 1)
repeatFasta(gene: alu, n: 2*n)

let two = ">TWO IUB ambiguity codes\n"
writeRBuffer(buffer: two.utf8CString.map({ UInt8($0) }), len: two.utf8CString.count - 1)
randomFasta(acid: &iub, n: 3*n)

let three = ">THREE Homo sapiens frequency\n"
writeRBuffer(buffer: three.utf8CString.map({ UInt8($0) }), len: three.utf8CString.count - 1)
randomFasta(acid: &homosapiens, n: 5*n)

