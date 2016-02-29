/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/
   contributed by Ian Partridge

   Swift port of Java #3 implementation
*/

let LINE_LENGTH = 60
let OUT_BUFFER_SIZE = 256*1024
let LOOKUP_SIZE = 4*1024
let LOOKUP_SCALE = Double(LOOKUP_SIZE - 1)

struct Freq {
    var c: Character
    var p: Double
    init(_ c: Character, _ p: Double) {
        self.c = c
        self.p = p
    }
}

let ALU =
"GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTG" +
"GGAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGA" +
"GACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAA" +
"AATACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAAT" +
"CCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAAC" +
"CCGGGAGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTG" +
"CACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"

var IUB =
    [Freq("a", 0.27),
     Freq("c", 0.12),
     Freq("g", 0.12),
     Freq("t", 0.27),
    
     Freq("B", 0.02),
     Freq("D", 0.02),
     Freq("H", 0.02),
     Freq("K", 0.02),
     Freq("M", 0.02),
     Freq("N", 0.02),
     Freq("R", 0.02),
     Freq("S", 0.02),
     Freq("V", 0.02),
     Freq("W", 0.02),
     Freq("Y", 0.02)]

var HomoSapiens =
    [Freq("a", 0.3029549426680),
     Freq("c", 0.1979883004921),
     Freq("g", 0.1975473066391),
     Freq("t", 0.3015094502008)]

func sumAndScale(inout a: [Freq]) {
    var p = 0.0
    for i in 0..<a.count {
        p += a[i].p
        a[i].p = p * LOOKUP_SCALE
    }
    a[a.count - 1].p = LOOKUP_SCALE
}

class Random {
    static let IM = 139968
    static let IA = 3877
    static let IC = 29573
    static var SCALE: Double { get { return LOOKUP_SCALE / Double(IM)}}
    static var last = 42
    
    class func next() -> Double {
        last = (last * IA + IC) % IM
        return SCALE * Double(last)
    }
}

class Out {
    static var buf = String()
    static let lim = OUT_BUFFER_SIZE - (2 * LINE_LENGTH) - 1
    static var ct = 0
    
    class func checkFlush() -> Bool {
        if(ct >= lim) {
            print(buf, terminator: "")
            ct = 0
            buf = ""
            return true
        }
        return false
    }
    
    class func close() {
        print(buf, terminator: "")
        ct = 0
        buf = ""
    }
}

struct RandomFasta {
    var lookup = [Freq]()
    
    mutating func makeLookup(inout a: [Freq]) {
        lookup = []
        var j = 0
        for i in 0..<LOOKUP_SIZE {
            while (a[j].p < Double(i)) {
                j += 1
            }
            lookup.append(a[j])
        }
    }
    
    func addLine(bytes: Int) {
        Out.checkFlush()
        var lct = Out.ct
        while  (lct < Out.ct + bytes) {
            let r = Random.next()
            var ai = Int(r)
            while (lookup[ai].p < r) {
                ai += 1
            }
            Out.buf.append(lookup[ai].c)
            lct += 1
        }
        Out.buf.append(Character("\n"))
        lct += 1
        Out.ct = lct
    }
    
    mutating func make (desc: String, a: [Freq], n: Int) {
        var a = a
        var n = n
        makeLookup(&a)
        
        Out.buf.appendContentsOf(desc)
        Out.ct += desc.characters.count
        
        while (n > 0) {
            let bytes = min(LINE_LENGTH, n)
            addLine(bytes)
            n -= bytes
        }
    }
}

struct RepeatFasta {
    static func make(desc: String, alu: String, n: Int) {
        var n = n
        Out.buf.appendContentsOf(desc)
        Out.ct += desc.characters.count
        
        let len = alu.characters.count
        var buf = alu
        buf.appendContentsOf(alu[Range<String.CharacterView.Index>(start: alu.startIndex, end: alu.startIndex.advancedBy(LINE_LENGTH))])
        
        var pos = 0
        while (n > 0) {
            let bytes = min(LINE_LENGTH, n)
            Out.checkFlush()
            let startIndex = buf.startIndex.advancedBy(pos)
            let endIndex = startIndex.advancedBy(bytes)
            let towrite = buf[Range<String.CharacterView.Index>(start: startIndex, end: endIndex)]
            Out.buf.appendContentsOf(towrite)
            Out.buf.append(Character("\n"))
            Out.ct += bytes + 1
            pos = (pos + bytes) % len
            n -= bytes
        }
    }
}

var n = 25000000
if Process.argc > 1 {
    n = Int(Process.arguments[1])!
}
sumAndScale(&IUB)
sumAndScale(&HomoSapiens)

RepeatFasta.make(">ONE Homo sapiens alu\n", alu: ALU, n: n * 2)
var randomFasta = RandomFasta()
randomFasta.make(">TWO IUB ambiguity codes\n", a: IUB, n: n * 3)
randomFasta.make(">THREE Homo sapiens frequency\n", a: HomoSapiens, n: n * 5)
Out.close()
