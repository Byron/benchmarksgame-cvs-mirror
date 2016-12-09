// The Computer Language Benchmarks Game
// http://benchmarksgame.alioth.debian.org/
//
// contributed by Daniel Muellenborn
// parallelized using Dispatch by Dave Grove

import Foundation
import Dispatch

#if os(macOS)
typealias RegExT = RegularExpression
#else
typealias RegExT = NSRegularExpression
#endif

let regex: (String) -> RegExT = { pattern in
    return try! RegExT(pattern: pattern, options: [])
}

// Read data from stdin
let inputData = FileHandle.readDataToEndOfFile(FileHandle.standardInput)()
let inputLength = inputData.count
var sequence = String(data: inputData, encoding: .utf8)!

// Remove sequence descriptions and linefeeds
sequence = regex(">[^\n]*\n|\n").stringByReplacingMatches(in: sequence, options: [], range: NSRange(0..<inputLength), withTemplate: "")
let cleanedInput = sequence.utf8
let codeLength = cleanedInput.count

// Count matches
let variants = [
   "agggtaaa|tttaccct",
   "[cgt]gggtaaa|tttaccc[acg]",
   "a[act]ggtaaa|tttacc[agt]t",
   "ag[act]gtaaa|tttac[agt]ct",
   "agg[act]taaa|ttta[agt]cct",
   "aggg[acg]aaa|ttt[cgt]ccct",
   "agggt[cgt]aa|tt[acg]accct",
   "agggta[cgt]a|t[acg]taccct",
   "agggtaa[cgt]|[acg]ttaccct",
   ]
let counts = UnsafeMutablePointer<Int>.allocate(capacity: variants.count)

let range = NSRange(0..<codeLength)
let dg = DispatchGroup()
for i in 0..<variants.count {
    DispatchQueue.global().async(group: dg) {
        counts[i] = regex(variants[i]).numberOfMatches(in: sequence, options: [], range: range)
    }
}
dg.wait()

// output regex and counts
for i in 0..<variants.count {
   print(variants[i], "\(counts[i])")
}

// make IUB alternatives explicit
let replacements = [
   (regex("B"), "(c|g|t)"),
   (regex("D"), "(a|g|t)"),
   (regex("H"), "(a|c|t)"),
   (regex("K"), "(g|t)"),
   (regex("M"), "(a|c)"),
   (regex("N"), "(a|c|g|t)"),
   (regex("R"), "(a|g)"),
   (regex("S"), "(c|g)"),
   (regex("V"), "(a|c|g)"),
   (regex("W"), "(a|t)"),
   (regex("Y"), "(c|t)"),
	]

for (re, replacement) in replacements {
    sequence = re.stringByReplacingMatches(in: sequence, options: [], range: range, withTemplate: replacement)
}


let resultLength = sequence.utf8.count

print("", inputLength, codeLength, resultLength, separator: "\n")


