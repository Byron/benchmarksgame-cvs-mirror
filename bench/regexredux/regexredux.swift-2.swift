// The Computer Language Benchmarks Game
// http://benchmarksgame.alioth.debian.org/
//
// regex-dna program contributed by Daniel Muellenborn
// NSRegularExpression fix by Dave Grove
// converted from regex-dna program

import Foundation

func stdin() -> String {
   let stream = fopen("/dev/stdin", "r")
   var input = ""
   while true {
      let c = fgetc(stream)
      if c == -1 { break }
      input.append(Character(UnicodeScalar(UInt32(c))!))
   }
   return input
}

var sequence = stdin()

let inputLength = sequence.utf8.count

let regex: (String) -> NSRegularExpression = { pattern in
   return try! NSRegularExpression(pattern: pattern, options: [])
}

var range = NSRange(0..<inputLength)

sequence = regex(">[^\n]*\n|\n").stringByReplacingMatches(in: sequence, options: [], range: range, withTemplate: "")

let codeLength = sequence.utf8.count
range.length = codeLength

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

var counts = Array(repeating: ("",0), count: variants.count)

for n in 0..<variants.count { 
   counts[n] = (variants[n], regex(variants[n]).numberOfMatches(in: sequence, options: [], range: range))
}

for (variant, count) in counts {
   print(variant, "\(count)")
}

let replacements = [
   (regex("tHa[Nt]"), "<4>"),
   (regex("aND|caN|Ha[DS]|WaS"), "<3>"),
   (regex("a[NSt]|BY"), "<2>"),
   (regex("<[^>]*>"), "|"),
   (regex("[^|][^|]*"), ""),
	]

for (re, replacement) in replacements {
   sequence = re.stringByReplacingMatches(in: sequence, options: [], range: range, withTemplate: replacement)
}

let resultLength = sequence.utf8.count

print("", inputLength, codeLength, resultLength, separator: "\n")

