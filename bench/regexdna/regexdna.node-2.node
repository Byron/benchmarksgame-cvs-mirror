// The Computer Language Benchmarks Game
// http://benchmarksgame.alioth.debian.org/
//
// contributed by Jesse Millikan
// Base on the Ruby version by jose fco. gonzalez
// fixed by Matthew Wilson 
// ported to Node.js and sped up by Roman Pletnev

var fs = require('fs'), i = fs.readFileSync('/dev/stdin', 'ascii'),
  ilen = i.length, clen, j,
  q = [/agggtaaa|tttaccct/ig, /[cgt]gggtaaa|tttaccc[acg]/ig,
    /a[act]ggtaaa|tttacc[agt]t/ig, /ag[act]gtaaa|tttac[agt]ct/ig,
    /agg[act]taaa|ttta[agt]cct/ig, /aggg[acg]aaa|ttt[cgt]ccct/ig,
    /agggt[cgt]aa|tt[acg]accct/ig, /agggta[cgt]a|t[acg]taccct/ig,
    /agggtaa[cgt]|[acg]ttaccct/ig],
  b = ["(c|t)", /Y/g, "(a|t)", /W/g, "(a|c|g)", /V/g, "(c|g)", /S/g,
    "(a|g)", /R/g, "(a|c|g|t)", /N/g, "(a|c)", /M/g, "(g|t)", /K/g,
    "(a|c|t)", /H/g, "(a|g|t)", /D/g, "(c|g|t)", /B/g];

i = i.replace(/^>.*\n|\n/mg, '');
clen = i.length;
for(j = 0; j<q.length; ++j) {
  var qj = q[j], m = i.match(qj);
  console.log(qj.source, m ? m.length : 0);
}
while(b.length) i = i.replace(b.pop(), b.pop());
console.log(["", ilen, clen, i.length].join("\n"));
