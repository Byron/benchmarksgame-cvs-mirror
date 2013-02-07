"* The Computer Language Benchmarks Game
    http://benchmarksgame.alioth.debian.org/
    contributed by Eliot Miranda *"!


!Tests class methodsFor: 'benchmarking'!
matchPatterns
   ^#(   'agggtaaa|tttaccct'
         '[cgt]gggtaaa|tttaccc[acg]'
         'a[act]ggtaaa|tttacc[agt]t'
         'ag[act]gtaaa|tttac[agt]ct'
         'agg[act]taaa|ttta[agt]cct'
         'aggg[acg]aaa|ttt[cgt]ccct'
         'agggt[cgt]aa|tt[acg]accct'
         'agggta[cgt]a|t[acg]taccct'
         'agggtaa[cgt]|[acg]ttaccct'
   )! !

!Tests class methodsFor: 'benchmarking'!
substitutionPatterns
   ^#(   #('B' '(c|g|t)')
         #('D' '(a|g|t)')
         #('H' '(a|c|t)')
         #('K' '(g|t)')
         #('M' '(a|c)')
         #('N' '(a|c|g|t)')
         #('R' '(a|g)')
         #('S' '(c|g)')
         #('V' '(a|c|g)')
         #('W' '(a|t)')
         #('Y' '(c|t)'))! !


!Tests class methodsFor: 'benchmarking'!
regexDNA: sequence to: output
   | s size1 size2 translation |
   size1 := sequence size.

   "* remove FASTA sequence descriptions and new-lines *"
   s := sequence copyWithRegex: '>[^\r]*\r|\r' matchesReplacedWith: ''.
   size2 := s size.

   "* regex match *"
   self matchPatterns do: [:each| 
      output 
         nextPutAll: each; space; 
         print: (s occurrencesOfRegex: each); nl
      ]. 

   "* regex substitution *"
   translation := Dictionary new.
   self substitutionPatterns do: [:each| 
      translation at: each first put: each last].

   s := s copyWithRegex: '[', 
         (translation keys asArray fold: [:a :b| a, b]), ']'
      matchesTranslatedUsing: [:l| translation at: l].

   output
      nl;
      print: size1; nl; 
      print: size2; nl; 
      print: s size; nl! !


!Tests class methodsFor: 'benchmark scripts'!
regexdna
   self regexDNA: self stdinSpecial contents to: self stdout.
   ^'' ! !
