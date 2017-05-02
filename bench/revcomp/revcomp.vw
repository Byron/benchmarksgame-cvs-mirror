"* The Computer Language Benchmarks Game
    http://benchmarksgame.alioth.debian.org/
   contributed by Eliot Miranda and Isaac Gouy *"!


Smalltalk.Core defineClass: #BenchmarksGame
	superclass: #{Core.Object}
	indexedType: #none
	private: false
	instanceVariableNames: ''
	classInstanceVariableNames: ''
	imports: ''
	category: ''!


!Core.BenchmarksGame class methodsFor: 'private'!

readFasta: sequenceName from: input
   | prefix newline buffer description line char |
   prefix := '>',sequenceName.
   newline := Character cr.

   "* find start of particular fasta sequence *"
   [(input atEnd) or: [
         (input peek = $>) 
            ifTrue: [((line := input upTo: newline) 
               indexOfSubCollection: prefix startingAt: 1) = 1]
            ifFalse: [input skipThrough: newline. false]]
      ] whileFalse.

   "* line-by-line read - it would be a lot faster to block read *"
   description := line.
   buffer := ReadWriteStream on: (String new: 1028).
   [(input atEnd) or: [(char := input peek) = $>]] whileFalse: [
      (char = $;) 
         ifTrue: [input upTo: newline] 
         ifFalse: [buffer nextPutAll: (input upTo: newline)]
      ].
   ^Association key: description value: buffer contents!

reverseComplement: sequence named: sequenceName to: output
   | complement newline lineLength n |
   (sequenceName isNil) ifTrue: [^self].

   complement := String new: 128 withAll: $*.

   'ABCDGHKMNRSTVWY' with: 
   'TVGHCDMKNYSABWR'
      do: [:a :b|
         complement at: a asInteger put: b.
         complement at: a asLowercase asInteger put: b].

   newline := Character lf.
   lineLength := 60.
   n := sequence size.

   output nextPutAll: sequenceName; nextPut: newline.

   [n > 0] whileTrue: [ 
         1 to: ((n < lineLength) ifTrue: [n] ifFalse: [lineLength]) do:
            [:i | output nextPut: 
               (complement at: (sequence at: n - i + 1) asInteger)].
         output nextPut: newline.
         n := n - lineLength. 
      ]! !

!Core.BenchmarksGame class methodsFor: 'initialize-release'!

program
   | input output |
   input := ExternalReadStream on:
      (ExternalConnection ioAccessor: (UnixDiskFileAccessor new handle: 0)).
   output := ExternalWriteStream on:
      (ExternalConnection ioAccessor: (UnixDiskFileAccessor new handle: 1)).

   #('ONE' 'TWO' 'THREE') do:
      [:sequenceName|   | fasta |
         fasta := self readFasta: sequenceName from: input.
         self reverseComplement: fasta value named: fasta key to: output.
      ].

   output flush. 
   ^''! !
