"* The Computer Language Benchmarks Game
    http://benchmarksgame.alioth.debian.org/
    contributed by Paolo Bonzini 
    modified by Andres Valloud *"!

Smalltalk.Core defineClass: #BenchmarksGame
	superclass: #{Core.Object}
	indexedType: #none
	private: false
	instanceVariableNames: ''
	classInstanceVariableNames: ''
	imports: ''
	category: ''!

Smalltalk defineClass: #PiDigitSpigot
	superclass: #{Core.Stream}
	indexedType: #none
	private: false
	instanceVariableNames: 'numer accum denom k '
	classInstanceVariableNames: ''
	imports: ''
	category: 'Shootout'!

!Core.BenchmarksGame class methodsFor: 'private'!

pidigitsTo: v width: width to: output
   | n i pidigits |
   n := v.
   i := 0.
   pidigits := PiDigitSpigot new.
   [n > 0] whileTrue:
      [n < width
         ifTrue:
            [n timesRepeat: [output nextPut: (Character digitValue: pidigits next)].
            n to: width do: [:each | output space].
            i := i + n]
         ifFalse:
            [width timesRepeat: [output nextPut: (Character digitValue: pidigits next)].
            i := i + width].

      output tab; nextPut: $:; print: i; nl.

      n := n - width]! !

!Core.BenchmarksGame class methodsFor: 'initialize-release'!

program
   | n |
   n := CEnvironment commandLine last asNumber.
   self pidigitsTo: n width: 10 to: Stdout.
   ^''! !


!PiDigitSpigot class methodsFor: 'instance creation'!

new
   ^super basicNew initialize! !


!PiDigitSpigot methodsFor: 'private'!

initialize
    numer := denom := 1.
    k := accum := 0.!

extract
    | tmp |
    numer > accum ifTrue: [^nil].
    tmp := numer + numer + numer + accum.
    tmp \\ denom >= (denom - numer) ifTrue: [^nil].
    ^tmp // denom!

eliminate: digit
    accum := accum - (denom * digit).
    accum := accum * 10.
    numer := numer * 10!

step
    | y2 |
    k := k + 1.
    y2 := k * 2 + 1.
    accum := (numer + numer + accum) * y2.
    numer := numer * k.
    denom := denom * y2.! !

!PiDigitSpigot methodsFor: 'stream'!

atEnd
    ^false!

next
    | digit |
    [ self step. (digit := self extract) isNil ] whileTrue.
    self eliminate: digit.
    ^digit! !

!Core.Stream methodsFor: 'benchmarks game'!

nl
   self nextPut: Character lf! !
