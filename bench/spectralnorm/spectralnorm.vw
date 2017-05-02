"* The Computer Language Benchmarks Game
    http://benchmarksgame.alioth.debian.org/
    contributed by Paolo Bonzini 
    reworked by Isaac Gouy *"!

Smalltalk.Core defineClass: #BenchmarksGame
    superclass: #{Core.Object}
    indexedType: #none
    private: false
    instanceVariableNames: ''
    classInstanceVariableNames: ''
    imports: ''
    category: ''!


!Core.BenchmarksGame class methodsFor: 'private'!

spectralnorm: n
   | u v vBv vv |
   u := Array new: n withAll: 1.0d0.
   10 timesRepeat:
      [v := u multiplyAtAv.
       u := v multiplyAtAv].
   vBv := 0.0d0.
   vv := 0.0d0.
   1 to: n do:
      [:i |
       vBv := vBv + ((u at: i) * (v at: i)).
       vv := vv + ((v at: i) * (v at: i))].
   ^(vBv / vv) sqrt! !

!Core.BenchmarksGame class methodsFor: 'initialize-release'!

program
   | n |
   n := CEnvironment commandLine last asNumber.

   Stdout
      print: (self spectralnorm: n) digits: 9;
      nl.

   ^''! !


!Core.SmallInteger methodsFor: 'benchmarks game'!

matrixA: anInteger
   ^1.0d0 / ((self + anInteger - 2) * (self + anInteger - 1) /2  + self)! !


!Core.Array methodsFor: 'benchmarks game'!

multiplyAtv
   | n atv sum |
   n := self size.
   atv := Array new: n.
   1 to: n do: [:i|
      sum := 0.0d0.
      1 to: n do: [:j|
         sum := sum + ((j matrixA: i) * (self at: j)) ].
      atv at: i put: sum].
   ^atv!

multiplyAtAv
   ^(self multiplyAv) multiplyAtv!

multiplyAv
   | n av sum |
   n := self size.
   av := Array new: n.
   1 to: n do: [:i|
      sum := 0.0d0.
      1 to: n do: [:j|
         sum := sum + ((i matrixA: j) * (self at: j)) ].
      av at: i put: sum].
   ^av! !


!Core.Stream methodsFor: 'benchmarks game'!

nl
   self nextPut: Character lf!

print: number digits: decimalPlaces
   self nextPutAll: 
      ((number asFixedPoint: decimalPlaces) printString copyWithout: $s)! !

