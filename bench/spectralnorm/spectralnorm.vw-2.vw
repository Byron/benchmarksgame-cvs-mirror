"* The Computer Language Benchmarks Game
    http://benchmarksgame.alioth.debian.org/
    contributed by Paolo Bonzini 
    reworked for multicore by Isaac Gouy *"!

Smalltalk.Core defineClass: #BenchmarksGame
    superclass: #{Core.Object}
    indexedType: #none
    private: false
    instanceVariableNames: 'n workers first last '
    classInstanceVariableNames: ''
    imports: ''
    category: ''!


!Core.BenchmarksGame class methodsFor: 'initialize-release'!

program
   | n |
   n := CEnvironment commandLine last asNumber.
   Stdout print: (self spectralnorm: n) digits: 9; nl.
   ^''! !

!Core.BenchmarksGame class methodsFor: 'instance creation'!

spectralnorm: anInteger
   ^super new spectralnorm: anInteger! !

!Core.BenchmarksGame methodsFor: 'initialize-release'!

spectralnorm: anInteger
   | nprocs chunkSize |
   n := anInteger.
   nprocs := (ExternalProcess shOne: 'nproc') asNumber.
   workers := MatriX.VirtualMachines new: nprocs.
   [   
      chunkSize := anInteger // nprocs + 1.
      first := (0 to: (nprocs - 1)) collect: [:each| each * chunkSize + 1].
      last := first collect: [:each| (each + chunkSize - 1) min: n].
      ^self spectralnorm.

   ] ensure: [workers release].! !

!Core.BenchmarksGame methodsFor: 'private'!

map: aBlock with: anArray
   ^workers 
      do: aBlock
      with: (first collect: [:each| anArray]) 
      with: first 
      with: last.!

multiplyAtAv: anArray
   ^self reduce: 
      (self 
         map: [:w :i :j | w multiplyAtvFrom: i to: j ] 
         with: 
            (self reduce: 
               (self 
                   map: [:w :i :j | w multiplyAvFrom: i to: j ] 
                   with: anArray
               )
            )
      )!

reduce: aCollection
   | a |
   a := Array new: n.
   aCollection keysDo: [:index|
      a replaceElementsFrom: (first at: index) 
         to: (last at: index) 
         withSequenceableCollection: (aCollection at: index) 
         startingAt: (first at: index)
   ].
   ^a!

spectralnorm
   | u v vBv vv |
   u := Array new: n withAll: 1.0d0.
   10 timesRepeat: [
      v := self multiplyAtAv: u.
      u := self multiplyAtAv: v.
   ].
   vBv := 0.0d0.
   vv := 0.0d0.
   1 to: n do:
      [:i |
       vBv := vBv + ((u at: i) * (v at: i)).
       vv := vv + ((v at: i) * (v at: i))].
   ^(vBv / vv) sqrt! !


!Core.SmallInteger methodsFor: 'benchmarks game'!

matrixA: anInteger
   ^1.0d0 / ((self + anInteger - 2) * (self + anInteger - 1) /2  + self)! !


!Core.Array methodsFor: 'benchmarks game'!

multiplyAvFrom: first to: last
   | n av sum |
   n := self size.
   av := Array new: n.
   first to: last do: [:i|
      sum := 0.0d0.
      1 to: n do: [:j|
         sum := sum + ((i matrixA: j) * (self at: j)) ].
      av at: i put: sum].
   ^av!

multiplyAtvFrom: first to: last
   | n atv sum |
   n := self size.
   atv := Array new: n.
   first to: last do: [:i|
      sum := 0.0d0.
      1 to: n do: [:j|
         sum := sum + ((j matrixA: i) * (self at: j)) ].
      atv at: i put: sum].
   ^atv! !

!Core.Stream methodsFor: 'benchmarks game'!

nl
   self nextPut: Character lf!

print: number digits: decimalPlaces
   self nextPutAll: 
      ((number asFixedPoint: decimalPlaces) printString copyWithout: $s)! !

