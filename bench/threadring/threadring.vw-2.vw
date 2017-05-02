"* The Computer Language Benchmarks Game
    http://benchmarksgame.alioth.debian.org/
    adapted from a program by Paolo Bonzini
    contributed by Isaac Gouy 
    modified by Carlo Teixeira *"!

Smalltalk defineClass: #Thread
	superclass: #{Core.Object}
	indexedType: #none
	private: false
	instanceVariableNames: 'name nextThread token semaphore done '
	classInstanceVariableNames: ''
	imports: ''
	category: 'BenchmarksGame'!

Smalltalk.Core defineClass: #BenchmarksGame
	superclass: #{Core.Object}
	indexedType: #none
	private: false
	instanceVariableNames: ''
	classInstanceVariableNames: ''
	imports: ''
	category: ''!

!Core.BenchmarksGame class methodsFor: 'private'!

threadRing: aSemaphore
   | first last |
   503 to: 1 by: -1 do: [:i|
      first := Thread named: i next: first done: aSemaphore.
      last isNil ifTrue: [ last:=first. ].
   ].
   last nextThread: first.
   ^first! !

!Core.BenchmarksGame class methodsFor: 'initialize-release'!

program
   | n done |
   n := CEnvironment commandLine last asNumber.
   (self threadRing: (done := Semaphore new)) takeToken: n.
   done wait.
   ^''! !

!Thread class methodsFor: 'instance creation'!

named: anInteger next: aThread done: aSemaphore
   ^self new name: anInteger; nextThread: aThread; done: aSemaphore; fork!

new
   ^self basicNew semaphore: Semaphore new! !


!Thread methodsFor: 'accessing'!

run
   [semaphore wait.
   0==token] whileFalse: [nextThread takeToken: token - 1].
   name printOn: Stdout.
   Stdout cr.
   done signal!

fork
   [ self run ] forkAt: Processor userBackgroundPriority.!

semaphore: aSemaphore
   semaphore := aSemaphore!

done: aSemaphore
   done := aSemaphore!

name: anInteger
   name := anInteger!

nextThread: aThread
   nextThread := aThread!

takeToken: x
   token := x.
   semaphore signal! !
