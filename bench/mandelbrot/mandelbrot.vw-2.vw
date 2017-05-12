"* The Computer Language Benchmarks Game
    http://benchmarksgame.alioth.debian.org/
    contributed by Paolo Bonzini 
    reworked for multicore by Isaac Gouy *"!


Smalltalk.Core defineClass: #BenchmarksGame
    superclass: #{Core.Object}
    indexedType: #none
    private: false
    instanceVariableNames: ''
    classInstanceVariableNames: ''
    imports: ''
    category: ''!

!Core.BenchmarksGame class methodsFor: 'benchmarks game'!

program
   | n |
   n := CEnvironment commandLine last asNumber.
   Stdout
      nextPutAll: 'P4'; nl; 
      print: n; space; print: n; nl;
      binary.

   self splitCombinePutMandelbrot: n on: Stdout.
   ^''!

splitCombinePutMandelbrot: anInteger on: aStream
   | chunks chunkSize extent first last nprocs workers |
   nprocs := (ExternalProcess shOne: 'nproc') asNumber.
      "Somethings broken about this program at 16000 with 4 workers"
   workers := MatriX.VirtualMachines new: 3.
   [   
      chunkSize := anInteger // nprocs + 1.
      first := (0 to: (nprocs - 1)) collect: [:each| (each * chunkSize + 1) - 1].
      last := first collect: [:each| (each + chunkSize - 1) min: (anInteger - 1)].
      extent := first collect: [:each| anInteger].

      chunks := workers 
         do: [ :i :j :n | BenchmarksGame mandelbrotRowsFrom: i to: j for: n]
         with: first 
         with: last
         with: extent.

      chunks do: [:each| aStream nextPutAll: each].

   ] ensure: [workers release].!

mandelbrotRowsFrom: first to: last for: extent
   | s |
   s := ReadWriteStream on: (ByteArray new: 8192).
   self putMandelbrotRowsFrom: first to: last for: extent on: s.
   ^s contents!

putMandelbrotRowsFrom: first to: last for: extent on: aStream
   | bits ci cr i limit2 m stepi stepr tr zi zr |
   limit2 := 4.0d0.
   m := 50.

   stepr := 2.0d0 / extent.
   stepi := 2.0d0 / extent.

   first to: last do: [ :y |
      bits := 0.
      ci := stepi * y asFloat - 1.0d0.
      0 to: extent - 1 do: [ :x |
         cr := stepr * x asFloat - 1.5d0.
         zr := cr. zi := ci.

         bits := bits bitShift: 1.
         i := 1.  
         [
            tr := (zr*zr) - (zi*zi) + cr.
            zi := 2.0d0 * zr * zi + ci.
            zr := tr.
            (zr*zr) + (zi*zi) < limit2 and: [ (i := i + 1) < m ]
         ] whileTrue.

         i = m ifTrue: [ bits := bits + 1 ].
         (x bitAnd: 7) == 7 ifTrue: [
            aStream nextPut: bits.
            bits := 0.
         ]
      ]. 
      (extent bitAnd: 7) == 0 ifFalse: [
         bits := bits bitShift: 8 - (extent bitAnd: 7).
         aStream nextPut: bits.
      ]
   ]! !

!Core.Stream methodsFor: 'benchmarks game'!

nl
   self nextPut: Character lf! !
