"* The Computer Language Shootout
    http://shootout.alioth.debian.org/
    contributed by Isaac Gouy
    modified by Eliot Miranda 
    reworked for multicore by Isaac Gouy
*"!

Smalltalk.Core defineClass: #BenchmarksGame
	superclass: #{Core.Object}
	indexedType: #none
	private: false
	instanceVariableNames: ''
	classInstanceVariableNames: ''
	imports: ''
	category: ''!

Smalltalk defineClass: #TreeNode
	superclass: #{Core.Object}
	indexedType: #none
	private: false
	instanceVariableNames: 'left right '
	classInstanceVariableNames: ''
	imports: ''
	category: 'benchmarks game'!

!Core.BenchmarksGame class methodsFor: 'benchmarks game'!

program
   | checks depths iterations longLivedTree maxDepth minDepth
     n nprocs stretchDepth |

   n := CEnvironment commandLine last asNumber.
   minDepth := 4.
   maxDepth := minDepth + 2 max: n.
   stretchDepth := maxDepth + 1.

   checks := (TreeNode bottomUpTree: stretchDepth) itemCheck.
   Stdout
      nextPutAll: 'stretch tree of depth '; print: stretchDepth; tab;
      nextPutAll: ' check: '; print: checks; nl.

   longLivedTree := TreeNode bottomUpTree: maxDepth.

   depths := minDepth to: maxDepth by: 2.
   iterations := depths collect: [:each| 1 bitShift: maxDepth - each + minDepth].

      "for larger workloads split the work across multiple processes"
   nprocs := (ExternalProcess shOne: 'nproc') asNumber.   
   (nprocs > 1 and: [n > 16]) 
      ifTrue: [
         | workers |        
         workers := MatriX.VirtualMachines new: nprocs.
         [checks := workers do: self checkBlock with: depths with: iterations]
            ensure: [workers release].
      ]
      ifFalse: [
         checks := OrderedCollection new.
         depths keysDo: [:j| checks add: 
            (self checkBlock value: (depths at: j) value: (iterations at: j))].
      ].        

   checks keysDo: [:i|
      Stdout
         print: (iterations at: i); tab;
         nextPutAll: ' trees of depth '; print: (depths at: i); tab;
         nextPutAll: ' check: '; print: (checks at: i); nl
   ].

   Stdout
      nextPutAll: 'long lived tree of depth '; print: maxDepth; tab;
      nextPutAll: ' check: '; print: longLivedTree itemCheck; nl.
   ^''! 


checkBlock
   ^[:d :m| 
      | check |
      check := 0.
      1 to: m do: [:i| check := check + (TreeNode bottomUpTree: d) itemCheck].
      check
   ]! !


!TreeNode class methodsFor: 'instance creation'!

left: leftChild right: rightChild      
   ^(super new) left: leftChild right: rightChild!

bottomUpTree: anInteger
   ^(anInteger > 0) 
      ifTrue: [
         self 
            left: (self bottomUpTree: anInteger - 1) 
            right: (self bottomUpTree: anInteger - 1)  
      ]
      ifFalse: [
         self left: nil right: nil
      ]! !


!TreeNode methodsFor: 'benchmarks game'!

itemCheck
   ^left isNil 
      ifTrue: [1] ifFalse: [1 + left itemCheck + right itemCheck]! !

!TreeNode methodsFor: 'instance creation'!

left: leftChild right: rightChild
   left := leftChild.
   right := rightChild! !


!Core.Stream methodsFor: 'benchmarks game'!

nl
   self nextPut: Character lf! !

