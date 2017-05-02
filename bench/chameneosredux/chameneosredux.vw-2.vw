"* The Computer Language Benchmarks Game
    http://benchmarksgame.alioth.debian.org/
    contributed by Carlo Teixeira *"!

Smalltalk.Core defineClass: #BenchmarksGame
	superclass: #{Core.Object}
	indexedType: #none
	private: false
	instanceVariableNames: ''
	classInstanceVariableNames: ''
	imports: ''
	category: ''!

Smalltalk defineClass: #Pair
	superclass: #{Core.Object}
	indexedType: #none
	private: false
	instanceVariableNames: 'partner me sema '
	classInstanceVariableNames: ''
	imports: ''
	category: '(none)'!

Smalltalk defineClass: #Mall
	superclass: #{Core.Object}
	indexedType: #none
	private: false
	instanceVariableNames: 'guard maxRendezvous open process queue cache pairCache '
	classInstanceVariableNames: 'Units'
	imports: ''
	category: 'chameleon'!

Smalltalk defineClass: #Creature
	superclass: #{Core.Object}
	indexedType: #none
	private: false
	instanceVariableNames: 'creatureName colour selfMet creaturesMet '
	classInstanceVariableNames: ''
	imports: ''
	category: 'chameleon'!

Smalltalk defineClass: #ChameneosColour
	superclass: #{Core.Object}
	indexedType: #none
	private: false
	instanceVariableNames: 'color '
	classInstanceVariableNames: 'Blue Red Yellow'
	imports: ''
	category: 'chameleon'!


!Mall class methodsFor: 'initialize-release'!

createCreaturesWith: aCollectionOfColours 
   "Private"

   | aName |
   aName := 0.
   ^aCollectionOfColours collect: 
         [:aColour | 
         aName := aName + 1.
         Creature withName: aName colour: aColour]!

new
   ^self shouldNotImplement!

initialize
   "self initialize"

   Units := #('zero' 'one' 'two' 'three' 'four' 'five' 'six' 'seven' 'eight' 'nine')!

createAllowing: maxRendezvous 
   "Private"

   ^self basicNew initialize maxRendezvous: maxRendezvous!

openMallWith: aCollectionOfColours forNumberOfMeets: aNumber 
   | mall creatures guard |
   mall := self createAllowing: aNumber.
   mall run.
   creatures := self createCreaturesWith: aCollectionOfColours.
   guard := Semaphore new.
   self 
      openMall: mall
      forCreatures: creatures
      usingGuard: guard.
   self 
      waitForClosingOfMall: mall
      withCreatures: creatures
      usingGuard: guard.
   ^creatures! !

!Mall class methodsFor: 'public'!

runBenchMark: number on: anOutputStream 
   "self runBenchMark: 60000 on: Transcript."

   | firstTestColours secondTestColours blue red yellow creatures |
   blue := ChameneosColour blue.
   red := ChameneosColour red.
   yellow := ChameneosColour yellow.
   firstTestColours := Array 
            with: blue
            with: red
            with: yellow.
   secondTestColours := (OrderedCollection new)
            add: blue;
            add: red;
            add: yellow;
            add: red;
            add: yellow;
            add: blue;
            add: red;
            add: yellow;
            add: red;
            add: blue;
            yourself.
   (ChameneosColour generateReportOfColoursOn: anOutputStream) nl.
   (self generateReportForColours: firstTestColours printOn: anOutputStream) 
      nl.
   creatures := Mall openMallWith: firstTestColours forNumberOfMeets: number.
   (self generateReportFor: creatures printOn: anOutputStream)
      nl;
      nl.
   (self generateReportForColours: secondTestColours printOn: anOutputStream) 
      nl.
   creatures := Mall openMallWith: secondTestColours forNumberOfMeets: number.
   (self generateReportFor: creatures printOn: anOutputStream)
      nl;
      nl! !

!Mall class methodsFor: 'printing'!

generateReportForColours: colours printOn: stream 
   stream space.
   colours do: [:colour | colour printOn: stream] separatedBy: [stream space].
   ^stream!

generateReportFor: creatures printOn: stream 
   | sum |
   sum := creatures inject: 0 into: [:accum :each | accum + each creaturesMet].
   creatures do: 
         [:aCreature | 
         aCreature creaturesMet printOn: stream.
         stream
            space;
            nextPutAll: (self units at: aCreature selfMet + 1);
            nl].
   stream space.
   sum printString 
      do: [:el | stream nextPutAll: (self units at: el digitValue + 1)]
      separatedBy: [stream space].
   ^stream! !

!Mall class methodsFor: 'accessing'!

units
   ^Units! !

!Mall class methodsFor: 'private'!

openMall: aMall forCreatures: creatures usingGuard: sema 
   | processes |
   processes := creatures 
            collect: [:aCreature | 
               [aCreature visitMall: aMall.
               sema signal] newProcess].
   processes do: 
         [:proc | 
         proc priority: Processor userBackgroundPriority.
         proc resume]!

waitForClosingOfMall: aMall withCreatures: creatures usingGuard: guard 
   creatures size timesRepeat: [guard wait].
   aMall close! !


!Mall methodsFor: 'private'!

releasePair: pair 
   pair release.
   cache addFirst: pair!

setPartnersOn: first and: second
   first partner: second me.
   second partner: first me.!

shutdown
   [queue isEmpty] whileFalse: [queue next signal].
   process terminate.
   process := nil!

obtainPair
   ^cache removeFirst!

processVisitors
   [open] whileTrue: 
         [1 to: maxRendezvous
            do: 
               [:x | 
               | first second |
               first := queue next.
               second := queue next.
               self setPartnersOn: first and: second.
               first signal.
               second signal].
         [queue isEmpty] whileFalse: [queue next signal]].
   process terminate.
   process := nil! !

!Mall methodsFor: 'accessing'!

maxRendezvous: max 
   maxRendezvous := max! !

!Mall methodsFor: 'controlling'!

close
   open := false!

visitWith: aChameneos 
   | pair partner |
   pair := self obtainPair.
   pair me: aChameneos.
   queue nextPut: pair.
   pair wait.
   partner := pair partner.
   self releasePair: pair.
   ^partner! !

!Mall methodsFor: 'initialize-release'!

initialize
   guard := Semaphore forMutualExclusion.
   queue := SharedQueue new.
   cache := OrderedCollection new.
   1 to: 10 do: [:x | cache add: Pair new]!

run
   open := true.
   process ifNil: 
         [process := [self processVisitors] newProcess.
         process priority: Processor userBackgroundPriority -1 ].
   process resume! !


!Creature class methodsFor: 'initialize-release'!

withName: aName colour: aColour 
   ^(Creature new initialize)
      name: aName;
      colour: aColour! !


!Creature methodsFor: 'accessing'!

colour: anObject 
   colour := anObject!

name: anObject 
   creatureName := anObject!

selfMet: anObject 
   ^selfMet := anObject!

name
   ^creatureName!

creaturesMet
   ^creaturesMet!

colour
   ^colour!

creaturesMet: anObject 
   creaturesMet := anObject!

selfMet
   ^selfMet! !

!Creature methodsFor: 'controlling'!

visitMall: mall 
   
   [| partner |
   partner := mall visitWith: self.
   partner ifNotNil: 
         [colour := colour complementaryColourFor: partner colour.
         self == partner ifTrue: [selfMet := selfMet + 1].
         creaturesMet := creaturesMet + 1].
   partner isNil] 
         whileFalse! !

!Creature methodsFor: 'initialize-release'!

initialize
   selfMet := 0.
   creaturesMet := 0! !


!ChameneosColour class methodsFor: 'accessing'!

blue: anObject
   Blue := anObject!

red: anObject
   Red := anObject!

yellow
   ^Yellow!

yellow: anObject
   Yellow := anObject!

blue
   ^Blue!

red
   ^Red! !

!ChameneosColour class methodsFor: 'printing'!

generateReportOfColoursOn: readOut 
   | colours |
   colours := Array 
            with: Blue
            with: Red
            with: Yellow.
   colours do: 
         [:aColour | 
         colours do: 
               [:anotherColour | 
               aColour printOn: readOut.
               readOut nextPutAll: ' + '.
               anotherColour printOn: readOut.
               readOut nextPutAll: ' -> '.
               (aColour complementaryColourFor: anotherColour) printOn: readOut.
               readOut nl]].
   ^readOut! !

!ChameneosColour class methodsFor: 'initialize-release'!

createYellow
   ^super new color: #yellow!

createBlue
   ^super new color: #blue!

createRed
   ^super new color: #red!

initialize
   Red := self createRed.
   Blue := self createBlue.
   Yellow := self createYellow! !


!ChameneosColour methodsFor: 'accessing'!

color
   ^color!

color: aColor 
   color := aColor! !

!ChameneosColour methodsFor: 'testing'!

isYellow
   ^self == self class yellow!

isBlue
   ^self == self class blue!

hasSameColorAs: aChameneos 
   ^self color == aChameneos color!

isRed
   ^self == self class red! !

!ChameneosColour methodsFor: 'as yet unclassified'!

complementaryColourFor: aChameneosColour 
   "determine the complementary colour defined as..."

   self == aChameneosColour ifTrue: [^self].
   self isBlue 
      ifTrue: 
         [aChameneosColour isRed 
            ifTrue: [^self class yellow]
            ifFalse: [^self class red]].
   self isRed 
      ifTrue: 
         [aChameneosColour isBlue 
            ifTrue: [^self class yellow]
            ifFalse: [^self class blue]].
   aChameneosColour isBlue 
      ifTrue: [^self class red]
      ifFalse: [^self class blue]! !

!ChameneosColour methodsFor: 'printing'!

printOn: aStream 
   aStream nextPutAll: self color! !


!Core.BenchmarksGame class methodsFor: 'initialize-release'!

program
   | n |
   n := CEnvironment commandLine last asNumber.
   Mall runBenchMark: n on: Stdout.
   ^''! !


!Pair class methodsFor: 'instance creation'!

new
   "Answer a newly created and initialized instance."
   ^super new initialize.!

with: me 
   "Answer a newly created and initialized instance."
self halt.
   ^super new initialize me: me! !


!Pair methodsFor: 'initialize-release'!

wait
   sema wait!

signal
   sema signal!

initialize
   "Initialize a newly created instance. This method must answer the receiver."

   partner := nil.
   me := nil.
   sema := Semaphore new.
   ^self!

release
partner:=nil.! !

!Pair methodsFor: 'accessing'!

partner: anObject
   partner := anObject!

partner
   ^partner!

me: anObject
   me := anObject!

me
   ^me! !


#{ChameneosColour} initialize!

#{Mall} initialize!


!Core.Stream methodsFor: 'benchmarks game'!

nl
   self nextPut: Character lf! !
