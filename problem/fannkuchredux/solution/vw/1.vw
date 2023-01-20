"* The Computer Language Benchmarks Game
   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
   contributed by Paolo Bonzini
   modified by Isaac Gouy *"!

Smalltalk.Core defineClass: #BenchmarksGame
        superclass: #{Core.Object}
        indexedType: #none
        private: false
        instanceVariableNames: ʼʼ
        classInstanceVariableNames: ʼʼ
        imports: ʼʼ
        category: ʼʼ!

Smalltalk defineClass: #PermGeneratorRedux
        superclass: #{Core.Object}
        indexedType: #none
        private: false
        instanceVariableNames: ʼtimesRotated perm atEnd ʼ
        classInstanceVariableNames: ʼʼ
        imports: ʼʼ
        category: ʼbenchmarks gameʼ!

!PermGeneratorRedux class methodsFor: ʼinstance creationʼ!

new: size
   ^self new
      initialize: size;
      yourself! !


!PermGeneratorRedux methodsFor: ʼaccessingʼ!

atEnd
   ^atEnd!

maxPfannkuchenTo: output
   | max permutation checksum permCount flipsCount |
   max := 0.
   permCount := 0.
   checksum := 0.
   [self atEnd] whileFalse:
      [permutation := self next.
      permCount := permCount + 1.
      (permCount = 1048576) ifTrue: [permCount := 0].
      flipsCount := permutation pfannkuchen.
      checksum := permCount odd
         ifTrue: [checksum+flipsCount]
         ifFalse: [checksum-flipsCount].
      max := max max: flipsCount].
   output print: checksum; nl.
   ^max!

next
   | result |
   result := perm copy.
   self makeNext.
   ^result! !

!PermGeneratorRedux methodsFor: ʼinitialize-releaseʼ!

initialize: size
   perm := (1 to: size) asArray.
   timesRotated := Array new: size withAll: 0.
   atEnd := false.!

makeNext
   | temp remainder |
   "* Generate the next permutation. *"
   2 to: perm size do: [ :r |
      "* Rotate the first r items to the left. *"
      temp := perm at: 1.
      1 to: r - 1 do: [ :i | perm at: i put: (perm at: i + 1) ].
      perm at: r put: temp.

      remainder := timesRotated at: r
                                put: ((timesRotated at: r) + 1) \\ r.
      remainder = 0 ifFalse: [ ^self ].

      "* After r rotations, the first r items
         are in their original positions.
      Go on rotating the first r+1 items. *"
   ].

   "* We are past the final permutation. *"
   atEnd := true! !


!Core.BenchmarksGame class methodsFor: ʼprivateʼ!

fannkuchRedux: n to: output
   ^(PermGeneratorRedux new: n) maxPfannkuchenTo: output! !

!Core.BenchmarksGame class methodsFor: ʼinitialize-releaseʼ!

do: n
   | f |
   f := self fannkuchRedux: n to: Stdout.
   Stdout
      nextPutAll: ʼPfannkuchen(ʼ, n printString, ʼ) = ʼ;
      print: f; nl.

   ^ʼʼ! !


!Core.Array methodsFor: ʼbenchmarks gameʼ!

pfannkuchen
   | first complement a b k |
   k := 0.
   [ (first := self at: 1) == 1 ] whileFalse: [
      k := k + 1.
      complement := first + 1.
      1 to: first // 2 do: [ :i |
         a := self at: i.
         b := self at: complement - i.
         self at: i put: b.
         self at: complement - i put: a.
      ]
   ].
   ^k! !


!Core.Stream methodsFor: ʼbenchmarks gameʼ!

nl
   self nextPut: Character lf! !

