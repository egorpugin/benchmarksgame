"* The Computer Language Benchmarks Game
    https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
    contributed by Isaac Gouy
    modified by Eliot Miranda
    reworked for multicore by Isaac Gouy
*"!

Smalltalk.Core defineClass: #BenchmarksGame
        superclass: #{Core.Object}
        indexedType: #none
        private: false
        instanceVariableNames: ʼʼ
        classInstanceVariableNames: ʼʼ
        imports: ʼʼ
        category: ʼʼ!

Smalltalk defineClass: #TreeNode
        superclass: #{Core.Object}
        indexedType: #none
        private: false
        instanceVariableNames: ʼleft right ʼ
        classInstanceVariableNames: ʼʼ
        imports: ʼʼ
        category: ʼbenchmarks gameʼ!

!Core.BenchmarksGame class methodsFor: ʼbenchmarks gameʼ!

do: n
   | checks depths iterations longLivedTree maxDepth minDepth
     nprocs stretchDepth |

   minDepth := 4.
   maxDepth := minDepth + 2 max: n.
   stretchDepth := maxDepth + 1.

   checks := (TreeNode bottomUpTree: stretchDepth) itemCheck.
   Stdout
      nextPutAll: ʼstretch tree of depth ʼ; print: stretchDepth; tab;
      nextPutAll: ʼ check: ʼ; print: checks; nl.

   longLivedTree := TreeNode bottomUpTree: maxDepth.

   depths := minDepth to: maxDepth by: 2.
   iterations := depths collect: [:each| 1 bitShift: maxDepth - each + minDepth]
.

      "for larger workloads split the work across multiple processes"
   nprocs := (ExternalProcess shOne: ʼnprocʼ) asNumber.
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
         nextPutAll: ʼ trees of depth ʼ; print: (depths at: i); tab;
         nextPutAll: ʼ check: ʼ; print: (checks at: i); nl
   ].

   Stdout
      nextPutAll: ʼlong lived tree of depth ʼ; print: maxDepth; tab;
      nextPutAll: ʼ check: ʼ; print: longLivedTree itemCheck; nl.
   ^ʼʼ!


checkBlock
   ^[:d :m|
      | check |
      check := 0.
      1 to: m do: [:i| check := check + (TreeNode bottomUpTree: d) itemCheck].
      check
   ]! !


!TreeNode class methodsFor: ʼinstance creationʼ!

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


!TreeNode methodsFor: ʼbenchmarks gameʼ!

itemCheck
   ^left isNil
      ifTrue: [1] ifFalse: [1 + left itemCheck + right itemCheck]! !

!TreeNode methodsFor: ʼinstance creationʼ!

left: leftChild right: rightChild
   left := leftChild.
   right := rightChild! !


!Core.Stream methodsFor: ʼbenchmarks gameʼ!

nl
   self nextPut: Character lf! !


