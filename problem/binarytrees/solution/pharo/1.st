"* The Computer Language Benchmarks Game
    https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
    contributed by Isaac Gouy
    modified by Eliot Miranda
*"!

Object subclass: #BenchmarksGame
   instanceVariableNames: ʼʼ
   classVariableNames: ʼʼ
   poolDictionaries: ʼʼ
   category: ʼʼ!

Object subclass: #TreeNode
   instanceVariableNames: ʼleft rightʼ
   classVariableNames: ʼʼ
   poolDictionaries: ʼʼ
   category: ʼbenchmarks gameʼ!


!BenchmarksGame class methodsFor: ʼinitialize-releaseʼ!

do: n
   | minDepth maxDepth stretchDepth check longLivedTree iterations |
   minDepth := 4.
   maxDepth := minDepth + 2 max: n.
   stretchDepth := maxDepth + 1.

   check := (TreeNode bottomUpTree: stretchDepth) itemCheck.
   Stdio stdout
      nextPutAll: ʼstretch tree of depth ʼ; print: stretchDepth; tab;
      nextPutAll: ʼ check: ʼ; print: check; nl.

   longLivedTree := TreeNode bottomUpTree: maxDepth.
   minDepth to: maxDepth by: 2 do: [:depth|
      iterations := 1 bitShift: maxDepth - depth + minDepth.

      check := 0.
      1 to: iterations do: [:i|
         check := check + (TreeNode bottomUpTree: depth) itemCheck
         ].
      Stdio stdout
         print: iterations; tab;
         nextPutAll: ʼ trees of depth ʼ; print: depth; tab;
         nextPutAll: ʼ check: ʼ; print: check; nl
      ].

   Stdio stdout
      nextPutAll: ʼlong lived tree of depth ʼ; print: maxDepth; tab;
      nextPutAll: ʼ check: ʼ; print: longLivedTree itemCheck; nl! !

!TreeNode class methodsFor: ʼinstance creationʼ!

bottomUpTree: anInteger
   ^(anInteger > 0)
      ifTrue: [
         self
            left: (self bottomUpTree: anInteger - 1)
            right: (self bottomUpTree: anInteger - 1)
         ]
      ifFalse: [self left: nil right: nil]!

left: leftChild right: rightChild
   ^(super new) left: leftChild right: rightChild! !


!TreeNode methodsFor: ʼaccessingʼ!

itemCheck
   ^left isNil
      ifTrue: [1] ifFalse: [1 + left itemCheck + right itemCheck]! !

!TreeNode methodsFor: ʼinitialize-releaseʼ!

left: leftChild right: rightChild
   left := leftChild.
   right := rightChild! !


!StdioStream methodsFor: ʼbenchmarks gameʼ!

tab
   self nextPut: Character tab!

nl
   self nextPut: Character lf! !

