"* The Computer Language Benchmarks Game
    https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
    contributed by Isaac Gouy
    modified by Eliot Miranda *"!

Smalltalk.Core defineClass: #BenchmarksGame
        superclass: #{Core.Object}
        indexedType: #none
        private: false
        instanceVariableNames: ʼʼ
        classInstanceVariableNames: ʼʼ
        imports: ʼʼ
        category: ʼʼ!

Smalltalk defineClass: #RandomNumber
        superclass: #{Core.Object}
        indexedType: #none
        private: false
        instanceVariableNames: ʼseed scale ʼ
        classInstanceVariableNames: ʼʼ
        imports: ʼʼ
        category: ʼbenchmarks gameʼ!

Smalltalk defineClass: #RepeatStream
        superclass: #{Core.ReadStream}
        indexedType: #none
        private: false
        instanceVariableNames: ʼrepeatPtr repeatLimit ʼ
        classInstanceVariableNames: ʼʼ
        imports: ʼʼ
        category: ʼbenchmarks gameʼ!

Smalltalk defineClass: #RandomStream
        superclass: #{Smalltalk.RepeatStream}
        indexedType: #none
        private: false
        instanceVariableNames: ʼrandom percentages ʼ
        classInstanceVariableNames: ʼʼ
        imports: ʼʼ
        category: ʼbenchmarks gameʼ!

Smalltalk.RandomNumber defineSharedVariable: #FModulus
        private: false
        constant: false
        category: ʼAs yet unclassifiedʼ
        initializer: nil!


Smalltalk.RandomNumber defineSharedVariable: #Increment
        private: false
        constant: false
        category: ʼAs yet unclassifiedʼ
        initializer: nil!


Smalltalk.RandomNumber defineSharedVariable: #Modulus
        private: false
        constant: false
        category: ʼAs yet unclassifiedʼ
        initializer: nil!


Smalltalk.RandomNumber defineSharedVariable: #Multiplier
        private: false
        constant: false
        category: ʼAs yet unclassifiedʼ
        initializer: nil!

!Core.BenchmarksGame class methodsFor: ʼprivateʼ!

fasta: n to: out
   | r lineLength |
   lineLength := 60.
   self
      writeFasta: ʼONE Homo sapiens aluʼ
      from:
         ( RepeatStream
            to: n*2
            on:ʼGGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGʼ,
               ʼGAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGAʼ,
               ʼCCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATʼ,
               ʼACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCAʼ,
               ʼGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGGʼ,
               ʼAGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCCʼ,
               ʼAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAAʼ )
      to: out
      lineLength: lineLength.

   r := RandomNumber to: 1. "Shared random sequence"

   self
      writeFasta: ʼTWO IUB ambiguity codesʼ
      from:
         (( RandomStream
            to: n*3
            on: #(   #($a 0.27d0)
                  #($c 0.12d0)
                  #($g 0.12d0)
                  #($t 0.27d0)

                  #($B 0.02d0)
                  #($D 0.02d0)
                  #($H 0.02d0)
                  #($K 0.02d0)
                  #($M 0.02d0)
                  #($N 0.02d0)
                  #($R 0.02d0)
                  #($S 0.02d0)
                  #($V 0.02d0)
                  #($W 0.02d0)
                  #($Y 0.02d0)))
         random: r;
         yourself)
      to: out
      lineLength: lineLength.

   self
      writeFasta: ʼTHREE Homo sapiens frequencyʼ
      from:
         (( RandomStream
            to: n*5
            on: #(   #($a 0.3029549426680d0)
                  #($c 0.1979883004921d0)
                  #($g 0.1975473066391d0)
                  #($t 0.3015094502008d0)))
            random: r;
            yourself)
      to: out
      lineLength: lineLength.

   out flush.!

writeFasta: aString from: inStream to: outStream lineLength: lineLength
   | i |
   outStream nextPut: $>; nextPutAll: aString; nl.
   i := 0.
   [inStream atEnd] whileFalse:
      [i == lineLength ifTrue: [outStream nl. i := 0].
      outStream nextPut: inStream next.
      i := i + 1].
   outStream nl! !

!Core.BenchmarksGame class methodsFor: ʼinitialize-releaseʼ!

do: n
   | output |
   output := ExternalWriteStream on:
      (ExternalConnection ioAccessor: (UnixDiskFileAccessor new handle: 1)).
   self fasta: n to: output.
   ^ʼʼ! !


!RandomNumber class methodsFor: ʼclass initializationʼ!

initialize
   FModulus := 139968.0d0.
   Increment := 29573.
   Modulus := 139968.
   Multiplier := 3877.! !

!RandomNumber class methodsFor: ʼinitialize-releaseʼ!

to: anInteger
   ^self basicNew to: anInteger! !


!RandomNumber methodsFor: ʼprivateʼ!

to: anInteger
   seed := 42.
   scale := anInteger! !

!RandomNumber methodsFor: ʼaccessingʼ!

next
   seed := (seed * Multiplier + Increment) \\ Modulus.
   ^(seed * scale) / FModulus! !


!RepeatStream class methodsFor: ʼinstance creationʼ!

to: anInteger on: aCollection
   ^(super on: aCollection) to: anInteger! !


!RepeatStream methodsFor: ʼaccessingʼ!

next
   position >= readLimit ifTrue: [ self position: 0 ].
   repeatPtr := repeatPtr + 1.
   ^collection at: (position := position + 1)! !

!RepeatStream methodsFor: ʼtestingʼ!

atEnd
   ^repeatPtr >= repeatLimit! !

!RepeatStream methodsFor: ʼinitialize-releaseʼ!

to: anInteger
   repeatPtr := 0.
   repeatLimit := anInteger! !


!RandomStream methodsFor: ʼaccessingʼ!

random: aRandomNumber
"* Share the random number generator so we can get the expected results. *"
   random := aRandomNumber!

next
   | r |
   r := random next.
   repeatPtr := repeatPtr + 1.
   1 to: percentages size do: [:i|
      (r < (percentages at: i)) ifTrue: [^collection at: i]]! !

!RandomStream methodsFor: ʼinitialize-releaseʼ!

on: aCollection
   | size cp |
   repeatPtr := 0.
   random := RandomNumber to: 1.0d0.
   size := aCollection size.
   percentages := Array new: size.
   collection := Array new: size.
   cp := 0.0d0.
   1 to: size do: [:i|
      collection at: i put: (aCollection at: i) first.
      percentages at: i put: (cp := cp + (aCollection at: i) last).
   ]! !


!Core.Stream methodsFor: ʼbenchmarks gameʼ!

nl
   self nextPut: Character lf! !


RandomNumber initialize!

