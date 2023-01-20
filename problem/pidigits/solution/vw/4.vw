"* The Computer Language Benchmarks Game
    https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
    contributed by Paolo Bonzini
    modified by Andres Valloud *"!

Smalltalk.Core defineClass: #BenchmarksGame
        superclass: #{Core.Object}
        indexedType: #none
        private: false
        instanceVariableNames: ʼʼ
        classInstanceVariableNames: ʼʼ
        imports: ʼʼ
        category: ʼʼ!

Smalltalk defineClass: #PiDigitSpigot
        superclass: #{Core.Stream}
        indexedType: #none
        private: false
        instanceVariableNames: ʼnumer accum denom k ʼ
        classInstanceVariableNames: ʼʼ
        imports: ʼʼ
        category: ʼShootoutʼ!

!Core.BenchmarksGame class methodsFor: ʼprivateʼ!

pidigitsTo: v width: width to: output
   | n i pidigits |
   n := v.
   i := 0.
   pidigits := PiDigitSpigot new.
   [n > 0] whileTrue:
      [n < width
         ifTrue:
            [n timesRepeat: [output nextPut: (Character digitValue: pidigits nex
t)].
            n to: width do: [:each | output space].
            i := i + n]
         ifFalse:
            [width timesRepeat: [output nextPut: (Character digitValue: pidigits
 next)].
            i := i + width].

      output tab; nextPut: $:; print: i; nl.

      n := n - width]! !

!Core.BenchmarksGame class methodsFor: ʼinitialize-releaseʼ!

do: n
   self pidigitsTo: n width: 10 to: Stdout.
   ^ʼʼ! !


!PiDigitSpigot class methodsFor: ʼinstance creationʼ!

new
   ^super basicNew initialize! !


!PiDigitSpigot methodsFor: ʼprivateʼ!

initialize
    numer := denom := 1.
    k := accum := 0.!

extract
    | tmp |
    numer > accum ifTrue: [^nil].
    tmp := numer + numer + numer + accum.
    tmp \\ denom >= (denom - numer) ifTrue: [^nil].
    ^tmp // denom!

eliminate: digit
    accum := accum - (denom * digit).
    accum := accum * 10.
    numer := numer * 10!

step
    | y2 |
    k := k + 1.
    y2 := k * 2 + 1.
    accum := (numer + numer + accum) * y2.
    numer := numer * k.
    denom := denom * y2.! !

!PiDigitSpigot methodsFor: ʼstreamʼ!

atEnd
    ^false!

next
    | digit |
    [ self step. (digit := self extract) isNil ] whileTrue.
    self eliminate: digit.
    ^digit! !

!Core.Stream methodsFor: ʼbenchmarks gameʼ!

nl
   self nextPut: Character lf! !

