"* The Computer Language Benchmarks Game
    https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
    contributed by Paolo Bonzini
    modified by Andres Valloud *"!

Object subclass: #BenchmarksGame
   instanceVariableNames: ʼʼ
   classVariableNames: ʼʼ
   poolDictionaries: ʼʼ
   category: ʼʼ!

Object subclass: #PiDigitSpigot
   instanceVariableNames: ʼnumer accum denom kʼ
   classVariableNames: ʼʼ
   poolDictionaries: ʼʼ
   category: ʼbenchmarks gameʼ!


!BenchmarksGame class methodsFor: ʼprivateʼ!

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

!BenchmarksGame class methodsFor: ʼinitialize-releaseʼ!

do: n
   self pidigitsTo: n width: 10 to: Stdio stdout! !


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

!StdioStream methodsFor: ʼbenchmarks gameʼ!

tab
   self nextPut: Character tab!

nl
   self nextPut: Character lf! !

