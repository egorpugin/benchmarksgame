"* The Computer Language Benchmarks Game
    https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
    contributed by Paolo Bonzini
    reworked by Isaac Gouy *"!

Smalltalk.Core defineClass: #BenchmarksGame
    superclass: #{Core.Object}
    indexedType: #none
    private: false
    instanceVariableNames: ʼʼ
    classInstanceVariableNames: ʼʼ
    imports: ʼʼ
    category: ʼʼ!


!Core.BenchmarksGame class methodsFor: ʼprivateʼ!

spectralnorm: n
   | u v vBv vv |
   u := Array new: n withAll: 1.0d0.
   10 timesRepeat:
      [v := u multiplyAtAv.
       u := v multiplyAtAv].
   vBv := 0.0d0.
   vv := 0.0d0.
   1 to: n do:
      [:i |
       vBv := vBv + ((u at: i) * (v at: i)).
       vv := vv + ((v at: i) * (v at: i))].
   ^(vBv / vv) sqrt! !

!Core.BenchmarksGame class methodsFor: ʼinitialize-releaseʼ!

do: n
   Stdout
      print: (self spectralnorm: n) digits: 9;
      nl.

   ^ʼʼ! !


!Core.SmallInteger methodsFor: ʼbenchmarks gameʼ!

matrixA: anInteger
   ^1.0d0 / ((self + anInteger - 2) * (self + anInteger - 1) /2  + self)! !


!Core.Array methodsFor: ʼbenchmarks gameʼ!

multiplyAtv
   | n atv sum |
   n := self size.
   atv := Array new: n.
   1 to: n do: [:i|
      sum := 0.0d0.
      1 to: n do: [:j|
         sum := sum + ((j matrixA: i) * (self at: j)) ].
      atv at: i put: sum].
   ^atv!

multiplyAtAv
   ^(self multiplyAv) multiplyAtv!

multiplyAv
   | n av sum |
   n := self size.
   av := Array new: n.
   1 to: n do: [:i|
      sum := 0.0d0.
      1 to: n do: [:j|
         sum := sum + ((i matrixA: j) * (self at: j)) ].
      av at: i put: sum].
   ^av! !


!Core.Stream methodsFor: ʼbenchmarks gameʼ!

nl
   self nextPut: Character lf!

print: number digits: decimalPlaces
   self nextPutAll:
      ((number asFixedPoint: decimalPlaces) printString copyWithout: $s)! !


