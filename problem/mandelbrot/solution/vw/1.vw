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

mandelbrot: extent to: output
   | limit2 m bits zr zi cr ci i tr stepr stepi |
   limit2 := 4.0d0.
   m := 50.

   stepr := 2.0d0 / extent.
   stepi := 2.0d0 / extent.

   0 to: extent - 1 do: [ :y |
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
            output nextPut: bits.
            bits := 0.
         ]
      ].
      (extent bitAnd: 7) == 0 ifFalse: [
         bits := bits bitShift: 8 - (extent bitAnd: 7).
         output nextPut: bits.
      ]
   ]! !

!Core.BenchmarksGame class methodsFor: ʼinitialize-releaseʼ!

do: n
   Stdout
      nextPutAll: ʼP4ʼ; nl;
      print: n; space; print: n; nl;
      binary.

   self mandelbrot: n to: Stdout.
   ^ʼʼ! !

!Core.Stream methodsFor: ʼbenchmarks gameʼ!

nl
   self nextPut: Character lf! !

