"* The Computer Language Benchmarks Game
    http://benchmarksgame.alioth.debian.org/
    contributed by Eliot Miranda
    modified by Isaac Gouy *"!


Object subclass: #BenchmarksGame
   instanceVariableNames: ʼʼ
   classVariableNames: ʼʼ
   poolDictionaries: ʼʼ
   category: ʼʼ!


!BenchmarksGame class methodsFor: ʼprivateʼ!

matchPatterns
   ^#(   ʼagggtaaa|tttaccctʼ
         ʼ[cgt]gggtaaa|tttaccc[acg]ʼ
         ʼa[act]ggtaaa|tttacc[agt]tʼ
         ʼag[act]gtaaa|tttac[agt]ctʼ
         ʼagg[act]taaa|ttta[agt]cctʼ
         ʼaggg[acg]aaa|ttt[cgt]ccctʼ
         ʼagggt[cgt]aa|tt[acg]accctʼ
         ʼagggta[cgt]a|t[acg]taccctʼ
         ʼagggtaa[cgt]|[acg]ttaccctʼ
   )! !

!BenchmarksGame class methodsFor: ʼprivateʼ!

substitutionPatterns
   ^#(   #(ʼtHa[Nt]ʼ ʼ<4>ʼ)
         #(ʼaND|caN|Ha[DS]|WaSʼ ʼ<3>ʼ)
         #(ʼa[NSt]|BYʼ ʼ<2>ʼ)
         #(ʼ<[^>]*>ʼ ʼ|ʼ)
         #(ʼ\\|[^|][^|]*\\|ʼ ʼ-ʼ))! !


!BenchmarksGame class methodsFor: ʼprivateʼ!

regexDNA: sequence to: output
   | s size1 size2 translation |
   size1 := sequence size.

   "* remove FASTA sequence descriptions and new-lines *"
   s := sequence copyWithRegex: ʼ>[^\r]*\r|\rʼ matchesReplacedWith: ʼʼ.
   size2 := s size.

   "* regex match *"
   self matchPatterns do: [:each|
      output
         nextPutAll: each; space;
         print: (s occurrencesOfRegex: each); nl
      ].

   "* regex substitution *"
   translation := Dictionary new.
   self substitutionPatterns do: [:each|
      translation at: each first put: each last].

   s := s copyWithRegex: ʼ[ʼ,
         (translation keys asArray fold: [:a :b| a, b]), ʼ]ʼ
      matchesTranslatedUsing: [:l| translation at: l].

   output
      nl;
      print: size1; nl;
      print: size2; nl;
      print: s size; nl! !


!BenchmarksGame class methodsFor: ʼinitialize-releaseʼ!

do: n
   self regexDNA: Stdio stdin contents asString to: Stdio stdout! !


!Stream methodsFor: ʼbenchmarks gameʼ!

nl
   self nextPut: Character lf! !


