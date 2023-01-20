<?php
/* The Computer Language Benchmarks Game
   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

   contributed by Wing-Chung Leung
   modified by Isaac Gouy
*/

# error_reporting(E_STRICT);

define (ʼIMʼ, 139968);
define (ʼIAʼ, 3877);
define (ʼICʼ, 29573);

function gen_random($max) {
   static $last = 42;
   return $max * ($last = ($last * IA + IC) % IM) / IM;
}

/* Weighted selection from alphabet */

function makeCumulative(&$genelist) {
   $count = count($genelist);
   for ($i=1; $i < $count; $i++) {
      $genelist[$i][1] += $genelist[$i-1][1];
   }
}


function selectRandom(&$a) {
   $r = gen_random(1);
   $hi = sizeof($a);

   for ($i = 0; $i < $hi; $i++) {
      if ($r < $a[$i][1]) return $a[$i][0];
   }
   return $a[$hi-1][0];
}

/* Generate and write FASTA format */

define (ʼLINE_LENGTHʼ, 60);


function makeRandomFasta($id, $desc, &$genelist, $n) {
   print(">$id $desc\n");

   for ($todo = $n; $todo > 0; $todo -= LINE_LENGTH) {
      $pick = ʼʼ;
      $m = $todo < LINE_LENGTH ? $todo : LINE_LENGTH;
      for ($i=0; $i < $m; $i++) $pick .= selectRandom($genelist);
      $pick .= "\n";
      print( $pick );
   }
}


function makeRepeatFasta($id, $desc, $s, $n) {
   echo ">$id $desc\n";
   $i = 0; $sLength = strlen($s); $lineLength = LINE_LENGTH;
   while ($n > 0) {
      if ($n < $lineLength) $lineLength = $n;
      if ($i + $lineLength < $sLength){
         print(substr($s,$i,$lineLength)); print("\n");
         $i += $lineLength;
      } else {
         print(substr($s,$i));
         $i = $lineLength - ($sLength - $i);
         print(substr($s,0,$i)); print("\n");
      }
      $n -= $lineLength;
   }
}


/* Main -- define alphabets, make 3 fragments */

$iub=array(
   array(ʼaʼ, 0.27),
   array(ʼcʼ, 0.12),
   array(ʼgʼ, 0.12),
   array(ʼtʼ, 0.27),

   array(ʼBʼ, 0.02),
   array(ʼDʼ, 0.02),
   array(ʼHʼ, 0.02),
   array(ʼKʼ, 0.02),
   array(ʼMʼ, 0.02),
   array(ʼNʼ, 0.02),
   array(ʼRʼ, 0.02),
   array(ʼSʼ, 0.02),
   array(ʼVʼ, 0.02),
   array(ʼWʼ, 0.02),
   array(ʼYʼ, 0.02)
);

$homosapiens = array(
   array(ʼaʼ, 0.3029549426680),
   array(ʼcʼ, 0.1979883004921),
   array(ʼgʼ, 0.1975473066391),
   array(ʼtʼ, 0.3015094502008)
);

$alu =
   ʼGGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGʼ .
   ʼGAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGAʼ .
   ʼCCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATʼ .
   ʼACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCAʼ .
   ʼGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGGʼ .
   ʼAGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCCʼ .
   ʼAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAAʼ;

$n = 1000;

if ($_SERVER[ʼargcʼ] > 1) $n = $_SERVER[ʼargvʼ][1];

   makeCumulative($iub);
   makeCumulative($homosapiens);

   makeRepeatFasta(ʼONEʼ, ʼHomo sapiens aluʼ, $alu, $n*2);
   makeRandomFasta(ʼTWOʼ, ʼIUB ambiguity codesʼ, $iub, $n*3);
   makeRandomFasta(ʼTHREEʼ, ʼHomo sapiens frequencyʼ, $homosapiens, $n*5);
?>

