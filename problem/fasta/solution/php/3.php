<?php
/* The Computer Language Benchmarks Game
   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

   contributed by Wing-Chung Leung
   modified by Isaac Gouy
   modified by anon
 */

ob_implicit_flush(1);
ob_start(NULL, 4096);

$last = 42.0;
function gen_random(&$last, &$randoms, $max = 1.0, $ia = 3877.0, $ic = 29573.0,
$im = 139968.0) {
   foreach($randoms as &$r) {
      $r = $max * ($last = ($last * $ia + $ic) % $im) / $im;
   }
}

/* Weighted selection from alphabet */

function makeCumulative(&$genelist) {
   $cumul = 0.0;
   foreach($genelist as $k=>&$v) {
      $cumul = $v += $cumul;
   }
}


/* Generate and write FASTA format */

function makeRandomFasta(&$genelist, $n) {
   $width = 60;
   $lines = (int) ($n / $width);
   $pick = str_repeat(ʼ?ʼ, $width)."\n";
   $randoms = array_fill(0, $width, 0.0);
   global $last;

   // full lines
   for ($i = 0; $i < $lines; ++$i) {
      gen_random($last, $randoms);
      $j = 0;
      foreach ($randoms as $r) {
         foreach($genelist as $k=>$v) {
            if ($r < $v) {
               break;
            }
         }
         $pick[$j++] = $k;
      }
      echo $pick;
   }

   // last, partial line
   $w = $n % $width;
   if ($w !== 0) {
      $randoms = array_fill(0, $w, 0.0);
      gen_random($last, $randoms);
      $j = 0;
      foreach ($randoms as $r) {
         foreach($genelist as $k=>$v) {
            if ($r < $v) {
               break;
            }
         }
         $pick[$j++] = $k;
      }
      $pick[$w] = "\n";
      echo substr($pick, 0, $w+1);
   }

}


function makeRepeatFasta($s, $n) {
   $i = 0; $sLength = strlen($s); $lineLength = 60;
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
   ʼaʼ => 0.27,
   ʼcʼ => 0.12,
   ʼgʼ => 0.12,
   ʼtʼ => 0.27,

   ʼBʼ => 0.02,
   ʼDʼ => 0.02,
   ʼHʼ => 0.02,
   ʼKʼ => 0.02,
   ʼMʼ => 0.02,
   ʼNʼ => 0.02,
   ʼRʼ => 0.02,
   ʼSʼ => 0.02,
   ʼVʼ => 0.02,
   ʼWʼ => 0.02,
   ʼYʼ => 0.02
);

$homosapiens = array(
   ʼaʼ => 0.3029549426680,
   ʼcʼ => 0.1979883004921,
   ʼgʼ => 0.1975473066391,
   ʼtʼ => 0.3015094502008
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

echo ">ONE Homo sapiens alu\n";
makeRepeatFasta($alu, $n*2);

echo ">TWO IUB ambiguity codes\n";
makeRandomFasta($iub, $n*3);

echo ">THREE Homo sapiens frequency\n";
makeRandomFasta($homosapiens, $n*5);


