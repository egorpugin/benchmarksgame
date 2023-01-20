<?php
/* The Computer Language Benchmarks Game
   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

   regex-dna program contributed by Danny Sauer
   modified by Josh Goldfoot
   modified by Sergey Khripunov
   modified by Craig Russell
   converted from regex-dna program
*/

$tok = ftok(__FILE__, chr(time() & 255));
$queue = msg_get_queue($tok);

$variants = array(
    ʼagggtaaa|tttaccctʼ,
    ʼ[cgt]gggtaaa|tttaccc[acg]ʼ,
    ʼa[act]ggtaaa|tttacc[agt]tʼ,
    ʼag[act]gtaaa|tttac[agt]ctʼ,
    ʼagg[act]taaa|ttta[agt]cctʼ,
    ʼaggg[acg]aaa|ttt[cgt]ccctʼ,
    ʼagggt[cgt]aa|tt[acg]accctʼ,
    ʼagggta[cgt]a|t[acg]taccctʼ,
    ʼagggtaa[cgt]|[acg]ttaccctʼ,
);

// IUB replacement parallel arrays
$IUB = array();                 $IUBnew = array();
$IUB[]=ʼ/tHa[Nt]/Sʼ;            $IUBnew[]=ʼ<4>ʼ;
$IUB[]=ʼ/aND|caN|Ha[DS]|WaS/Sʼ; $IUBnew[]=ʼ<3>ʼ;
$IUB[]=ʼ/a[NSt]|BY/Sʼ;          $IUBnew[]=ʼ<2>ʼ;
$IUB[]=ʼ/<[^>]*>/Sʼ;            $IUBnew[]=ʼ|ʼ;
$IUB[]=ʼ/\\|[^|][^|]*\\|/Sʼ;    $IUBnew[]=ʼ-ʼ;


// read in file
$contents = file_get_contents(ʼphp://stdinʼ);
$initialLength = strlen($contents);

// remove things
$contents = preg_replace(ʼ/^>.*$|\n/mSʼ, ʼʼ, $contents);
$codeLength = strlen($contents);

// do regexp counts
$messages = array_flip($variants);
$workers = $results = array();
foreach ($variants as $key => $regex){
   if($key == 0 || $key == 2 || $key == 4 || $key == 6) {
      $pid = pcntl_fork();
      if($pid) $workers[] = $pid;
   }
   if($pid && $key > 7) {
      $messages[$regex] =
         preg_match_all(ʼ/ʼ . $regex . ʼ/iSʼ, $contents, $discard);
   }
   else if(!$pid) {
      $results[] = $regex . ʼ,ʼ .
         preg_match_all(ʼ/ʼ . $regex . ʼ/iSʼ, $contents, $discard);
      if($key == 1 || $key == 3 || $key == 5 || $key == 7) {
         msg_send($queue, 2, implode(ʼ;ʼ, $results), false, false, $errno);
         exit;
          }
   }
}

// receive and output the counts
pcntl_wait($status);
foreach($workers as $worker) {
   msg_receive($queue, 2, $msgtype, 4096, $message, false);
   $message = explode(ʼ;ʼ, $message, 3);
   foreach($message as $line) {
      $tmp = explode(ʼ,ʼ, $line, 2);
      $messages[$tmp[0]] = $tmp[1];
   }
}
foreach($messages as $regex => $count) {
   echo $regex, ʼ ʼ, $count, "\n";
}

// do replacements
$contents = preg_replace($IUB, $IUBnew, $contents);

echo "\n",
      $initialLength, "\n",
      $codeLength, "\n",
      strlen($contents), "\n";

