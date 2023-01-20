<?php
#
# The Computer Language Benchmarks Game
# https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
#
# reverse complement in PHP
# contributed by Danny Sauer
# modified by Craig Russell
#


ob_implicit_flush(1);
ob_start(NULL, 4096);

$seq = ʼʼ;

# read in the file, a line at a time
do {
    $str = trim(fgets(STDIN));
    if( $str && $str[0] == ʼ>ʼ ){
        # if weʼre on a comment line, print the previous seq and move on
                if($seq != ʼʼ){
                   echo wordwrap( strrev( strtr(strtoupper($seq),
                          ʼCGATMKRYVBHDʼ, ʼGCTAKMYRBVDHʼ) ), 60, "\n", true ), "
\n";
                   $seq = ʼʼ;
                }
        echo $str, "\n";
    }else{
        # otherwise, just append to the sequence
        $seq .= $str;
    }
} while( !feof(STDIN) );
if($seq != ʼʼ){
        echo wordwrap( strrev( strtr(strtoupper($seq),
           ʼCGATMKRYVBHDʼ, ʼGCTAKMYRBVDHʼ) ), 60, "\n", true ), "\n";
}

